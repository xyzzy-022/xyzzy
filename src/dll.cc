#include "stdafx.h"
#include "ed.h"
#include "except.h"

ldll_module *
make_dll_module ()
{
  ldll_module *p = ldata <ldll_module, Tdll_module>::lalloc ();
  p->name = Qnil;
  p->handle = 0;
  p->loaded = 0;
  return p;
}

ldll_function *
make_dll_function ()
{
  ldll_function *p = ldata <ldll_function, Tdll_function>::lalloc ();
  p->module = Qnil;
  p->name = Qnil;
  p->proc = 0;
  p->arg_types = 0;
  p->nargs = 0;
  p->return_type = 0;
  p->arg_size = 0;
  return p;
}

lc_callable *
make_c_callable ()
{
  lc_callable *p = ldata <lc_callable, Tc_callable>::lalloc ();
  p->function = Qnil;
  p->arg_types = 0;
  p->nargs = 0;
  p->return_type = 0;
  p->arg_size = 0;
  return p;
}

static lisp
find_module (lisp name)
{
  for (lisp p = xsymbol_value (Vdll_module_list); consp (p); p = xcdr (p))
    {
      lisp x = xcar (p);
      if (dll_module_p (x)
          && xdll_module_handle (x)
          && Fstring_equalp (xdll_module_name (x), name, Qnil) != Qnil)
        return x;
    }
  return Qnil;
}

lisp
Fsi_load_dll_module (lisp lname)
{
  check_string (lname);

  lisp dll = find_module (lname);
  if (dll != Qnil)
    return dll;

  char *name = (char *)alloca (xstring_length (lname) * 2 + 1);
  w2s (name, lname);

  dll = make_dll_module ();
  lisp list = xcons (dll, xsymbol_value (Vdll_module_list));
  HMODULE h = GetModuleHandle (name);
  if (!h)
    {
      h = WINFS::LoadLibrary (name);
      if (!h)
        FEsimple_win32_error (GetLastError (), lname);
      xdll_module_loaded (dll) = 1;
    }
  xdll_module_name (dll) = lname;
  xdll_module_handle (dll) = h;
  xsymbol_value (Vdll_module_list) = list;
  return dll;
}

static u_char
check_c_type (lisp type)
{
  if (type == Kvoid)
    return CTYPE_VOID;
  if (type == Kint8)
    return CTYPE_INT8;
  if (type == Kuint8)
    return CTYPE_UINT8;
  if (type == Kint16)
    return CTYPE_INT16;
  if (type == Kuint16)
    return CTYPE_UINT16;
  if (type == Kint32)
    return CTYPE_INT32;
  if (type == Kuint32)
    return CTYPE_UINT32;
  if (type == Kint64)
    return CTYPE_INT64;
  if (type == Kuint64)
    return CTYPE_UINT64;
  if (type == Kfloat)
    return CTYPE_FLOAT;
  if (type == Kdouble)
    return CTYPE_DOUBLE;
  FEprogram_error (Eunknown_c_type, type);
  return 0;
}

static u_char
check_calling_convention (lisp keys)
{
  lisp convention = find_keyword (Kconvention, keys);
  if (convention == Qnil || convention == Kstdcall)
    return CALLING_CONVENTION_STDCALL;
  if (convention == Kcdecl)
    return CALLING_CONVENTION_CDECL;
  FEprogram_error (Eunknown_calling_convention, convention);
  return 0;
}

static int
calc_c_size (u_char type)
{
  switch (type)
    {
    case CTYPE_INT8:
    case CTYPE_UINT8:
    case CTYPE_INT16:
    case CTYPE_UINT16:
    case CTYPE_INT32:
    case CTYPE_UINT32:
      return sizeof (int);

    case CTYPE_INT64:
    case CTYPE_UINT64:
      return sizeof (int64_t);

    case CTYPE_FLOAT:
      return sizeof (float);

    case CTYPE_DOUBLE:
      return sizeof (double);

    default:
      assert (0);
    }
  return 0;
}

static u_char
check_vaarg_type (lisp type)
{
  u_char t = check_c_type (type);

  // default argument promotions
  switch (t)
    {
    case CTYPE_INT8:
    case CTYPE_INT16:
      t = CTYPE_INT32;
      break;

    case CTYPE_UINT8:
    case CTYPE_UINT16:
      t = CTYPE_UINT32;
      break;

    case CTYPE_FLOAT:
      t = CTYPE_DOUBLE;
      break;
    }

  return t;
}

static void
check_vaargs (lisp vaargs)
{
  if (vaargs == Qnil)
    return;

  if (!consp (vaargs))
    FEprogram_error (Einvalid_c_vaarg_type, vaargs);

  for (; consp (vaargs); vaargs = xcdr (vaargs))
    {
      lisp vaarg = xcar (vaargs);
      if (!consp (vaarg) || xlist_length (vaarg) != 2)
        FEprogram_error (Einvalid_c_vaarg_type, vaargs);

      u_char t = check_c_type (xcar (vaarg));
      if (t == CTYPE_VOID)
        FEprogram_error (Einvalid_c_vaarg_type, vaargs);
    }
}

static int
calc_vaarg_size (lisp fn, lisp arglist)
{
  if (!xdll_function_vaarg_p (fn))
    return 0;

  int nargs = xdll_function_nargs (fn);
  lisp vaargs = Fnth (make_fixnum (nargs), arglist);
  if (vaargs == Qnil)
    return 0;

  check_vaargs (vaargs);
  int size = 0;
  for (; consp (vaargs); vaargs = xcdr (vaargs))
    {
      u_char t = check_vaarg_type (Fcaar (vaargs));
      size += calc_c_size (t);
    }
  return size;
}

static int
calc_argument_size (u_char *at, lisp largs)
{
  int size = 0;
  for (lisp a = largs; consp (a); a = xcdr (a))
    {
      u_char t = check_c_type (xcar (a));
      *at++ = t;
      size += calc_c_size (t);
    }
  return size;
}

lisp
Fsi_make_c_function (lisp lmodule, lisp lname, lisp largs, lisp lrettype, lisp keys)
{
  check_dll_module (lmodule);
  check_string (lname);

  char *name = (char *)alloca (xstring_length (lname) * 2 + 1);
  w2s (name, lname);
  FARPROC proc = GetProcAddress (xdll_module_handle (lmodule), name);
  if (!proc)
    FEsimple_win32_error (GetLastError (), lname);

  int return_type = check_c_type (lrettype);
  int nargs = 0;
  for (lisp a = largs; consp (a); a = xcdr (a), nargs++)
    if (check_c_type (xcar (a)) == CTYPE_VOID)
      FEprogram_error (Einvalid_c_argument_type, Kvoid);

  u_char vaarg_p = find_keyword_bool (Kvaarg, keys);
  if (vaarg_p && check_calling_convention (keys) == CALLING_CONVENTION_STDCALL)
    FEprogram_error (Ecannot_call_vaarg_function_by_stdcall);

  lisp fn = make_dll_function ();
  xdll_function_module (fn) = lmodule;
  xdll_function_name (fn) = lname;
  xdll_function_proc (fn) = proc;
  xdll_function_return_type (fn) = return_type;
  xdll_function_vaarg_p (fn) = vaarg_p;

  xdll_function_nargs (fn) = nargs;
  if (nargs)
    {
      u_char *at = (u_char *)xmalloc (nargs);
      xdll_function_arg_types (fn) = at;
      xdll_function_arg_size (fn) = calc_argument_size (at, largs);
    }
  return fn;
}

lisp
Fsi_last_win32_error ()
{
  return xsymbol_value (Vlast_win32_error);
}

lisp
Fsi_set_last_win32_error (lisp lerror)
{
  DWORD error = fixnum_value (lerror);
  SetLastError (error);
  xsymbol_value (Vlast_win32_error) = lerror;
  return lerror;
}

static __forceinline void
save_last_error ()
{
  xsymbol_value (Vlast_win32_error) = make_fixnum (GetLastError ());
}

template<typename T>
static __forceinline T
call_proc (FARPROC proc)
{
  T r = (reinterpret_cast <T (__stdcall *)()> (proc))();
  save_last_error ();
  return r;
}

static char *
push_arg (char *stack, u_char at, lisp a)
{
  switch (at)
    {
    default:
      assert (0);
    case CTYPE_INT8:
    case CTYPE_UINT8:
    case CTYPE_INT16:
    case CTYPE_UINT16:
    case CTYPE_INT32:
    case CTYPE_UINT32:
      *(long *)stack = cast_to_long (a);
      stack += sizeof (long);
      break;

    case CTYPE_INT64:
    case CTYPE_UINT64:
      *(int64_t *)stack = cast_to_int64 (a);
      stack += sizeof (int64_t);
      break;

    case CTYPE_FLOAT:
      *(float *)stack = coerce_to_single_float (a);
      stack += sizeof (float);
      break;

    case CTYPE_DOUBLE:
      *(double *)stack = coerce_to_double_float (a);
      stack += sizeof (double);
      break;
    }
  return stack;
}

static char *
push_vaarg (char *stack, lisp vaarg)
{
  u_char t = check_vaarg_type (xcar (vaarg));
  return push_arg (stack, t, Fcadr (vaarg));
}

lisp
funcall_dll (lisp fn, lisp arglist)
{
  assert (dll_function_p (fn));

  if (!xdll_function_proc (fn))
    FEprogram_error (Edll_not_initialized, fn);

#ifdef _M_IX86
  int arg_size = xdll_function_arg_size (fn) + calc_vaarg_size (fn, arglist);
  char *stack = (char *)alloca (arg_size);
  for (const u_char *at = xdll_function_arg_types (fn),
       *ae = at + xdll_function_nargs (fn);
       at < ae; at++, arglist = xcdr (arglist))
    {
      if (!consp (arglist))
        FEtoo_few_arguments ();
      stack = push_arg (stack, *at, xcar (arglist));
    }

  if (consp (arglist) && xdll_function_vaarg_p (fn))
    {
      lisp vaargs = xcar (arglist);
      check_vaargs (vaargs);
      for (; consp (vaargs); vaargs = xcdr (vaargs))
        stack = push_vaarg (stack, xcar (vaargs));
      arglist = xcdr (arglist);
    }

  if (consp (arglist))
    FEtoo_many_arguments ();

  FARPROC proc = xdll_function_proc (fn);
  try
    {
      switch (xdll_function_return_type (fn))
        {
        default:
          assert (0);

        case CTYPE_VOID:
          proc ();
          return Qnil;

        case CTYPE_INT8:
          return make_fixnum (call_proc <char> (proc));

        case CTYPE_UINT8:
          return make_fixnum (call_proc <u_char> (proc));

        case CTYPE_INT16:
          return make_fixnum (call_proc <short> (proc));

        case CTYPE_UINT16:
          return make_fixnum (call_proc <u_short> (proc));

        case CTYPE_INT32:
          return make_fixnum (call_proc <long> (proc));

        case CTYPE_UINT32:
          return make_integer (call_proc <u_long> (proc));

        case CTYPE_INT64:
          return make_integer (call_proc <int64_t> (proc));

        case CTYPE_UINT64:
          return make_integer (call_proc <uint64_t> (proc));

        case CTYPE_FLOAT:
          return make_single_float (call_proc <float> (proc));

        case CTYPE_DOUBLE:
          return make_double_float (call_proc <double> (proc));
        }
    }
  catch (Win32Exception &e)
    {
      e.throw_lisp_error ();
      throw;
    }
#else
# error "yet"
#endif
}

lisp
funcall_c_callable (lisp fn, lisp arglist)
{
  QUIT;
  return Ffuncall (xc_callable_function (fn), arglist);
}

#ifdef _M_IX86

/*
  ESP ------------------
       c-callable object
  +4  ------------------
       stack (points to ESP + 12)
  +8  ------------------
       return address (from c_callable_stub)
  +12 ------------------
       original c-callable object
  +16 ------------------
       return address (from c_callable_stub_int, etc.)
  +20 ------------------
       passed arguments...
 */

static lisp
c_callable_stub (lisp cc, char *stack)
{
  char *cargs = stack + 8 + xc_callable_arg_size (cc);
  lisp largs = Qnil;
  for (int i = xc_callable_nargs (cc) - 1; i >= 0; i--)
    {
      lisp v;
      switch (xc_callable_arg_types (cc)[i])
        {
        case CTYPE_INT8:
          cargs -= sizeof (int);
          v = make_fixnum (*(char *)cargs);
          break;

        case CTYPE_UINT8:
          cargs -= sizeof (int);
          v = make_fixnum (*(u_char *)cargs);
          break;

        case CTYPE_INT16:
          cargs -= sizeof (int);
          v = make_fixnum (*(short *)cargs);
          break;

        case CTYPE_UINT16:
          cargs -= sizeof (int);
          v = make_fixnum (*(u_short *)cargs);
          break;

        case CTYPE_INT32:
          cargs -= sizeof (int);
          v = make_fixnum (*(int *)cargs);
          break;

        case CTYPE_UINT32:
          cargs -= sizeof (int);
          v = make_integer (int64_t (*(u_long *)cargs));
          break;

        case CTYPE_INT64:
          cargs -= sizeof (int64_t);
          v = make_integer (*(int64_t *)cargs);
          break;

        case CTYPE_UINT64:
          cargs -= sizeof (int64_t);
          v = make_integer (*(uint64_t *)cargs);
          break;

        case CTYPE_FLOAT:
          cargs -= sizeof (float);
          v = make_single_float (*(float *)cargs);
          break;

        case CTYPE_DOUBLE:
          cargs -= sizeof (double);
          v = make_double_float (*(double *)cargs);
          break;
        }
      largs = xcons (v, largs);
    }

  protect_gc gcpro (largs);
  return Ffuncall (xc_callable_function (cc), largs);
}

static void __stdcall
c_callable_stub_void (lisp cc)
{
  try
    {
      c_callable_stub (cc, (char *)&cc);
    }
  catch (nonlocal_jump &)
    {
    }
}

static int __stdcall
c_callable_stub_int (lisp cc)
{
  try
    {
      return cast_to_long (c_callable_stub (cc, (char *)&cc));
    }
  catch (nonlocal_jump &)
    {
    }
  return 0;
}

static float __stdcall
c_callable_stub_float (lisp cc)
{
  try
    {
      return coerce_to_single_float (c_callable_stub (cc, (char *)&cc));
    }
  catch (nonlocal_jump &)
    {
    }
  return 0.0F;
}

static double __stdcall
c_callable_stub_double (lisp cc)
{
  try
    {
      return coerce_to_double_float (c_callable_stub (cc, (char *)&cc));
    }
  catch (nonlocal_jump &)
    {
    }
  return 0.0;
}

/* stub code (stdcall):
0000: 68 XX XX XX XX : push SELF
0005: e8 XX XX XX XX : call C-CALLABLE-STUB
000a: c2 NN NN       : ret  N
000d:

   stub code (cdecl):
0000: 68 XX XX XX XX : push SELF
0005: e8 XX XX XX XX : call C-CALLABLE-STUB
000a: c3             : ret
000b: cc             : int 3
000c: cc             : int 3
000d:
 */

void
init_c_callable (lisp cc)
{
  char *stub;
  switch (xc_callable_return_type (cc))
    {
    case CTYPE_VOID:
      stub = (char *)c_callable_stub_void;
      break;

    case CTYPE_FLOAT:
      stub = (char *)c_callable_stub_float;
      break;

    case CTYPE_DOUBLE:
      stub = (char *)c_callable_stub_double;
      break;

    default:
      stub = (char *)c_callable_stub_int;
      break;
    }

  u_char *insn = xc_callable_insn (cc);

  DWORD o = 0;
  if (!VirtualProtect (insn, INSN_SIZE, PAGE_EXECUTE_READWRITE, &o))
    FEsimple_win32_error (GetLastError (), make_fixnum (o));

  insn[0] = 0x68;
  *(lisp *)&insn[1] = cc;
  insn[5] = 0xe8;
  *(long *)&insn[6] = stub - ((char *)insn + 0xa);
  if (xc_callable_convention (cc) == CALLING_CONVENTION_STDCALL)
    {
      insn[0xa] = 0xc2;
      *(short *)&insn[0xb] = short (xc_callable_arg_size (cc));
    }
  else
    {
      insn[0xa] = 0xc3;
      insn[0xb] = 0xcc;
      insn[0xc] = 0xcc;
    }
}
#endif

static lisp
check_fn (lisp fn)
{
  if (!immediatep (fn))
    switch (object_typeof (fn))
      {
      case Tsymbol:
        return Fsymbol_function (fn);

      case Tclosure:
        return fn;

      case Tfunction:
        if (special_form_p (fn))
          FEtype_error (fn, Qfunction);
        return fn;

      case Tcons:
        if (xcar (fn) == Qlambda)
          return fn;
        break;
      }
  return FEinvalid_function (fn);
}

lisp
Fsi_make_c_callable (lisp fn, lisp largs, lisp lrettype, lisp keys)
{
  fn = check_fn (fn);
  int return_type = check_c_type (lrettype);
  int nargs = 0;
  for (lisp a = largs; consp (a); a = xcdr (a), nargs++)
    if (check_c_type (xcar (a)) == CTYPE_VOID)
      FEprogram_error (Einvalid_c_argument_type, Kvoid);

  lisp cc = make_c_callable ();
  xc_callable_function (cc) = fn;
  xc_callable_return_type (cc) = return_type;
  xc_callable_nargs (cc) = nargs;
  xc_callable_convention (cc) = check_calling_convention (keys);
  if (nargs)
    {
      u_char *at = (u_char *)xmalloc (nargs);
      xc_callable_arg_types (cc) = at;
      xc_callable_arg_size (cc) = calc_argument_size (at, largs);
    }
  init_c_callable (cc);
  return cc;
}
