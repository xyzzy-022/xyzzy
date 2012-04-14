#include "stdafx.h"
#include "ed.h"
#include "lex.h"

#define BCconstant 1
#define BCglobal_set 2
#define BCglobal_ref 3
#define BClexical_set 4
#define BClexical_ref 5
#define BClocal_set 8
#define BClocal_ref 9
#define BCmake_closure 10
#define BCdiscard 11
#define BCgoto 12
#define BCif_nil_goto 13
#define BCif_non_nil_goto 14
#define BCif_nil_goto_and_pop 15
#define BCif_non_nil_goto_and_pop 16
#define BCgo 17
#define BCreturn 18
#define BCadjust_stack 19

#define BCcall_0 20
#define BCcall_1 21
#define BCcall_2 22
#define BCcall_3 23
#define BCcall_4 24
#define BCcall_n 25

#define BCglobal_set_discard 26
#define BClexical_set_discard 27
#define BClocal_set_discard 28
#define BClexical_bind 29

#define BCblock 30
#define BCspecial 31
#define BCtagbody 32
#define BCunwind_protect 33
#define BCcatch 34
#define BCthrow 35

#define BCsave_excursion 36
#define BCsave_restriction 37
#define BCsave_window_excursion 38

#define BCfunction_symbol 39

#define BCmultiple_value_set 42
#define BClist_multiple_value 43
#define BCcall_multiple_value 44
#define BCsave_multiple_value 45

#define BCt 80
#define BCnil 81
#define BCinteger_range 1024
#define BCzero 40000

#define BCfuncall 33255
#define BCset 33256
#define BCsymbol_value 33257
#define BCboundp 33258
#define BCconstantp 33259
#define BCspecialp 33260
#define BCmake_constant 33261
#define BCmake_special 33262
#define BCfset 33263
#define BCvalues_list 33264
#define BCvalues 33265
#define BCnull 33266
#define BCsymbolp 33267
#define BCatom 33268
#define BCconsp 33269
#define BCeq 33270
#define BCeql 33271
#define BCequal 33272
#define BCequalp 33273
#define BCcar 33274
#define BCcdr 33275
#define BCcons 33276
#define BCendp 33277
#define BCnth 33278
#define BCnthcdr 33279
#define BClist_1 33280
#define BClist_2 33281
#define BClist_n 33282
#define BCrplaca 33283
#define BCrplacd 33284
#define BCelt 33285
#define BCset_elt 33286
#define BClength 33287
#define BCreverse 33288
#define BCnreverse 33289
#define BCsvref 33290
#define BCsvset 33291
#define BCchar 33294
#define BCset_char 33295
#define BCschar 33296
#define BCset_schar 33297
#define BCstring_eq 33298
#define BCstring_equal 33299
#define BCzerop 33300
#define BCplusp 33301
#define BCminusp 33302
#define BCoddp 33303
#define BCevenp 33304
#define BCnumber_eql 33305
#define BCnumber_not_eql 33306
#define BCnumber_less 33307
#define BCnumber_greater 33308
#define BCnumber_not_greater 33309
#define BCnumber_not_less 33310
#define BCmax 33311
#define BCmin 33312
#define BCadd 33313
#define BCsub 33314
#define BCnagate 33315
#define BCmul 33316
#define BCdiv 33317
#define BCabs 33318
#define BCchar_eql 33319
#define BCchar_not_eql 33320
#define BCchar_less 33321
#define BCchar_greater 33322
#define BCchar_not_greater 33323
#define BCchar_not_less 33324
#define BCchar_equal 33325
#define BCchar_not_equal 33326
#define BCchar_lessp 33327
#define BCchar_greaterp 33328
#define BCchar_not_greaterp 33329
#define BCchar_not_lessp 33330
#define BCchar_code 33331
#define BCcode_char 33332

#define BCbobp 33536
#define BCeobp 33537
#define BCbolp 33538
#define BCeolp 33539
#define BCgoto_bol 33540
#define BCgoto_eol 33541
#define BCforward_char 33542
#define BCforward_line 33543
#define BCgoto_line 33544
#define BCgoto_column 33545
#define BCcurrent_column 33546
#define BCfollowing_char 33547
#define BCpreceding_char 33548
#define BCpoint 33549
#define BCgoto_char 33550
#define BClooking_for 33551
#define BClooking_at 33552
#define BCskip_chars_forward 33553
#define BCskip_chars_backward 33554
#define BCpoint_min 33555
#define BCpoint_max 33556
#define BCskip_syntax_spec_forward 33557
#define BCskip_syntax_spec_backward 33558
#define BCinteractive_p 33559
#define BCget_selection_type 33560
#define BCselection_mark 33561
#define BCstop_selection 33562
#define BCpre_selection_p 33563
#define BCcontinue_pre_selection 33564
#define BCdelete_region 33565
#define BCbuffer_substring 33566
#define BCselection_point 33567
#define BCvirtual_bolp 33568
#define BCvirtual_eolp 33569
#define BCgoto_virtual_bol 33570
#define BCgoto_virtual_eol 33571
#define BCforward_virtual_line 33572
#define BCgoto_virtual_line 33573
#define BCgoto_virtual_column 33574
#define BCcurrent_virtual_column 33575

class ByteCode
{
public:
  lisp *bc_stackb;
  lisp *bc_stacke;
  lisp *bc_stackp;

  lisp *bc_constants;
  int bc_nconstants;

  lisp *bc_locals;
  int bc_nlocals;

  Char *bc_opecode;
  int bc_pcb;
  int bc_pce;
  int bc_pc;

  void set_pc (int);
  int fetch ();
  void skip ();
  void xgoto (int);
  lisp constant (int);
  void push (lisp);
  lisp pop ();
  void drop ();
  lisp &top ();
  void set_stack (int);
  int stack_depth () const;
  lisp &local (int);

  void xglobal_set (lisp);
  void xlocal_set (lisp);
  void xlexical_set (lex_env &, lisp);
  lisp xglobal_ref (int);
  lisp xlocal_ref ();
  lisp xlexical_ref (lex_env &, int);
  void xgo (lex_env &);
  void xreturn (lex_env &);
  lisp xfuncall (lisp, int);
  lisp xfuncall (int);
  void xblock (lex_env &);
  void xtagbody (lex_env &);
  void xlexical_bind (lex_env &);
  void xspecial (lex_env &);
  void xunwind_protect (lex_env &);
  void xcatch (lex_env &);
  void xthrow (lex_env &);
  void xmultiple_value_set (lex_env &);
  void xlist_multiple_value ();
  void xcall_multiple_value ();
  void xsave_multiple_value (lex_env &);
  void xsave_excursion (lex_env &);
  void xsave_restriction (lex_env &);
  void xsave_window_excursion (lex_env &);

  void process (lex_env &, int, int);

  ByteCode (lisp *, int, lisp *, int, lisp *, int, Char *, int);
};

inline void
ByteCode::set_pc (int i)
{
  assert (i >= bc_pcb && i <= bc_pce);
  bc_pc = i;
}

inline int
ByteCode::fetch ()
{
  assert (bc_pc >= bc_pcb && bc_pc < bc_pce);
  return bc_opecode[bc_pc++];
}

inline void
ByteCode::skip ()
{
  assert (bc_pc >= bc_pcb && bc_pc < bc_pce);
  bc_pc++;
}

inline void
ByteCode::xgoto (int i)
{
  set_pc (i);
}

inline lisp
ByteCode::constant (int i)
{
  assert (i >= 0 && i < bc_nconstants);
  return bc_constants[i];
}

inline void
ByteCode::push (lisp x)
{
  assert (bc_stackp < bc_stacke);
  *bc_stackp++ = x;
}

inline lisp
ByteCode::pop ()
{
  assert (bc_stackp > bc_stackb);
  return *--bc_stackp;
}

inline void
ByteCode::drop ()
{
  assert (bc_stackp > bc_stackb);
  bc_stackp--;
}

inline lisp &
ByteCode::top ()
{
  assert (bc_stackp != bc_stackb);
  return bc_stackp[-1];
}

inline void
ByteCode::set_stack (int i)
{
  assert (i >= 0 && i < bc_stacke - bc_stackb);
  bc_stackp = bc_stackb + i;
}

inline int
ByteCode::stack_depth () const
{
  return bc_stackp - bc_stackb;
}

inline lisp &
ByteCode::local (int i)
{
  assert (i >= 0 && i < bc_nlocals);
  return bc_locals[i];
}

inline void
ByteCode::xlocal_set (lisp value)
{
  local (fetch ()) = value;
}

inline lisp
ByteCode::xlocal_ref ()
{
  return local (fetch ());
}

void
ByteCode::xglobal_set (lisp value)
{
  lisp var = constant (fetch ());
  assert (symbolp (var));
  check_symbol (var);
  if (constantp (var))
    FEmodify_constant (var);
  set_globally (var, value, selected_buffer ());
}

void
ByteCode::xlexical_set (lex_env &lex, lisp value)
{
  lisp var = constant (fetch ());
  assert (symbolp (var));
  check_symbol (var);
  if (constantp (var))
    FEmodify_constant (var);
  if (!lex.set (var, value))
    {
      assert (0);
    }
}

lisp
ByteCode::xglobal_ref (int noerror)
{
  lisp var = constant (fetch ());
  assert (symbolp (var));
  lisp val = symbol_value (var, selected_buffer ());
  if (!noerror && val == Qunbound)
    FEunbound_variable (var);
  return val;
}

lisp
ByteCode::xlexical_ref (lex_env &lex, int noerror)
{
  lisp var = constant (fetch ());
  lisp val = Qunbound;
  assert (symbolp (var));
  check_symbol (var);
  lisp x = assq (var, lex.lex_var);
  if (x)
    val = xcdr (x);
  if (val == Qunbound)
    {
      val = symbol_value (var, selected_buffer ());
      if (!noerror && val == Qunbound)
        FEunbound_variable (var);
    }
  return val;
}

void
ByteCode::xgo (lex_env &lex)
{
  lisp tag = constant (fetch ());
  lisp x = lex.search_frame (Qtagbody, tag);
  if (x == Qnil)
    FEno_target_for (Sgo, tag);
  assert (consp (x));
  assert (consp (xcdr (x)));
  lisp frame = xcdr (xcdr (x));
  if (!active_frame_p (frame))
    FEtarget_missing (Sgo, tag);
  nonlocal_data *nld = nonlocal_jump::data ();
  nld->type = Qtagbody;
  nld->value = Qnil;
  nld->tag = tag;
  nld->id = frame;
  throw nonlocal_jump ();
}

void
ByteCode::xreturn (lex_env &lex)
{
  lisp tag = constant (fetch ());
  lisp value = pop ();
  lisp x = lex.search_frame (Qblock, tag);
  if (x == Qnil)
    FEno_target_for (Sreturn_from, tag);
  assert (consp (x));
  assert (consp (xcdr (x)));
  lisp frame = xcdr (xcdr (x));
  if (!active_frame_p (frame))
    FEtarget_missing (Sreturn_from, tag);

  nonlocal_data *nld = nonlocal_jump::data ();
  nld->type = Qblock;
  nld->value = value;
  nld->tag = tag;
  nld->id = frame;
  throw nonlocal_jump ();
}

lisp
ByteCode::xfuncall (lisp fn, int nargs)
{
  assert (bc_stackp - bc_stackb >= nargs);
  lisp arg = Qnil;
  for (; nargs > 0; nargs--)
    arg = xcons (pop (), arg);
  protect_gc gcpro (arg);
  return Ffuncall (fn, arg);
}

lisp
ByteCode::xfuncall (int nargs)
{
  assert (bc_stackp - bc_stackb >= nargs);
  lisp arg = Qnil;
  for (nargs--; nargs > 0; nargs--)
    arg = xcons (pop (), arg);
  protect_gc gcpro (arg);
  return Ffuncall (pop (), arg);
}

void
ByteCode::xblock (lex_env &olex)
{
  int depth = stack_depth ();
  int next_pc = fetch ();
  lisp tag = constant (fetch ());

  lisp frame = make_frame ();
  dynamic_extent dyn (frame);
  lex_env nlex (olex);
  nlex.bind_frame (Qblock, tag, frame);

  try
    {
      process (nlex, bc_pc, next_pc);
    }
  catch (nonlocal_jump &)
    {
      nonlocal_data *nld = nonlocal_jump::data ();
      if (nld->type != Qblock || nld->tag != tag || nld->id != frame)
        throw;
      set_pc (next_pc);
      set_stack (depth);
      push (nld->value);
    }
}

void
ByteCode::xtagbody (lex_env &olex)
{
  int depth = stack_depth ();
  int end_pc = fetch ();
  int ntags = fetch ();
  int frame_pc = bc_pc;

  lisp frame = make_frame ();
  protect_gc gcpro (frame);
  dynamic_extent dyn (frame);
  lex_env nlex (olex);

  for (int i = 0; i < ntags; i++)
    {
      skip ();
      nlex.bind_frame (Qtagbody, constant (fetch ()), frame);
    }

  int start_pc = bc_pc;

restart:
  try
    {
      process (nlex, start_pc, end_pc);
    }
  catch (nonlocal_jump &)
    {
      nonlocal_data *nld = nonlocal_jump::data ();
      if (nld->type == Qtagbody && nld->id == frame)
        {
          set_pc (frame_pc);
          for (int i = 0; i < ntags; i++)
            {
              int next_pc = fetch ();
              if (Feql (nld->tag, constant (fetch ())) != Qnil)
                {
                  set_pc (next_pc);
                  set_stack (depth);
                  goto restart;
                }
            }
        }
      throw;
    }
}

void
ByteCode::xlexical_bind (lex_env &olex)
{
  int next_pc = fetch ();
  int nvars = fetch ();
  lex_env nlex (olex);
  for (int i = 0; i < nvars; i++)
    nlex.bind (constant (fetch ()), Qnil);
  process (nlex, bc_pc, next_pc);
}

class byte_special_bind
{
  lisp *save;
  char *flags;
  int n;
public:
  byte_special_bind (lisp *, char *, int);
  ~byte_special_bind ();
};

inline
byte_special_bind::byte_special_bind (lisp *p, char *f, int i)
     : save (p), flags (f), n (i)
{
}

inline
byte_special_bind::~byte_special_bind ()
{
  for (int i = n - 2, j = n/2 - 1; i >= 0; i -= 2, j--)
    {
      assert (symbolp (save[i]));
      if (!flags[j])
        xsymbol_flags (save[i]) &= ~SFdynamic_bind;
      else
        assert (xsymbol_flags (save[i]) & SFdynamic_bind);
      xsymbol_value (save[i]) = save[i + 1];
    }
}

void
ByteCode::xspecial (lex_env &lex)
{
  int next_pc = fetch ();
  int nargs = fetch ();
  char *oflags = (char *)alloca (nargs + sizeof (lisp) * 2 * nargs);
  lisp *save = (lisp *)(oflags + nargs);
  int i, j;
  for (i = 0, j = 0; i < nargs; i++)
    {
      lisp var = constant (fetch ());
      assert (symbolp (var));
      oflags[i] = xsymbol_flags (var) & SFdynamic_bind;
      xsymbol_flags (var) |= SFdynamic_bind;
      save[j++] = var;
      save[j++] = xsymbol_value (var);
    }

  protect_gc gcpro (save, j);
  byte_special_bind bsb (save, oflags, j);

  for (i = j = 0; i < nargs; i++, j += 2)
    {
      lisp x;
      switch (fetch ())
        {
        case BCglobal_ref:
          x = xglobal_ref (1);
          break;

        case BClexical_ref:
          x = xlexical_ref (lex, 1);
          break;

        case BClocal_ref:
          x = xlocal_ref ();
          break;

        default:
          assert (0);
          x = Qnil;
          break;
        }
      xsymbol_value (save[j]) = x;
    }
  process (lex, bc_pc, next_pc);
}

void
ByteCode::xunwind_protect (lex_env &lex)
{
  int protect_pc = fetch ();
  int cleanup_pc = fetch ();
  int depth = stack_depth ();

  try
    {
      process (lex, bc_pc, protect_pc);
    }
  catch (nonlocal_jump &)
    {
      save_multiple_value save_mvalue (multiple_value::value (0));
      save_nonlocal_jump save_nlocal;
      try
        {
          set_stack (depth + 1);
          set_pc (protect_pc);
          process (lex, protect_pc, cleanup_pc);
        }
      catch (nonlocal_jump &)
        {
        }
      throw;
    }
  save_multiple_value save_mvalue (top ());
  int next_pc = bc_pc == protect_pc ? cleanup_pc : bc_pc;
  assert (stack_depth () == depth + 1);
  set_pc (protect_pc);
  process (lex, protect_pc, cleanup_pc);
  set_pc (next_pc);
  set_stack (depth + 1);
}

void
ByteCode::xcatch (lex_env &lex)
{
  lisp tag = pop ();
  protect_gc gcpro (tag);
  int next_pc = fetch ();
  int depth = stack_depth ();

  try
    {
      process (lex, bc_pc, next_pc);
    }
  catch (nonlocal_jump &)
    {
      nonlocal_data *nld = nonlocal_jump::data ();
      if (nld->type == Qcatch && nld->tag == tag)
        {
          set_pc (next_pc);
          set_stack (depth);
          push (nld->value);
          return;
        }
      throw;
    }
}

void
ByteCode::xthrow (lex_env &)
{
  lisp tag = pop ();
  lisp value = pop ();
  nonlocal_data *nld = nonlocal_jump::data ();
  nld->type = Qcatch;
  nld->value = value;
  nld->tag = tag;
  nld->id = Qnil;
  throw nonlocal_jump ();
}

void
ByteCode::xmultiple_value_set (lex_env &lex)
{
  int n = fetch ();
  multiple_value::value (0) = top ();
  for (int i = 0; i < n; i++)
    {
      lisp value = i < multiple_value::count () ? multiple_value::value (i) : Qnil;
      switch (fetch ())
        {
        case BCglobal_set:
          xglobal_set (value);
          break;

        case BClexical_set:
          xlexical_set (lex, value);
          break;

        case BClocal_set:
          xlocal_set (value);
          break;

        default:
          assert (0);
          break;
        }
    }
}

inline void
ByteCode::xlist_multiple_value ()
{
  multiple_value::value (0) = pop ();
  lisp list = top ();
  for (int i = 0; i < multiple_value::count (); i++)
    list = xcons (multiple_value::value (i), list);
  top () = list;
}

inline void
ByteCode::xcall_multiple_value ()
{
  lisp args = Fnreverse (pop ());
  protect_gc gcpro (args);
  lisp fn = top ();
  top () = Ffuncall (fn, args);
}

void
ByteCode::xsave_multiple_value (lex_env &lex)
{
  multiple_value::value (0) = top ();
  multiple_value_data save;
  save.count = multiple_value::count ();
  bcopy (multiple_value::data ()->values, save.values, save.count);
  protect_gc gcpro (save.values, save.count);
  int next_pc = fetch ();
  process (lex, bc_pc, next_pc);
  bcopy (save.values, multiple_value::data ()->values, save.count);
  multiple_value::count () = save.count;
}

inline void
ByteCode::xsave_excursion (lex_env &lex)
{
  save_excursion se;
  process (lex, bc_pc, fetch ());
  se.cleanup ();
}

inline void
ByteCode::xsave_restriction (lex_env &lex)
{
  save_restriction sr;
  process (lex, bc_pc, fetch ());
}

inline void
ByteCode::xsave_window_excursion (lex_env &lex)
{
  WindowConfiguration wc;
  process (lex, bc_pc, fetch ());
}

#define NUMBER_PRED(OP) \
  y = pop (); \
  x = top (); \
  top () = boole (number_compare (x, y) OP 0)

#define NUMBER_PRED_REAL(OP) \
  y = pop (); \
  x = top (); \
  if (!realp (x)) FEtype_error (x, Qreal); \
  if (!realp (y)) FEtype_error (y, Qreal); \
  top () = boole (number_compare (x, y) OP 0)

#define NUMBER_MINMAX(OP) \
  y = pop (); \
  x = top (); \
  if (!realp (x)) FEtype_error (x, Qreal); \
  if (!realp (y)) FEtype_error (y, Qreal); \
  top () = number_compare (x, y) OP 0 ? x : y

#define STRING_PRED(FN) \
  y = pop (); \
  x = top (); \
  x = coerce_to_string (x, 0); \
  y = coerce_to_string (y, 0); \
  top () = boole ((FN)(x, y))

#define CHAR_PRED(OP) \
  y = pop (); \
  x = top (); \
  check_char (x); \
  check_char (y); \
  top () = boole (xchar_code (x) OP xchar_code (y))

#define CHAR_CASE_PRED(OP) \
  y = pop (); \
  x = top (); \
  check_char (x); \
  check_char (y); \
  top () = boole (char_upcase (xchar_code (x)) OP char_upcase (xchar_code (y)))

#define CALL_0(FN) \
  push ((FN)())

#define CALL_1(FN) \
  top () = (FN)(top ())

#define CALL_2(FN) \
  y = pop (); \
  x = top (); \
  top () = (FN)(x, y)

#define CALL_3(FN) \
  z = pop (); \
  y = pop (); \
  x = top (); \
  top () = (FN)(x, y, z)

void
ByteCode::process (lex_env &lex, int limb, int lime)
{
  lisp x, y, z;
  int i, n;

#define AFTER_JUMP goto after_jump

after_jump:

  if (bc_pc < limb)
    return;

  QUIT;

  while (bc_pc < lime)
    {
      int ope = fetch ();
      switch (ope)
        {
        case BCconstant:
          push (constant (fetch ()));
          break;

        case BCglobal_set:
          xglobal_set (top ());
          break;

        case BCglobal_set_discard:
          xglobal_set (pop ());
          break;

        case BCglobal_ref:
          push (xglobal_ref (0));
          break;

        case BClexical_set:
          xlexical_set (lex, top ());
          break;

        case BClexical_set_discard:
          xlexical_set (lex, pop ());
          break;

        case BClexical_ref:
          push (xlexical_ref (lex, 0));
          break;

        case BClocal_set:
          xlocal_set (top ());
          break;

        case BClocal_set_discard:
          xlocal_set (pop ());
          break;

        case BClocal_ref:
          push (xlocal_ref ());
          break;

        case BCmake_closure:
          push (make_closure (constant (fetch ()), lex.lex_var,
                              lex.lex_fns, lex.lex_frame));
          break;

        case BCdiscard:
          drop ();
          continue;

        case BCgoto:
          xgoto (fetch ());
          AFTER_JUMP;

        case BCif_nil_goto:
          if (top () == Qnil)
            {
              xgoto (fetch ());
              AFTER_JUMP;
            }
          else
            {
              skip ();
              drop ();
            }
          continue;

        case BCif_non_nil_goto:
          if (top () != Qnil)
            {
              xgoto (fetch ());
              AFTER_JUMP;
            }
          else
            {
              skip ();
              drop ();
            }
          continue;

        case BCif_nil_goto_and_pop:
          if (pop () == Qnil)
            {
              xgoto (fetch ());
              AFTER_JUMP;
            }
          else
            skip ();
          continue;

        case BCif_non_nil_goto_and_pop:
          if (pop () != Qnil)
            {
              xgoto (fetch ());
              AFTER_JUMP;
            }
          else
            skip ();
          continue;

        case BCgo:
          xgo (lex);
          AFTER_JUMP;

        case BCreturn:
          xreturn (lex);
          AFTER_JUMP;

        case BCadjust_stack:
          set_stack (fetch ());
          continue;

        case BCcall_0:
        case BCcall_1:
        case BCcall_2:
        case BCcall_3:
        case BCcall_4:
          push (xfuncall (constant (fetch ()), ope - BCcall_0));
          continue;

        case BCcall_n:
          n = fetch ();
          push (xfuncall (constant (fetch ()), n));
          continue;

        case BCblock:
          xblock (lex);
          AFTER_JUMP;

        case BCtagbody:
          xtagbody (lex);
          AFTER_JUMP;

        case BClexical_bind:
          xlexical_bind (lex);
          AFTER_JUMP;

        case BCspecial:
          xspecial (lex);
          AFTER_JUMP;

        case BCunwind_protect:
          xunwind_protect (lex);
          AFTER_JUMP;

        case BCcatch:
          xcatch (lex);
          AFTER_JUMP;

        case BCthrow:
          xthrow (lex);
          AFTER_JUMP;

        case BCsave_excursion:
          xsave_excursion (lex);
          AFTER_JUMP;

        case BCsave_restriction:
          xsave_restriction (lex);
          AFTER_JUMP;

        case BCsave_window_excursion:
          xsave_window_excursion (lex);
          AFTER_JUMP;

        case BCfunction_symbol:
          push (Fsymbol_function (constant (fetch ())));
          break;

        case BCmultiple_value_set:
          xmultiple_value_set (lex);
          break;

        case BClist_multiple_value:
          xlist_multiple_value ();
          break;

        case BCcall_multiple_value:
          xcall_multiple_value ();
          continue;

        case BCsave_multiple_value:
          xsave_multiple_value (lex);
          AFTER_JUMP;

        case BCt:
          push (Qt);
          break;

        case BCnil:
          push (Qnil);
          break;

        case BCfuncall:
          push (xfuncall (fetch ()));
          continue;

        case BCset:
          CALL_2 (Fset);
          break;

        case BCsymbol_value:
          CALL_1 (Fsymbol_value);
          break;

        case BCboundp:
          CALL_1 (Fboundp);
          break;

        case BCconstantp:
          CALL_1 (Fconstantp);
          break;

        case BCspecialp:
          CALL_1 (Fsi_specialp);
          break;

        case BCmake_constant:
          CALL_1 (Fsi_make_constant);
          break;

        case BCmake_special:
          CALL_1 (Fsi_make_special);
          break;

        case BCfset:
          CALL_2 (Fsi_fset);
          break;

        case BCvalues_list:
          x = top ();
          for (i = 0; consp (x); i++, x = xcdr (x))
            {
              if (i == MULTIPLE_VALUES_LIMIT)
                FEtoo_many_arguments ();
              multiple_value::value (i) = xcar (x);
            }
          multiple_value::count () = i;
          if (!i)
            multiple_value::value (0) = Qnil;
          top () = multiple_value::value (0);
          continue;

        case BCvalues:
          n = fetch ();
          if (n >= MULTIPLE_VALUES_LIMIT)
            FEtoo_many_arguments ();
          multiple_value::count () = n;
          multiple_value::value (0) = Qnil;
          while (n-- > 0)
            multiple_value::value (n) = pop ();
          push (multiple_value::value (0));
          continue;

        case BCnull:
          top () = boole (top () == Qnil);
          break;

        case BCsymbolp:
          top () = boole (symbolp (top ()));
          break;

        case BCatom:
          top () = boole (!consp (top ()));
          break;

        case BCconsp:
          top () = boole (consp (top ()));
          break;

        case BCeq:
          x = pop ();
          top () = boole (top () == x);
          break;

        case BCeql:
          CALL_2 (Feql);
          break;

        case BCequal:
          CALL_2 (Fequal);
          break;

        case BCequalp:
          CALL_2 (Fequalp);
          break;

        case BCcar:
          x = top ();
          if (x != Qnil)
            {
              check_cons (x);
              top () = xcar (x);
            }
          break;

        case BCcdr:
          x = top ();
          if (x != Qnil)
            {
              check_cons (x);
              top () = xcdr (x);
            }
          break;

        case BCcons:
          CALL_2 (xcons);
          break;

        case BCendp:
          CALL_1 (Fendp);
          break;

        case BCnth:
          CALL_2 (Fnth);
          break;

        case BCnthcdr:
          CALL_2 (Fnthcdr);
          break;

        case BClist_1:
          top () = xcons (top (), Qnil);
          break;

        case BClist_2:
          y = pop ();
          x = top ();
          top () = xcons (x, xcons (y, Qnil));
          break;

        case BClist_n:
          n = fetch ();
          x = Qnil;
          while (n-- > 0)
            x = xcons (pop (), x);
          push (x);
          break;

        case BCrplaca:
          CALL_2 (Frplaca);
          break;

        case BCrplacd:
          CALL_2 (Frplacd);
          break;

        case BCelt:
          CALL_2 (Felt);
          break;

        case BCset_elt:
          CALL_3 (Fsi_set_elt);
          break;

        case BClength:
          CALL_1 (Flength);
          break;

        case BCreverse:
          CALL_1 (Freverse);
          break;

        case BCnreverse:
          CALL_1 (Fnreverse);
          break;

        case BCsvref:
          CALL_2 (Fsvref);
          break;

        case BCsvset:
          CALL_3 (Fsi_svset);
          break;

        case BCchar:
          CALL_2 (Fchar);
          break;

        case BCset_char:
          CALL_3 (Fsi_set_char);
          break;

        case BCschar:
          CALL_2 (Fschar);
          break;

        case BCset_schar:
          CALL_3 (Fsi_set_schar);
          break;

        case BCstring_eq:
          STRING_PRED (string_equal);
          break;

        case BCstring_equal:
          STRING_PRED (string_equalp);
          break;

        case BCzerop:
          CALL_1 (Fzerop);
          break;

        case BCplusp:
          CALL_1 (Fplusp);
          break;

        case BCminusp:
          CALL_1 (Fminusp);
          break;

        case BCoddp:
          CALL_1 (Foddp);
          break;

        case BCevenp:
          CALL_1 (Fevenp);
          break;

        case BCnumber_eql:
          NUMBER_PRED (==);
          break;

        case BCnumber_not_eql:
          NUMBER_PRED (!=);
          break;

        case BCnumber_less:
          NUMBER_PRED_REAL (<);
          break;

        case BCnumber_greater:
          NUMBER_PRED_REAL (>);
          break;

        case BCnumber_not_greater:
          NUMBER_PRED_REAL (<=);
          break;

        case BCnumber_not_less:
          NUMBER_PRED_REAL (>=);
          break;

        case BCmax:
          NUMBER_MINMAX (>);
          break;

        case BCmin:
          NUMBER_MINMAX (<);
          break;

        case BCadd:
          CALL_2 (number_add);
          break;

        case BCsub:
          CALL_2 (number_subtract);
          break;

        case BCnagate:
          CALL_1 (number_negate);
          break;

        case BCmul:
          CALL_2 (number_multiply);
          break;

        case BCdiv:
          CALL_2 (number_divide);
          break;

        case BCabs:
          CALL_1 (Fabs);
          break;

        case BCchar_eql:
          CHAR_PRED (==);
          break;

        case BCchar_not_eql:
          CHAR_PRED (!=);
          break;

        case BCchar_less:
          CHAR_PRED (<);
          break;

        case BCchar_greater:
          CHAR_PRED (>);
          break;

        case BCchar_not_greater:
          CHAR_PRED (<=);
          break;

        case BCchar_not_less:
          CHAR_PRED (>=);
          break;

        case BCchar_equal:
          CHAR_CASE_PRED (==);
          break;

        case BCchar_not_equal:
          CHAR_CASE_PRED (!=);
          break;

        case BCchar_lessp:
          CHAR_CASE_PRED (<);
          break;

        case BCchar_greaterp:
          CHAR_CASE_PRED (>);
          break;

        case BCchar_not_greaterp:
          CHAR_CASE_PRED (<=);
          break;

        case BCchar_not_lessp:
          CHAR_CASE_PRED (>=);
          break;

        case BCchar_code:
          x = top ();
          check_char (x);
          top () = make_fixnum (xchar_code (x));
          break;

        case BCcode_char:
          top () = make_char (Char (fixnum_value (top ())));
          break;

        case BCbobp:
          CALL_0 (Fbobp);
          break;

        case BCeobp:
          CALL_0 (Feobp);
          break;

        case BCbolp:
          CALL_0 (Fbolp);
          break;

        case BCeolp:
          CALL_0 (Feolp);
          break;

        case BCgoto_bol:
          CALL_0 (Fgoto_bol);
          break;

        case BCgoto_eol:
          CALL_0 (Fgoto_eol);
          break;

        case BCforward_char:
          CALL_1 (Fforward_char);
          break;

        case BCforward_line:
          CALL_1 (Fforward_line);
          break;

        case BCgoto_line:
          CALL_1 (Fgoto_line);
          break;

        case BCgoto_column:
          CALL_2 (Fgoto_column);
          break;

        case BCcurrent_column:
          CALL_0 (Fcurrent_column);
          break;

        case BCvirtual_bolp:
          CALL_0 (Fvirtual_bolp);
          break;

        case BCvirtual_eolp:
          CALL_0 (Fvirtual_eolp);
          break;

        case BCgoto_virtual_bol:
          CALL_0 (Fgoto_virtual_bol);
          break;

        case BCgoto_virtual_eol:
          CALL_0 (Fgoto_virtual_eol);
          break;

        case BCforward_virtual_line:
          CALL_1 (Fforward_virtual_line);
          break;

        case BCgoto_virtual_line:
          CALL_1 (Fgoto_virtual_line);
          break;

        case BCgoto_virtual_column:
          CALL_2 (Fgoto_virtual_column);
          break;

        case BCcurrent_virtual_column:
          CALL_0 (Fcurrent_virtual_column);
          break;

        case BCfollowing_char:
          CALL_0 (Ffollowing_char);
          break;

        case BCpreceding_char:
          CALL_0 (Fpreceding_char);
          break;

        case BCpoint:
          CALL_0 (Fpoint);
          break;

        case BCgoto_char:
          CALL_1 (Fgoto_char);
          break;

        case BClooking_for:
          CALL_2 (Flooking_for);
          break;

        case BClooking_at:
          CALL_2 (Flooking_at);
          break;

        case BCskip_chars_forward:
          CALL_1 (Fskip_chars_forward);
          break;

        case BCskip_chars_backward:
          CALL_1 (Fskip_chars_backward);
          break;

        case BCpoint_min:
          CALL_0 (Fpoint_min);
          break;

        case BCpoint_max:
          CALL_0 (Fpoint_max);
          break;

        case BCskip_syntax_spec_forward:
          CALL_1 (Fskip_syntax_spec_forward);
          break;

        case BCskip_syntax_spec_backward:
          CALL_1 (Fskip_syntax_spec_backward);
          break;

        case BCinteractive_p:
          CALL_0 (Finteractive_p);
          break;

        case BCget_selection_type:
          CALL_0 (Fget_selection_type);
          break;

        case BCselection_point:
          CALL_0 (Fselection_point);
          break;

        case BCselection_mark:
          CALL_0 (Fselection_mark);
          break;

        case BCstop_selection:
          CALL_0 (Fstop_selection);
          break;

        case BCpre_selection_p:
          CALL_0 (Fpre_selection_p);
          break;

        case BCcontinue_pre_selection:
          CALL_0 (Fcontinue_pre_selection);
          break;

        case BCdelete_region:
          CALL_2 (Fdelete_region);
          break;

        case BCbuffer_substring:
          CALL_2 (Fbuffer_substring);
          break;

        default:
          if (ope >= BCzero - BCinteger_range && ope <= BCzero + BCinteger_range)
            push (make_short_int (ope - BCzero));
          else
            {
              assert (0);
              FEinvalid_byte_code ();
            }
          break;
        }
      multiple_value::count () = 1;
    }
}

inline
ByteCode::ByteCode (lisp *frame, int nframe, lisp *stack, int nstack,
                    lisp *constant, int nconstant, Char *string, int slen)
     : bc_stackb (stack), bc_stacke (stack + nstack), bc_stackp (stack),
       bc_constants (constant), bc_nconstants (nconstant), bc_locals (frame),
       bc_nlocals (nframe), bc_opecode (string),
       bc_pcb (3), bc_pce (slen), bc_pc (3)
{
}

lisp
Fsi_byte_code (lisp arg, lex_env &lex)
{
  if (!consp (arg))
    FEtoo_few_arguments ();
  lisp byte_string = xcar (arg);
  check_string (byte_string);
  arg = xcdr (arg);

  lisp *vector_contents;
  int vector_length;
  if (!consp (arg))
    {
      vector_contents = 0;
      vector_length = 0;
    }
  else
    {
      if (consp (xcdr (arg)))
        FEtoo_many_arguments ();
      lisp v = xcar (arg);
      check_general_vector (v);
      vector_contents = xvector_contents (v);
      vector_length = xvector_length (v);
    }

  int slen = xstring_length (byte_string);
  if (slen < 3)
    FEinvalid_byte_code ();
  Char *string = xstring_contents (byte_string);
  int nstack_frame = string[0];
  int stack_depth = string[1];
  int nargs = string[2];

  int n = nstack_frame + stack_depth;
  lisp *stack = (lisp *)alloca (sizeof (lisp) * n);

  assert (nargs <= nstack_frame);

  lisp e = lex.lex_var;
  int i;
  for (i = 0; i < nargs; i++, e = xcdr (e))
    {
      assert (consp (e));
      if (!consp (e))
        FEinvalid_byte_code ();
      lisp x = xcar (e);
      assert (consp (x));
      stack[i] = xcdr (x);
    }
  for (; i < n; i++)
    stack[i] = Qnil;

  protect_gc gcpro (stack, n);

  ByteCode bc (stack, nstack_frame,
               stack + nstack_frame, stack_depth,
               vector_contents, vector_length,
               string, slen);
  bc.process (lex, 3, slen);
  assert (bc.stack_depth () == 1);
  return bc.pop ();
}
