#include "stdafx.h"
#include "ed.h"
#include "signal.h"

#ifdef DEBUG
static void check_condition_def ();
#endif

lisp
define_condition (lisp name, lisp var, lisp include, lisp report,
                  int print, int nslots, ...)
{
  assert (include == Qnil || struct_def_p (include));
  int skip = include != Qnil ? xstrdef_nslots (include) : 0;
  int total_slots = skip + nslots;
  lisp def = make_struct_def ();
  xstrdef_name (def) = name;
  xstrdef_type (def) = Qnil;
  xstrdef_includes (def) = (include == Qnil
                            ? Qnil
                            : xcons (include, xstrdef_includes (include)));
  xstrdef_constructors (def) = Qnil;
  xstrdef_print_function (def) = Qnil;
  if (symbolp (report) && functionp (xsymbol_function (report)))
    Funexport (report, xsymbol_package (report));
  if (report == Qnil && include != Qnil)
    report = xstrdef_report (include);
  xstrdef_report (def) = report;
  xstrdef_nslots (def) = total_slots;
  xstrdef_named_p (def) = 0;
  xstrdef_read_only_p (def) = 1;
  xstrdef_important_p (def) = print;

  xstrdef_slotdesc (def) = (struct_slotdesc *)xmalloc (sizeof (struct_slotdesc)
                                                       * total_slots);
  if (skip)
    memcpy (xstrdef_slotdesc (def), xstrdef_slotdesc (include),
            sizeof (struct_slotdesc) * skip);

  va_list ap;
  va_start (ap, nslots);
  for (int i = skip; i < total_slots; i++)
    {
      xstrdef_slotdesc (def) [i].name = va_arg (ap, lisp);
      xstrdef_slotdesc (def) [i].default_init = Qnil;
      xstrdef_slotdesc (def) [i].type = Qt;
      xstrdef_slotdesc (def) [i].read_only = Qnil;
      xstrdef_slotdesc (def) [i].offset = Qnil;
    }
  va_end (ap);

  assert (xsymbol_value (var) == Qunbound);
  xsymbol_value (var) = def;

  assert (Fget (name, Qsi_structure_definition, Qnil) == Qnil);
  Fsi_putprop (name, def, Qsi_structure_definition);

  return def;
}

#define D0(name, include, report, msgbox) \
  define_condition (CONCAT (Q, name), CONCAT (QC, name), xsymbol_value (include), \
                    (report), (msgbox), 0)
#define D1(name, include, report, msgbox, a1) \
  define_condition (CONCAT (Q, name), CONCAT (QC, name), xsymbol_value (include), \
                    (report), (msgbox), 1, (a1))
#define D2(name, include, report, msgbox, a1, a2) \
  define_condition (CONCAT (Q, name), CONCAT (QC, name), xsymbol_value (include), \
                    (report), (msgbox), 2, (a1), (a2))
#define D3(name, include, report, msgbox, a1, a2, a3) \
  define_condition (CONCAT (Q, name), CONCAT (QC, name), xsymbol_value (include), \
                    (report), (msgbox), 3, (a1), (a2), (a3))
#define D4(name, include, report, msgbox, a1, a2, a3, a4) \
  define_condition (CONCAT (Q, name), CONCAT (QC, name), xsymbol_value (include), \
                    (report), (msgbox), 4, (a1), (a2), (a3), (a4))
#define E0(name, include, report, msgbox) \
  D0 (name, include, make_message (report), msgbox)
#define E1(name, include, report, msgbox, a1) \
  D1 (name, include, make_message (report), msgbox, a1)
#define E2(name, include, report, msgbox, a1, a2) \
  D2 (name, include, make_message (report), msgbox, a1, a2)
#define E3(name, include, report, msgbox, a1, a2, a3) \
  D3 (name, include, make_message (report), msgbox, a1, a2, a3)
#define E4(name, include, report, msgbox, a1, a2, a3, a4) \
  D4 (name, include, make_message (report), msgbox, a1, a2, a3, a4)

void
init_condition ()
{
  D0 (condition, Qnil, Qnil, 1);
  D2 (  simple_condition, QCcondition, Ssi_report_simple_condition, 1, Kformat_string, Kformat_arguments);
  D0 (  serious_condition, QCcondition, Qnil, 1);
  D0 (    error, QCserious_condition, Qnil, 1);
  D2 (      simple_error, QCerror, Ssi_report_simple_condition, 1, Kformat_string, Kformat_arguments);
  D0 (        plain_error, QCsimple_error, Qnil, 0);
  D2 (      arithmetic_error, QCerror, Qnil, 1, Koperation, Koperands);
  E0 (        division_by_zero, QCarithmetic_error, Edivision_by_zero, 1);
  E0 (        floating_point_overflow, QCarithmetic_error, Efloating_point_overflow, 1);
  E0 (        floating_point_underflow, QCarithmetic_error, Efloating_point_underflow, 1);
  E0 (        domain_error, QCarithmetic_error, Earithmetic_domain_error, 1);
  E0 (        bignum_overflow, QCarithmetic_error, Ebignum_overflow, 1),
  E0 (        power_number_too_large, QCarithmetic_error, Epower_number_too_large, 1),
  D1 (      cell_error, QCerror, Qnil, 1, Kname);
  E0 (        unbound_variable, QCcell_error, Eunbound_variable, 1);
  E0 (        modify_constant, QCcell_error, Emodify_constant, 1);
  E0 (        undefined_function, QCcell_error, Eundefined_function, 1);
  D0 (      control_error, QCerror, Qnil, 1);
  D2 (        target_missing, QCcontrol_error, Ssi_report_target_missing, 1, Koperation, Ktarget);
  D2 (      file_error, QCerror, Ssi_report_file_error, 1, Kpathname, Kdatum);
  D0 (        file_not_found, QCfile_error, Qnil, 1);
  D0 (        path_not_found, QCfile_error, Qnil, 1);
  D0 (        access_denied, QCfile_error, Qnil, 1);
  D0 (        invalid_drive, QCfile_error, Qnil, 1);
  D0 (        current_directory, QCfile_error, Qnil, 1);
  D0 (        not_same_device, QCfile_error, Qnil, 1);
  D0 (        write_protected, QCfile_error, Qnil, 1);
  D0 (        bad_unit, QCfile_error, Qnil, 1);
  D0 (        device_not_ready, QCfile_error, Qnil, 1);
  D0 (        sharing_violation, QCfile_error, Qnil, 1);
  D0 (        lock_violation, QCfile_error, Qnil, 1);
  D0 (        wrong_disk, QCfile_error, Qnil, 1);
  D0 (        file_exists, QCfile_error, Qnil, 1);
  D0 (        not_empty, QCfile_error, Qnil, 1);
  D0 (        archiver_error, QCfile_error, Qnil, 1);
  D0 (        network_error, QCfile_error, Qnil, 1);
  D1 (        file_lost_error, QCfile_error, Ssi_report_file_lost_error, 1, Klost_pathname);
  D1 (      package_error, QCerror, Qnil, 1, Kpackage);
  D2 (        simple_package_error, QCpackage_error, Ssi_report_simple_package_error, 1, Kdatum1, Kdatum2);
  D0 (      program_error, QCerror, Qnil, 1);
  D2 (        simple_program_error, QCprogram_error, Ssi_report_simple_condition, 1, Kformat_string, Kformat_arguments);
  D2 (        format_error, QCprogram_error, Ssi_report_simple_condition, 1, Kformat_string, Kformat_arguments);
  D2 (        no_target, QCprogram_error, Ssi_report_no_target_for, 1, Koperation, Ktarget);
  E1 (        bad_macro_form, QCprogram_error, Ebad_macro_form, 1, Kdatum);
  E1 (        invalid_function, QCprogram_error, Einvalid_function, 1, Kdatum);
  E1 (        invalid_variable_list, QCprogram_error, Einvalid_variable_list, 1, Kdatum);
  E1 (        invalid_lambda_list, QCprogram_error, Einvalid_lambda_list, 1, Kdatum);
  E1 (        invalid_keyword_list, QCprogram_error, Einvalid_keyword_list, 1, Kdatum);
  E2 (      type_error, QCerror, Etype_error, 1, Kdatum, Kexpected_type);
  E1 (      range_error, QCerror, Erange_error, 1, Kdatum);
  D1 (      stream_error, QCerror, Qnil, 1, Kstream);
  E0 (        end_of_file, QCstream_error, Eend_of_file, 1);
  D4 (      reader_error, QCerror, Ssi_report_reader_error, 1, Kstream, Klinenum, Kdatum, Karguments);
  E1 (      too_few_arguments, QCerror, Etoo_few_arguments, 1, Kdatum);
  E1 (      too_many_arguments, QCerror, Etoo_many_arguments, 1, Kdatum);
  E1 (      bad_type_specifier, QCerror, Ebad_type_specifier, 1, Kdatum);
  E0 (      read_only_buffer, QCerror, Eread_only_buffer, 1);
  E1 (      dde_error, QCerror, Edde_error, 1, Kdatum);
  E0 (        dde_timeout, QCdde_error, Edde_timeout, 1);
  E0 (        dde_busy, QCdde_error, Edde_busy, 1);
  E0 (        dde_low_memory, QCdde_error, Edde_low_memory, 1);
  E0 (        dde_no_conv, QCdde_error, Edde_no_conv, 1);
  E0 (        dde_not_processed, QCdde_error, Edde_not_processed, 1);
  E0 (        dde_server_died, QCdde_error, Edde_server_died, 1);
  E0 (        dde_terminated_transaction, QCdde_error, Edde_terminated_transaction, 1);
  D2 (      socket_error, QCerror, Ssi_report_socket_error, 1, Kdatum, Koperation);
  E0 (    storage_condition, QCserious_condition, Estorage_condition, 1);
  E0 (    stack_overflow, QCserious_condition, Estack_overflow, 1);
  D3 (    win32_exception, QCserious_condition, Ssi_report_win32_exception, 1, Kdescription, Kcode, Kaddress);
  E0 (    invalid_byte_code, QCserious_condition, Einvalid_byte_code, 1);
  E0 (  quit, QCcondition, Equit, 0);
  E0 (    silent_quit, QCquit, Esilent_quit, 0);
  D0 (  warning, QCcondition, Qnil, 1);
  D2 (    simple_warning, QCwarning, Ssi_report_simple_condition, 0, Kformat_string, Kformat_arguments);

#undef D0
#undef D1
#undef D2
#undef D3
#undef D4
#undef E0
#undef E1
#undef E2
#undef E3
#undef E4

#define C1(name) \
  (xsymbol_value (CONCAT (Vierror_, name)) = \
   Fsi_make_structure_data (xsymbol_value (CONCAT (QC, name))))
#ifdef DEBUG
# define C(name) \
  (assert (!xstrdef_nslots (xsymbol_value (CONCAT (QC, name)))), \
   C1 (name))
#else
# define C C1
#endif
  C (storage_condition);
  C (quit);
  C (silent_quit);
  C (read_only_buffer);
#undef C
#undef C1

#ifdef DEBUG
  check_condition_def ();
#endif
}

static int
subtypep (lisp type1, lisp type2)
{
  if (consp (type2))
    {
      QUIT;
      lisp s = xcar (type2);
      type2 = xcdr (type2);
      if (s == Qor)
        {
          for (; consp (type2); type2 = xcdr (type2))
            if (subtypep (type1, xcar (type2)))
              return 1;
          return 0;
        }
      if (s == Qand)
        {
          for (; consp (type2); type2 = xcdr (type2))
            if (!subtypep (type1, xcar (type2)))
              return 0;
          return 1;
        }
      if (s == Qnot)
        {
          if (!consp (type2))
            return 0;
          return !subtypep (type1, xcar (type2));
        }
      return 0;
    }

  if (symbolp (type2))
    type2 = Fget (type2, Qsi_structure_definition, Qnil);
  return Fsi_structure_subtypep (type1, type2) != Qnil;
}

void
check_condition (lisp cc)
{
  if (!struct_data_p (cc)
      || Fsi_structure_subtypep (xstrdata_def (cc),
                                 xsymbol_value (QCcondition)) == Qnil)
    FEtype_error (cc, Qcondition);
}

lisp
Fsi_throw_error (lisp cc)
{
  check_condition (cc);
  protect_gc gcpro (cc);

  multiple_value::clear ();

  if (xsymbol_value (Vsi_trace_on_error) != Qnil)
    {
      static int in_stack_trace;
      if (!in_stack_trace)
        {
          in_stack_trace = 1;
          try
            {
              print_stack_trace (xsymbol_value (Verror_output), cc);
            }
          catch (nonlocal_jump &)
            {
            }
          in_stack_trace = 0;
        }
    }

  for (lisp p = xsymbol_value (Vsi_condition_handlers); consp (p); p = xcdr (p))
    for (lisp handlers = xcar (p); consp (handlers); handlers = xcdr (handlers))
      {
        lisp x = xcar (handlers);
        if (consp (x) && subtypep (xstrdata_def (cc), xcar (x)))
          {
            dynamic_bind dynb (Vsi_condition_handlers, xcdr (p));
            protect_gc gcpro1 (p);
            protect_gc gcpro2 (handlers);
            protect_gc gcpro3 (x);
            funcall_1 (xcdr (x), cc);
          }
      }

  if (Fsi_structure_subtypep (xstrdata_def (cc), xsymbol_value (QCserious_condition)) != Qnil
      || Fsi_structure_subtypep (xstrdata_def (cc), xsymbol_value (QCquit)) != Qnil)
    {
      multiple_value::clear ();
      nonlocal_data *d = nonlocal_jump::data ();
      d->type = Qtoplevel;
      d->value = Qnil;
      d->tag = Qnil;
      d->id = cc;
      throw nonlocal_jump ();
    }

  if (Fsi_structure_subtypep (xstrdata_def (cc), xsymbol_value (QCwarning)) != Qnil)
    Fsi_print_condition (cc);

  multiple_value::clear ();
  return Qnil;
}

// override report-functions in lisp

static lisp
report_raw (lisp c, lisp s)
{
  dynamic_bind (Vprint_escape, Qt);
  write_object (c, s, Qnil);
  return Qnil;
}

lisp
Fsi_report_reader_error (lisp c, lisp s)
{
  return report_raw (c, s);
}

lisp
Fsi_report_no_target_for (lisp c, lisp s)
{
  return report_raw (c, s);
}

lisp
Fsi_report_file_lost_error (lisp c, lisp s)
{
  return report_raw (c, s);
}

lisp
Fsi_report_file_error (lisp c, lisp s)
{
  return report_raw (c, s);
}

lisp
Fsi_report_target_missing (lisp c, lisp s)
{
  return report_raw (c, s);
}

lisp
Fsi_report_simple_condition (lisp c, lisp s)
{
  return report_raw (c, s);
}

lisp
Fsi_report_simple_package_error (lisp c, lisp s)
{
  return report_raw (c, s);
}

lisp
Fsi_report_socket_error (lisp c, lisp s)
{
  return report_raw (c, s);
}

lisp
Fsi_report_win32_exception (lisp c, lisp s)
{
  return report_raw (c, s);
}

void
handle_quit ()
{
  xsymbol_value (Vquit_flag) = Qnil;
  xsymbol_value (Vinhibit_quit) = Qnil;
  FEquit ();
}

static lisp
make_condition (lisp def, int nargs, ...)
{
  lisp c = Fsi_make_structure_data (def);
  assert (xstrdata_nslots (c) == nargs);
  va_list ap;
  va_start (ap, nargs);
  for (int i = 0; i < nargs; i++)
    xstrdata_data (c) [i] = va_arg (ap, lisp);
  va_end (ap);
  return c;
}

#define COND0(name) \
  return Fsi_throw_error (make_condition (xsymbol_value (CONCAT (QC, name)), 0))
#define COND1(name, a1) \
  return Fsi_throw_error (make_condition (xsymbol_value (CONCAT (QC, name)), 1, a1))
#define COND2(name, a1, a2) \
  return Fsi_throw_error (make_condition (xsymbol_value (CONCAT (QC, name)), 2, a1, a2))
#define COND3(name, a1, a2, a3) \
  return Fsi_throw_error (make_condition (xsymbol_value (CONCAT (QC, name)), 3, a1, a2, a3))
#define COND4(name, a1, a2, a3, a4) \
  return Fsi_throw_error (make_condition (xsymbol_value (CONCAT (QC, name)), 4, a1, a2, a3, a4))

#define SCOND(name) \
  return Fsi_throw_error (xsymbol_value (CONCAT (Vierror_, name)))
#define VCOND(name) \
  Fsi_throw_error (xsymbol_value (CONCAT (Vierror_, name)))

void
FEstorage_error ()
{
  VCOND (storage_condition);
}

lisp
FEstack_overflow ()
{
  COND0 (stack_overflow);
}

lisp
FEwin32_exception (const char* desc, u_int code, PVOID address)
{
  lisp ldesc = make_string (desc);
  lisp lcode = make_integer (static_cast <u_long> (code));
  lisp laddress = make_integer (reinterpret_cast <u_long> (address));
  COND3 (win32_exception, ldesc, lcode, laddress);
}

lisp
FEquit ()
{
  SCOND (quit);
}

lisp
FEsilent_quit ()
{
  SCOND (silent_quit);
}

lisp
FEread_only_buffer ()
{
  SCOND (read_only_buffer);
}

lisp
FEtype_error (lisp datum, lisp expected)
{
  COND2 (type_error, datum, expected);
}

lisp
FErange_error (lisp datum)
{
  COND1 (range_error, datum);
}

lisp
FEsimple_error (message_code e)
{
  COND2 (simple_error, make_message (e), Quninitialized);
}

lisp
FEsimple_error (message_code e, lisp args)
{
  COND2 (simple_error, make_message (e), args);
}

lisp
FEplain_error (message_code e)
{
  COND2 (plain_error, make_message (e), Quninitialized);
}

lisp
FEbad_type_specifier (lisp typespec)
{
  COND1 (bad_type_specifier, typespec);
}

lisp
FEmodify_constant (lisp name)
{
  COND1 (modify_constant, name);
}

lisp
FEunbound_variable (lisp name)
{
  COND1 (unbound_variable, name);
}

lisp
FEundefined_function (lisp name)
{
  COND1 (undefined_function, name);
}

lisp
FEno_target_for (lisp ope, lisp tag)
{
  COND2 (no_target, ope, tag);
}

lisp
FEtarget_missing (lisp ope, lisp tag)
{
  COND2 (target_missing, ope, tag);
}

lisp
FEinvalid_byte_code ()
{
  COND0 (invalid_byte_code);
}

lisp
FEinvalid_function (lisp fn)
{
  COND1 (invalid_function, fn);
}

lisp
FEbad_macro_form (lisp form)
{
  COND1 (bad_macro_form, form);
}

lisp
FEinvalid_variable_list (lisp l)
{
  COND1 (invalid_variable_list, l);
}

lisp
FEinvalid_lambda_list (lisp l)
{
  COND1 (invalid_lambda_list, l);
}

lisp
FEinvalid_keyword_list (lisp l)
{
  COND1 (invalid_keyword_list, l);
}

lisp
FEprogram_error (message_code e)
{
  COND2 (simple_program_error, make_message (e), Quninitialized);
}

lisp
FEprogram_error (message_code e, lisp args)
{
  COND2 (simple_program_error, make_message (e), args);
}

lisp
FEformat_error (message_code e)
{
  COND2 (format_error, make_message (e), Quninitialized);
}

static void
opearg (lisp &ope, lisp &arg)
{
  stack_trace *p = stack_trace::stp;
  if (p && (p->type == stack_trace::apply
            || p->type == stack_trace::macro
            || p->type == stack_trace::special_form))
    {
      ope = p->fn;
      arg = (p->args[0]
             ? (p->args[1]
                ? xcons (p->args[0], xcons (p->args[1], Qnil))
                : xcons (p->args[0], Qnil))
             : p->args[1]);
    }
  else
    ope = arg = Quninitialized;
}

lisp
FEtoo_few_arguments ()
{
  lisp ope, arg;
  opearg (ope, arg);
  COND1 (too_few_arguments, xcons (ope, arg));
}

lisp
FEtoo_many_arguments ()
{
  lisp ope, arg;
  opearg (ope, arg);
  COND1 (too_many_arguments, xcons (ope, arg));
}

lisp
FEdivision_by_zero ()
{
  lisp ope, arg;
  opearg (ope, arg);
  COND2 (division_by_zero, ope, arg);
}

lisp
FEfloating_point_overflow ()
{
  lisp ope, arg;
  opearg (ope, arg);
  COND2 (floating_point_overflow, ope, arg);
}

lisp
FEfloating_point_underflow ()
{
  lisp ope, arg;
  opearg (ope, arg);
  COND2 (floating_point_underflow, ope, arg);
}

lisp
FElog_domain_error ()
{
  lisp ope, arg;
  opearg (ope, arg);
  COND2 (domain_error, ope, arg);
}

lisp
FEbignum_overflow ()
{
  lisp ope, arg;
  opearg (ope, arg);
  COND2 (bignum_overflow, ope, arg);
}

lisp
FEpower_number_too_large ()
{
  lisp ope, arg;
  opearg (ope, arg);
  COND2 (power_number_too_large, ope, arg);
}

lisp
FEdde_timeout ()
{
  COND1 (dde_timeout, Quninitialized);
}

lisp
FEdde_busy ()
{
  COND1 (dde_busy, Quninitialized);
}

lisp
FEdde_low_memory ()
{
  COND1 (dde_low_memory, Quninitialized);
}

lisp
FEdde_no_conv ()
{
  COND1 (dde_no_conv, Quninitialized);
}

lisp
FEdde_not_processed ()
{
  COND1 (dde_not_processed, Quninitialized);
}

lisp
FEdde_server_died ()
{
  COND1 (dde_server_died, Quninitialized);
}

lisp
FEdde_error (lisp v)
{
  COND1 (dde_error, v);
}

lisp
FEdde_terminated_transaction ()
{
  COND1 (dde_terminated_transaction, Quninitialized);
}

lisp
FEnetwork_error (lisp n, lisp v)
{
  COND2 (network_error, n, v);
}

lisp
FEsimple_package_error (lisp p, message_code e, lisp s)
{
  COND3 (simple_package_error, p, make_message (e), s);
}

lisp
FEfile_lost_error (lisp path, lisp lpath)
{
  COND3 (file_lost_error, path, Quninitialized, lpath);
}

lisp
FEfile_error (message_code e, lisp path)
{
  COND2 (file_error, path, make_message (e));
}

lisp
FEreader_error (lisp s, lisp l, lisp m, lisp a)
{
  COND4 (reader_error, s, l, m, a);
}

lisp
FEsimple_crtl_error (int e)
{
  COND2 (simple_error, make_error (CRTL_ERROR, e), Quninitialized);
}

lisp
FEsimple_crtl_error (int e, lisp v)
{
  COND2 (simple_error, make_error (CRTL_ERROR, e), v);
}

lisp
FEsimple_win32_error (int e)
{
  COND2 (simple_error, make_error (WIN32_ERROR, e), Quninitialized);
}

lisp
FEsimple_win32_error (int e, lisp v)
{
  COND2 (simple_error, make_error (WIN32_ERROR, e), v);
}

lisp
FEsocket_error (int e, const char *ope)
{
  if (QUITP)
    {
      xsymbol_value (Vquit_flag) = Qnil;
      xsymbol_value (Vinhibit_quit) = Qnil;
      FEquit ();
    }
  COND2 (socket_error, make_error (WIN32_ERROR, e), make_string (ope ? ope : ""));
}

lisp
FEarchiver_error (message_code e, lisp f)
{
  COND2 (archiver_error, f, make_message (e));
}

lisp
FEend_of_file (lisp s)
{
  COND1 (end_of_file, s);
}

lisp
FEwin32_file_error (lisp def, int e, lisp path)
{
  return Fsi_throw_error (make_condition (def, 2, path,
                                          make_error (WIN32_ERROR, e)));
}

lisp
FEwin32_file_error (lisp def, int e)
{
  return FEwin32_file_error (def, e, Quninitialized);
}


#ifdef DEBUG
static void
check_condition_def ()
{
  message_code e = message_code (0);
  try {FEstorage_error ();} catch (nonlocal_jump &) {}
  try {FEstack_overflow ();} catch (nonlocal_jump &) {}
  try {FEwin32_exception ("", 0, 0);} catch (nonlocal_jump &) {}
  try {FEtoo_few_arguments ();} catch (nonlocal_jump &) {}
  try {FEtoo_many_arguments ();} catch (nonlocal_jump &) {}
  try {FEquit ();} catch (nonlocal_jump &) {}
  try {FEsilent_quit ();} catch (nonlocal_jump &) {}
  try {FEread_only_buffer ();} catch (nonlocal_jump &) {}
  try {FEtype_error (Qnil, Qnil);} catch (nonlocal_jump &) {}
  try {FErange_error (Qnil);} catch (nonlocal_jump &) {}
  try {FEsimple_error (e);} catch (nonlocal_jump &) {}
  try {FEsimple_error (e, Qnil);} catch (nonlocal_jump &) {}
  try {FEplain_error (e);} catch (nonlocal_jump &) {}
  try {FEbad_type_specifier (Qnil);} catch (nonlocal_jump &) {}
  try {FEmodify_constant (Qnil);} catch (nonlocal_jump &) {}
  try {FEunbound_variable (Qnil);} catch (nonlocal_jump &) {}
  try {FEundefined_function (Qnil);} catch (nonlocal_jump &) {}
  try {FEno_target_for (Qnil, Qnil);} catch (nonlocal_jump &) {}
  try {FEtarget_missing (Qnil, Qnil);} catch (nonlocal_jump &) {}
  try {FEinvalid_byte_code ();} catch (nonlocal_jump &) {}
  try {FEinvalid_function (Qnil);} catch (nonlocal_jump &) {}
  try {FEbad_macro_form (Qnil);} catch (nonlocal_jump &) {}
  try {FEinvalid_variable_list (Qnil);} catch (nonlocal_jump &) {}
  try {FEinvalid_lambda_list (Qnil);} catch (nonlocal_jump &) {}
  try {FEinvalid_keyword_list (Qnil);} catch (nonlocal_jump &) {}
  try {FEprogram_error (e);} catch (nonlocal_jump &) {}
  try {FEprogram_error (e, Qnil);} catch (nonlocal_jump &) {}
  try {FEformat_error (e);} catch (nonlocal_jump &) {}
  try {FEdivision_by_zero ();} catch (nonlocal_jump &) {}
  try {FEfloating_point_overflow ();} catch (nonlocal_jump &) {}
  try {FEfloating_point_underflow ();} catch (nonlocal_jump &) {}
  try {FElog_domain_error ();} catch (nonlocal_jump &) {}
  try {FEbignum_overflow ();} catch (nonlocal_jump &) {}
  try {FEpower_number_too_large ();} catch (nonlocal_jump &) {}
  try {FEdde_timeout ();} catch (nonlocal_jump &) {}
  try {FEdde_busy ();} catch (nonlocal_jump &) {}
  try {FEdde_low_memory ();} catch (nonlocal_jump &) {}
  try {FEdde_no_conv ();} catch (nonlocal_jump &) {}
  try {FEdde_not_processed ();} catch (nonlocal_jump &) {}
  try {FEdde_server_died ();} catch (nonlocal_jump &) {}
  try {FEdde_error (Qnil);} catch (nonlocal_jump &) {}
  try {FEdde_terminated_transaction ();} catch (nonlocal_jump &) {}
  try {FEnetwork_error (Qnil, Qnil);} catch (nonlocal_jump &) {}
  try {FEsimple_package_error (Qnil, e, Qnil);} catch (nonlocal_jump &) {}
  try {FEfile_lost_error (Qnil, Qnil);} catch (nonlocal_jump &) {}
  try {FEfile_error (e, Qnil);} catch (nonlocal_jump &) {}
  try {FEreader_error (Qnil, Qnil, Qnil, Qnil);} catch (nonlocal_jump &) {}
  try {FEsimple_crtl_error (e);} catch (nonlocal_jump &) {}
  try {FEsimple_crtl_error (e, Qnil);} catch (nonlocal_jump &) {}
  try {FEsimple_win32_error (e);} catch (nonlocal_jump &) {}
  try {FEsimple_win32_error (e, Qnil);} catch (nonlocal_jump &) {}
  try {FEsocket_error (0, "");} catch (nonlocal_jump &) {}
  try {FEarchiver_error (e, Qnil);} catch (nonlocal_jump &) {}
  try {FEend_of_file (Qnil);} catch (nonlocal_jump &) {}
  try {FEwin32_file_error (Qnil, e, Qnil);} catch (nonlocal_jump &) {}
  try {FEwin32_file_error (Qnil, e);} catch (nonlocal_jump &) {}
}
#endif
