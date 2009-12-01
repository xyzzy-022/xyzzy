// -*-C++-*-
#ifndef _readtab_h_
# define _readtab_h_

# define READTABLE_REP_SIZE 128

struct dispmacro_param;

typedef lisp (*reader_macro_function)(lisp, Char);
typedef lisp (*reader_dispmacro_function)(lisp, Char, dispmacro_param &);

enum standard_char_type
{
  SCT_ILLEGAL,
  SCT_WHITESPACE,
  SCT_CONSTITUENT,
  SCT_SINGLE_ESCAPE,
  SCT_MULTIPLE_ESCAPE,
  SCT_TERM_MACRO,
  SCT_NON_TERM_MACRO
};

enum readtable_case
{
  RTC_UPCASE,
  RTC_DOWNCASE,
  RTC_PRESERVE,
  RTC_INVERT,
};

struct disptab_rep
{
  lisp lfunc;
  reader_dispmacro_function cfunc;
};

struct readtab_rep
{
  standard_char_type type;
  lisp lfunc;
  reader_macro_function cfunc;
  disptab_rep *disp;
};

class lreadtable: public lisp_object
{
public:
  readtab_rep *rep;
  readtable_case rcase;
  ~lreadtable ();
};

# define readtablep(X) typep ((X), Treadtable)

inline void
check_readtable (lisp x)
{
  check_type (x, Treadtable, Qreadtable);
}

inline readtab_rep *&
xreadtable_rep (lisp x)
{
  assert (readtablep (x));
  return ((lreadtable *)x)->rep;
}

inline readtable_case &
xreadtable_case (lisp x)
{
  assert (readtablep (x));
  return ((lreadtable *)x)->rcase;
}

# define _stdchar_type(rep, c) ((rep)[(c)].type)
# define _stdchar_type_p(rep, type, c) (_stdchar_type ((rep), (c)) == (type))

static inline standard_char_type
stdchar_type (const readtab_rep *rep, Char c)
{
  return ascii_char_p (c) ? _stdchar_type (rep, c) : SCT_CONSTITUENT;
}

static inline int
stdchar_type_p (const readtab_rep *rep, standard_char_type type, Char c)
{
  return ascii_char_p (c) && _stdchar_type_p (rep, type, c);
}

# define stdchar_illegal_p(rep, c) \
  stdchar_type_p ((rep), SCT_ILLEGAL, (c))
# define stdchar_whitespace_p(rep, c) \
  stdchar_type_p ((rep), SCT_WHITESPACE, (c))
# define stdchar_constituent_p(rep, c) \
  stdchar_type_p ((rep), SCT_CONSTITUENT, (c))
# define stdchar_single_escape_p(rep, c) \
  stdchar_type_p ((rep), SCT_SINGLE_ESCAPE, (c))
# define stdchar_multiple_escape_p(rep, c) \
  stdchar_type_p ((rep), SCT_MULTIPLE_ESCAPE, (c))
# define stdchar_terminating_macro_p(rep, c) \
  stdchar_type_p ((rep), SCT_TERM_MACRO, (c))
# define stdchar_non_terminating_macro_p(rep, c) \
  stdchar_type_p ((rep), SCT_NON_TERM_MACRO, (c))

lisp current_readtable ();
reader_dispmacro_function get_reader_dispmacro_function (lisp);
reader_macro_function get_reader_macro_function (lisp);

#endif
