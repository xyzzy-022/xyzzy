// -*-C++-*-
#ifndef _syntax_h_
# define _syntax_h_

# define SYNTAX_OPT_CPP 1
# define SYNTAX_OPT_CPLUSPLUS 2
# define SYNTAX_OPT_JAVA 4
# define SYNTAX_OPT_CSHARP 8
# define SYNTAX_OPT_COLUMN_CHAR 16
# define SYNTAX_OPT_CPLUSPLUS_CLI 32

struct syntax_table
{
  u_char type[256];
  u_char match[256];
  u_char comment[256];
  int flags;
  int comment_column;
};

class lsyntax_table: public lisp_object
{
public:
  syntax_table *table;

  ~lsyntax_table () {xfree (table);}
};

# define syntax_table_p(X) typep ((X), Tsyntax_table)

inline void
check_syntax_table (lisp x)
{
  check_type (x, Tsyntax_table, Qsyntax_table);
}

inline syntax_table *&
xsyntax_table (lisp x)
{
  assert (syntax_table_p (x));
  return ((lsyntax_table *)x)->table;
}

enum syntax_code
{
  SCwhite,
  SCpunct,
  SCopen,
  SCclose,
  SCmath,
  SCstring,
  SCcomment_start,
  SCcomment_end,
  SCcplusplus_comment_end,
  SCescape,
  SCquote,
  SCsymbol,
  SCword,
  SCkana,
  SCkanji,
  SCjunk,
  SCtag_start,
  SCtag_end,
  SCsymbol_prefix,
  SCmax
};

# define Sin_string 1
# define Sin_comment 2
# define Sunmatched_comment 3
# define Sunmatched_paren 4
# define Sunbalanced_paren 5
# define Send_sexp 6
# define Seob 7
# define Sbob 7

# define SFcomment_start_first_char 1
# define SFcomment_start_second_char 2
# define SFcomment_end_first_char 4
# define SFcomment_end_second_char 8
# define SFcplusplus_comment_char 16
# define SFcolumn_comment_char 32
# define SFmaybe_comment_end 64
# define SFparse_sexp_ignore_comment 128

# define xchar_syntax(t, c) ((t)->type[(c)])
# define xchar_match(t, c) ((t)->match[(c)])
# define xchar_comment(t, c) ((t)->comment[(c)])

# define xcomment_start_first_char_p(t, c) \
  (xchar_comment ((t), (c)) & SFcomment_start_first_char)

# define xcomment_start_second_char_p(t, c) \
  (xchar_comment ((t), (c)) & SFcomment_start_second_char)

# define xcomment_end_first_char_p(t, c) \
  (xchar_comment ((t), (c)) & SFcomment_end_first_char)

# define xcomment_end_second_char_p(t, c) \
  (xchar_comment ((t), (c)) & SFcomment_end_second_char)

# define xcplusplus_comment_char_p(t, c) \
  (xchar_comment ((t), (c)) & SFcplusplus_comment_char)

# define xparse_sexp_ignore_comment_p(t, c) \
  (xchar_comment ((t), (c)) & SFparse_sexp_ignore_comment)

# define maybe_comment_end_p(t, c) \
  (xchar_comment ((t), (c)) & SFmaybe_comment_end)

# define xcolumn_comment_char_p(t, c) \
  (xchar_comment ((t), (c)) & SFcolumn_comment_char)

extern char syntax_spec_table[128];
void init_syntax_spec ();

inline lsyntax_table *
make_syntax_table ()
{
  lsyntax_table *p = ldata <lsyntax_table, Tsyntax_table>::lalloc ();
  p->table = 0;
  return p;
}

class word_state
{
public:
  enum word_category
    {
      WCword,
      WCnot_word,
      WCkana,
      WC2symbol,
      WC2alphanumeric,
      WC2hiragana,
      WC2katakana,
      WC2greek,
      WC2cyrillic,
      WC2line,
      WC2kanji,
      WC2hiragana_or_katakana,
      WC2hangul,
      WC2ipa,
      WC2georgian,
      WCfirst
    };
protected:
  struct category_range
    {
      ucs2_t from, to;
      word_category cat1, cat2;
    };
  static const category_range ws_range[];

  const syntax_table *ws_tab;
  word_category ws_last;

  word_category char_category (Char c) const
    {return char_category (ws_tab, c);}
public:
  enum
    {
      inword,
      not_inword,
      punct
    };
  int forward (Char);
  int backward (Char);
  word_state (const syntax_table *tab, Char c)
       : ws_tab (tab), ws_last (WCfirst) {ws_last = char_category (c);}
  static word_category char_category (const syntax_table *, Char);
};

#endif
