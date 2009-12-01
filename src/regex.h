#ifndef _regex_h_
# define _regex_h_

#define MAX_REGS 10

struct re_point;
class re_search;

static inline int
re_syntax_word_p (u_char c)
{
  return c == SCword;
}

static inline int
re_syntax_symbol_p (u_char c)
{
  return c == SCsymbol || c == SCword;
}

class Regexp
{
public:
  struct sregs
    {
      int nregs;
      point_t start[MAX_REGS];
      point_t end[MAX_REGS];
    };
  sregs re_regs;
protected:
  Char *re_pattern;
  int re_size;
  const u_char *re_translate;
  const syntax_table *re_syntax_table;
  int re_match_void_p;
  int re_match_bol_p;
  char re_fastmap[256];
  lisp re_object;
  int re_has_backref;
  Region re_range;
  point_t re_last_match;
  lChar re_last_match_char;

  class record_failure
    {
      enum {TABSIZE = 97};
      struct ent
        {
          ent *cdr;
          point_t point, max;
          const Char *pat;
        };
      ent *m_tab[TABSIZE];
      ent m_entbuf[512];
      ent *m_ep;
      static u_int hashval (const Char *, point_t, point_t);
    public:
      record_failure ();
      void init ();
      int find (const Char *, point_t, point_t) const;
      void add (const Char *, const re_point &);
    };
  record_failure re_failure;

  int backref (re_point &, int) const;
  int match_char_class (const Char *, Char) const;
  int match (re_point &, const Char *, const Char *);
  int match (const re_point &);
  int search (const re_search &, re_point &);
  int branch (re_point &, const Char *, const Char *);
  int closure (re_point &, const Char *, const Char *, int);
  int closure_backtrack (re_point &, const Char *, const Char *, int);
  int simple_closure (re_point &, const Char *, const Char *);
  int shortest_simple_closure (re_point &, const Char *, const Char *);
  static int compare_regs (const sregs &, const sregs &);
  void start_save_regs (int, point_t);
  void end_save_regs (int, point_t);
  void init_match (const re_point &, point_t, lChar);
  void init_match (const Buffer *, point_t, lChar);
  int bobp (const re_point &) const;
  int eobp (const re_point &) const;
  static int repeat_max (Char);
  u_char char_syntax (Char cc) const
    {
      if (cc >= 256)
        cc >>= 8;
      return xchar_syntax (re_syntax_table, cc);
    }
  int syntax_word_p (Char cc) const
    {return re_syntax_word_p (char_syntax (cc));}
  int syntax_symbol_p (Char cc) const
    {return re_syntax_symbol_p (char_syntax (cc));}

public:
  void compile (const Char *, int, int);
  void compile (lisp, int);
  int search (const Char *, int, int);
  int search (const Buffer *, const Point &, point_t, point_t, point_t, lChar);
  int search_backward (const Buffer *, const Point &, point_t, point_t, point_t, lChar);
  int match (const Char *, int, int);
  int match (const Buffer *, const Point &, point_t, point_t);
  Regexp (const u_char *translate, const syntax_table *syntax_tab)
       : re_pattern (0), re_translate (translate),
         re_syntax_table (syntax_tab), re_object (0) {}
  lisp make_regexp (lisp) const;
  ~Regexp ()
    {
      if (re_pattern && !re_object)
        free (re_pattern);
    }
  static int smart_case_fold_p (const Char *, int);
  const Region &range () const {return re_range;}
  point_t last_match () const {return re_last_match;}
  lChar last_match_char () const {return re_last_match_char;}
  static int merge_fastmap (lisp, char *, const syntax_table *);
};

#endif
