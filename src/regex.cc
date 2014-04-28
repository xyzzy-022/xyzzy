#include "stdafx.h"
#include "ed.h"
#include "regex.h"

extern u_char char_no_translate_table[];
extern u_char char_translate_upcase_table[];

enum
{
  BEGLINE,
  ENDLINE,
  BEGBUF,
  ENDBUF,
  ANYCHAR,
  START_SAVE_REGS,
  END_SAVE_REGS,
  BACKREF,
  NORMAL_CHARS,
  END_BRANCH,
  BRANCH,
  BRANCH_BACKTRACK,
  CLOSURE,
  CLOSURE_BACKTRACK,
  CLOSURE_SIMPLE,
  SHORTEST_CLOSURE,
  SHORTEST_CLOSURE_BACKTRACK,
  SHORTEST_CLOSURE_SIMPLE,
  CHAR_CLASS,
  CHAR_CLASS_NOT,
  BEGWORD,
  ENDWORD,
  WORDBOUND,
  NOT_WORDBOUND,
  WORDCHAR,
  NOT_WORDCHAR,
  SYNTAX_SPEC,
  NOT_SYNTAX_SPEC,
  BEGSYMBOL,
  ENDSYMBOL,
  SYMBOLBOUND,
  NOT_SYMBOLBOUND,
  SYMBOLCHAR,
  NOT_SYMBOLCHAR
};

#define NBITS (sizeof (Char) * 8)

class charclass
{
  Char hi[256 / NBITS];
  Char lo[256][256 / NBITS];
  static void set (Char *b, int n)
    {b[n / NBITS] |= 1 << (n % NBITS);}
  static int isset (const Char *b, int n)
    {return b[n / NBITS] & (1 << (n % NBITS));}
public:
  struct cc
    {
      int nent;
      struct
        {
          u_char hi;
          u_char all;
          u_char lower;
          u_char upper;
        } f[256];
    };
  charclass () {bzero (hi, sizeof hi);}
  void set (Char c)
    {
      int h = c >> 8;
      int l = c & 255;
      if (!isset (hi, h))
        {
          set (hi, h);
          bzero (lo[h], sizeof lo[h]);
        }
      set (lo[h], l);
    }
  int count_size (cc &) const;
  Char *copy (Char *, cc &, int) const;
};

int
charclass::count_size (cc &f) const
{
  int size = 1;
  f.nent = 0;
  for (int h = 0; h < 256; h++)
    if (isset (hi, h))
      {
        int l;
        for (l = 0; l < 256 / NBITS && !lo[h][l]; l++)
          ;
        int u;
        for (u = 256 / NBITS - 1; u > l && !lo[h][u]; u--)
          ;
        u++;
        int i;
        for (i = l; i < u; i++)
          if (lo[h][i] != Char (-1))
            break;
        size += 2;
        if (i < u)
          {
            size += u - l;
            f.f[f.nent].all = 0;
          }
        else
          f.f[f.nent].all = 1;
        f.f[f.nent].lower = l;
        f.f[f.nent].upper = u;
        f.f[f.nent].hi = h;
        f.nent++;
      }
  return size;
}

Char *
charclass::copy (Char *b, cc &f, int size) const
{
  *b++ = size;
  for (int i = 0; i < f.nent; i++)
    {
      *b++ = (f.f[i].hi << 8) + f.f[i].all;
      *b++ = (f.f[i].upper << 8) + f.f[i].lower;
      if (!f.f[i].all)
        for (int j = f.f[i].lower; j < f.f[i].upper; j++)
          *b++ = lo[f.f[i].hi][j];
    }
  return b;
}

#define REPEAT_INFINITY (CHAR_LIMIT - 1)

class regexp_compile
{
  struct stack
    {
      int buf;
      int remain_branch;
      int reg;
      int branch_start;
    };
  enum {MAX_STACK_DEPTH = 10};
  stack r_stackb[MAX_STACK_DEPTH];
  stack *r_stackp;

  int r_allocated;
  Char *r_last_start;
  Char *r_branch_start;
  int r_remain_branch;
  Char *r_normal_char;
  int r_regnum;
  const u_char *r_translate;
  const syntax_table *r_syntax_table;

  Char *extend_buffer (Char *, int);
  Char *branch (Char *);
  Char *closure (Char *, int, int, int);
  static void error (message_code e) {FEsimple_error (e);}
  const Char *char_class (const Char *, const Char *, Char *&);
  void char_class_fastmap (const Char *, char *) const;
  void char_class_not_fastmap (const Char *, char *) const;
  static int check_inner_closure (Char *, Char *);
  static int check_postfix (const Char *&, const Char *);

public:
  Char *r_buffer;
  int r_used;
  int r_has_backref;

  regexp_compile (const u_char *, const syntax_table *);
  regexp_compile (const u_char *, const syntax_table *, int); // XXX
  void compile (const Char *, int);
  static int match_void_p (const Char *, const Char *);
  static int match_bol_p (const Char *, const Char *);
  int compile_fastmap (char *, const Char *, const Char *) const;
  ~regexp_compile ()
    {
      if (r_buffer)
        xfree (r_buffer);
    }

#ifdef DEBUG
  void dump () const;
  static void dump (const Char *, int);
#endif
};

regexp_compile::regexp_compile (const u_char *translate, const syntax_table *syntax_tab)
     : r_translate (translate), r_syntax_table (syntax_tab), r_buffer (0)
{
  r_allocated = 128;
  r_buffer = (Char *)xmalloc (sizeof (Char) * r_allocated);
  r_stackp = r_stackb;
  r_last_start = 0;
  r_branch_start = r_buffer;
  r_remain_branch = 0;
  r_normal_char = 0;
  r_regnum = 1;
  r_has_backref = 0;
}

regexp_compile::regexp_compile (const u_char *translate, const syntax_table *syntax_tab, int)
     : r_translate (translate), r_syntax_table (syntax_tab), r_buffer (0)
{
}

Char *
regexp_compile::extend_buffer (Char *b, int req)
{
  if (b + req > r_buffer + r_allocated)
    {
      int size = r_allocated + max (req, 64);
      Char *p = (Char *)xrealloc (r_buffer, sizeof (Char) * size);
      r_allocated = size;
      b = p + (b - r_buffer);
      r_branch_start = p + (r_branch_start - r_buffer);
      if (r_last_start)
        r_last_start = p + (r_last_start - r_buffer);
      if (r_normal_char)
        r_normal_char = p + (r_normal_char - r_buffer);
      r_buffer = p;
    }
  return b;
}

#ifdef DEBUG
void
regexp_compile::dump (const Char *p0, int size)
{
  for (const Char *p = p0, *pe = p + size; p < pe;)
    {
      printf ("%5d: ", p - p0);
      Char c = *p++;
      switch (c)
        {
        case BEGLINE:
          printf ("begline");
          break;

        case ENDLINE:
          printf ("endline");
          break;

        case BEGBUF:
          printf ("begbuf");
          break;

        case ENDBUF:
          printf ("endbuf");
          break;

        case ANYCHAR:
          printf ("anychar");
          break;

        case START_SAVE_REGS:
          printf ("start-save-regs: %d", *p++);
          break;

        case END_SAVE_REGS:
          printf ("end-save-regs: %d", *p++);
          break;

        case BACKREF:
          printf ("backref: %d", *p++);
          break;

        case NORMAL_CHARS:
          {
            printf ("normal-chars: ");
            int i;
            for (i = 1; i <= *p; i++)
              putchar (p[i]);
            p += i;
          }
          break;

        case CHAR_CLASS:
        case CHAR_CLASS_NOT:
          {
            printf (c == CHAR_CLASS ? "char-class: " : "char-class-not: ");
            const Char *p2 = p + *p;
            for (p++; p < p2;)
              {
                int u = p[1] >> 8, l = p[1] & 0xff;
                printf ("%02x(%d-%d)", *p >> 8, l, u);
                if (*p & 0xff)
                  {
                    p += 2;
                    printf ("ALL");
                  }
                else
                  p += 2 + u - l;
              }
          }
          break;

        case END_BRANCH:
          printf ("end-branch");
          break;

        case BRANCH:
          printf ("branch: %d", *p++);
          break;

        case BRANCH_BACKTRACK:
          printf ("branch-backtrack: %d", *p++);
          break;

        case CLOSURE:
          printf ("closure: ");
          printf ("min: %d ", *p++);
          printf ("max: %d ", *p++);
          printf ("start: %d ", *p++);
          break;

        case CLOSURE_BACKTRACK:
          printf ("closure-backtrack: ");
          printf ("min: %d ", *p++);
          printf ("max: %d ", *p++);
          printf ("start: %d ", *p++);
          break;

        case CLOSURE_SIMPLE:
          printf ("closure-simple: ");
          printf ("min: %d ", *p++);
          printf ("max: %d ", *p++);
          printf ("start: %d ", *p++);
          break;

        case SHORTEST_CLOSURE:
          printf ("shortest-closure: ");
          printf ("min: %d ", *p++);
          printf ("max: %d ", *p++);
          printf ("start: %d ", *p++);
          break;

        case SHORTEST_CLOSURE_BACKTRACK:
          printf ("shortest-closure-backtrack: ");
          printf ("min: %d ", *p++);
          printf ("max: %d ", *p++);
          printf ("start: %d ", *p++);
          break;

        case SHORTEST_CLOSURE_SIMPLE:
          printf ("shortest-closure-simple: ");
          printf ("min: %d ", *p++);
          printf ("max: %d ", *p++);
          printf ("start: %d ", *p++);
          break;

        case BEGWORD:
          printf ("begword\n");
          break;

        case ENDWORD:
          printf ("endword\n");
          break;

        case WORDBOUND:
          printf ("word-bound\n");
          break;

        case NOT_WORDBOUND:
          printf ("not-word-bound\n");
          break;

        case WORDCHAR:
          printf ("word-char\n");
          break;

        case NOT_WORDCHAR:
          printf ("not-word-char\n");
          break;

        case SYNTAX_SPEC:
          printf ("syntax-spec: %d\n", *p++);
          break;

        case NOT_SYNTAX_SPEC:
          printf ("not-syntax-spec: %d\n", *p++);
          break;

        case BEGSYMBOL:
          printf ("begsymbol\n");
          break;

        case ENDSYMBOL:
          printf ("endsymbol\n");
          break;

        case SYMBOLBOUND:
          printf ("symbol-bound\n");
          break;

        case NOT_SYMBOLBOUND:
          printf ("not-symbol-bound\n");
          break;

        case SYMBOLCHAR:
          printf ("symbol-char\n");
          break;

        case NOT_SYMBOLCHAR:
          printf ("not-symbol-char\n");
          break;

        default:
          printf ("UNKNOWN CODE: %d", c);
          break;
        }
      putchar ('\n');
    }
  fflush (stdout);
}

void
regexp_compile::dump () const
{
  //dump (r_buffer, r_used);
}
#endif /* DEBUG */

int
regexp_compile::check_inner_closure (Char *p, Char *pe)
{
  int n = 0;
  while (p < pe)
    {
      switch (*p++)
        {
        case BEGLINE:
        case ENDLINE:
        case BEGBUF:
        case ENDBUF:
        case ANYCHAR:
        case BEGWORD:
        case ENDWORD:
        case WORDBOUND:
        case NOT_WORDBOUND:
        case WORDCHAR:
        case NOT_WORDCHAR:
        case END_BRANCH:
        case BEGSYMBOL:
        case ENDSYMBOL:
        case SYMBOLBOUND:
        case NOT_SYMBOLBOUND:
        case SYMBOLCHAR:
        case NOT_SYMBOLCHAR:
          break;

        case NORMAL_CHARS:
          p += *p + 1;
          break;

        case SYNTAX_SPEC:
        case NOT_SYNTAX_SPEC:
        case START_SAVE_REGS:
        case END_SAVE_REGS:
        case BACKREF:
          p++;
          break;

        case CHAR_CLASS:
        case CHAR_CLASS_NOT:
          p += *p;
          break;

        case BRANCH:
        case BRANCH_BACKTRACK:
          do
            {
              if (check_inner_closure (p + 1, p + *p - 1))
                p[-1] = BRANCH_BACKTRACK;
              p += *p - 1;
            }
          while (*p++ != END_BRANCH);
          n = 1;
          break;

        case CLOSURE:
        case CLOSURE_BACKTRACK:
        case CLOSURE_SIMPLE:
          if (check_inner_closure (p + 3, p + p[2] - 1))
            p[-1] = CLOSURE_BACKTRACK;
          p += p[2] - 1;
          n = 1;
          break;

        case SHORTEST_CLOSURE:
        case SHORTEST_CLOSURE_BACKTRACK:
        case SHORTEST_CLOSURE_SIMPLE:
          if (check_inner_closure (p + 3, p + p[2] - 1))
            p[-1] = SHORTEST_CLOSURE_BACKTRACK;
          p += p[2] - 1;
          n = 1;
          break;
        }
    }
  return n;
}

Char *
regexp_compile::branch (Char *b)
{
  Char *p = r_branch_start;
  memmove (p + 2, p, sizeof (Char) * (b - p));
  b += 2;
  if (b - p >= CHAR_LIMIT)
    error (Eregexp_too_long);
  p[0] = BRANCH;
  p[1] = b - p;
  r_normal_char = 0;
  return b;
}

Char *
regexp_compile::closure (Char *b, int min, int max, int shortest)
{
  Char *p = r_last_start;
  memmove (p + 4, p, sizeof (Char) * (b - p));
  b += 4;
  Char *endp;
  switch (p[4])
    {
    case NORMAL_CHARS:
      endp = p[5] == 1 ? &p[6] + p[5] : 0;
      break;

    case ANYCHAR:
    case WORDCHAR:
    case NOT_WORDCHAR:
    case SYMBOLCHAR:
    case NOT_SYMBOLCHAR:
      endp = &p[4] + 1;
      break;

    case SYNTAX_SPEC:
    case NOT_SYNTAX_SPEC:
      endp = &p[4] + 2;
      break;

    case CHAR_CLASS:
    case CHAR_CLASS_NOT:
      endp = &p[5] + p[5];
      break;

    default:
      endp = 0;
      break;
    }

  if (endp == b)
    p[0] = shortest ? SHORTEST_CLOSURE_SIMPLE : CLOSURE_SIMPLE;
  else
    p[0] = shortest ? SHORTEST_CLOSURE : CLOSURE;
  if (b - p >= CHAR_LIMIT)
    error (Eregexp_too_long);
  p[1] = min;
  p[2] = max;
  p[3] = b - p;
  r_normal_char = 0;
  return b;
}

const Char *
regexp_compile::char_class (const Char *p, const Char *pe, Char *&b)
{
  r_last_start = b;
  if (p == pe)
    error (Eunmatched_bracket);
  if (*p == '^')
    {
      *b++ = CHAR_CLASS_NOT;
      p++;
    }
  else
    *b++ = CHAR_CLASS;

  charclass ccl;

  const Char *p0 = p + 1;
  while (1)
    {
      if (p == pe)
        error (Eunmatched_bracket);
      lChar c = *p++;
      if (c == ']' && p != p0)
        break;
      if (p < pe - 1 && *p == '-' && p[1] != ']')
        {
          lChar c2 = p[1];
          p += 2;
          for (; c <= c2; c++)
            if (ascii_char_p (c))
              ccl.set (Char (r_translate[c]));
            else
              ccl.set (Char (c));
        }
      else
        if (ascii_char_p (c))
          ccl.set (Char (r_translate[c]));
        else
          ccl.set (Char (c));
    }

  charclass::cc f;
  int size = ccl.count_size (f);
  b = extend_buffer (b, size);
  Char *b2 = ccl.copy (b, f, size);
  assert (b2 == b + size);
  b = b2;
  return p;
}

inline int
regexp_compile::check_postfix (const Char *&p, const Char *pe)
{
  if (p == pe || *p != '?')
    return 0;
  p++;
  return 1;
}

void
regexp_compile::compile (const Char *pattern, int size)
{
  const Char *p = pattern, *pe = p + size;
  Char *b = r_buffer;

  while (p < pe)
    {
      b = extend_buffer (b, 32);
      Char c = *p++;
      switch (c)
        {
        case '^':
          if (r_last_start)
            goto normal_char;
          *b++ = BEGLINE;
          break;

        case '$':
          if (p == pe || (p < pe - 1 && *p == '\\' && (p[1] == '|' || p[1] == ')')))
            *b++ = ENDLINE;
          else
            goto normal_char;
          break;

        case '.':
          r_last_start = b;
          *b++ = ANYCHAR;
          break;

        case '[':
          p = char_class (p, pe, b);
          break;

        case '*':
          if (!r_last_start)
            goto normal_char;
          b = closure (b, 0, REPEAT_INFINITY, check_postfix (p, pe));
          break;

        case '+':
          if (!r_last_start)
            goto normal_char;
          b = closure (b, 1, REPEAT_INFINITY, check_postfix (p, pe));
          break;

        case '?':
          if (!r_last_start)
            goto normal_char;
          b = closure (b, 0, 1, check_postfix (p, pe));
          break;

        case '\\':
          if (p == pe)
            error (Ere_invalid_pattern);
          c = *p++;
          switch (c)
            {
            case '{':
              if (!r_last_start)
                goto normal_char;
              else
                {
                  /*
	            {M} - x == M
	            {M,} - x >= M
	            {,M} - 0 <= x <= M
	            {M,N} - M <= x <= N */
                  int minrep, maxrep;
                  const Char *op = p;
                  minrep = 0;
                  while (1)
                    {
                      if (p == pe)
                        error (Ere_unmatched_lbrace);
                      c = *p++;
                      if (c < '0' || c > '9')
                        break;
                      minrep = minrep * 10 + c - '0';
                    }
                  if (c == ',')
                    {
                      const Char *op2 = p;
                      maxrep = 0;
                      while (1)
                        {
                          if (p == pe)
                            error (Ere_unmatched_lbrace);
                          c = *p++;
                          if (c < '0' || c > '9')
                            break;
                          maxrep = maxrep * 10 + c - '0';
                          maxrep = min (maxrep, REPEAT_INFINITY);
                        }
                      if (p == op2 + 1)
                        {
                          if (p == op + 2)
                            error (Ere_malformed_repeat_count);
                          maxrep = REPEAT_INFINITY;
                        }
                    }
                  else
                    maxrep = minrep;
                  if (p == op + 1 || c != '\\' || p == pe || *p++ != '}')
                    error (Ere_malformed_repeat_count);
                  b = closure (b, minrep, maxrep, check_postfix (p, pe));
                }
              break;

            case '(':
              if (r_stackp == r_stackb + MAX_STACK_DEPTH)
                error (Ere_nesting_too_deep);
              r_stackp->buf = b - r_buffer;
              r_stackp->remain_branch = r_remain_branch;
              r_stackp->branch_start = r_branch_start - r_buffer;
              if (p + 2 <= pe && *p == '?' && p[1] == ':')
                {
                  r_stackp->reg = MAX_REGS;
                  p += 2;
                }
              else
                {
                  r_stackp->reg = r_regnum;
                  if (r_regnum < MAX_REGS)
                    {
                      *b++ = START_SAVE_REGS;
                      *b++ = r_regnum;
                    }
                  r_regnum++;
                }
              r_stackp++;
              r_remain_branch = 0;
              r_last_start = 0;
              r_branch_start = b;
              r_normal_char = 0;
              break;

            case ')':
              if (r_stackp == r_stackb)
                error (Ere_unmatched_rparen);
              if (r_remain_branch)
                {
                  b = branch (b);
                  *b++ = END_BRANCH;
                }
              r_stackp--;
              r_last_start = r_buffer + r_stackp->buf;
              r_remain_branch = r_stackp->remain_branch;
              r_branch_start = r_buffer + r_stackp->branch_start;
              if (r_stackp->reg < MAX_REGS)
                {
                  *b++ = END_SAVE_REGS;
                  *b++ = r_stackp->reg;
                }
              break;

            case '|':
              *b++ = END_BRANCH;
              b = branch (b);
              r_branch_start = b;
              r_remain_branch = 1;
              r_last_start = 0;
              break;

            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
              {
                int n = c - '0';
                if (n >= r_regnum)
                  goto normal_char;
                for (stack *p = r_stackb; p < r_stackp; p++)
                  if (p->reg == n)
                    goto normal_char;
                r_last_start = b;
                *b++ = BACKREF;
                *b++ = n;
                r_has_backref = 1;
              }
              break;

            case '`':
              if (r_last_start)
                goto normal_char;
              *b++ = BEGBUF;
              break;

            case '\'':
              if (p == pe || (p < pe - 1 && *p == '\\' && (p[1] == '|' || p[1] == ')')))
                *b++ = ENDBUF;
              else
                goto normal_char;
              break;

            case '<':
              *b++ = BEGWORD;
              break;

            case '>':
              *b++ = ENDWORD;
              break;

            case 'b':
              *b++ = WORDBOUND;
              break;

            case 'B':
              *b++ = NOT_WORDBOUND;
              break;

            case 'w':
              r_last_start = b;
              *b++ = WORDCHAR;
              break;

            case 'W':
              r_last_start = b;
              *b++ = NOT_WORDCHAR;
              break;

            case '_':
              if (p == pe)
                error (Ere_invalid_pattern);
              c = *p++;
              switch (c)
                {
                case '<':
                  *b++ = BEGSYMBOL;
                  break;

                case '>':
                  *b++ = ENDSYMBOL;
                  break;

                case 'b':
                  *b++ = SYMBOLBOUND;
                  break;

                case 'B':
                  *b++ = NOT_SYMBOLBOUND;
                  break;

                case 's':
                  r_last_start = b;
                  *b++ = SYMBOLCHAR;
                  break;

                case 'S':
                  r_last_start = b;
                  *b++ = NOT_SYMBOLCHAR;
                  break;

                default:
                  p--;
                  c = '_';
                  goto normal_char;
                }
              break;

            case 's':
              if (p == pe)
                error (Ere_invalid_pattern);
              c = *p++;
              if (!ascii_char_p (c) || syntax_spec_table[c] == -1)
                error (Einvalid_syntax_spec);
              r_last_start = b;
              *b++ = SYNTAX_SPEC;
              *b++ = syntax_spec_table[c];
              break;

            case 'S':
              if (p == pe)
                error (Ere_invalid_pattern);
              c = *p++;
              if (!ascii_char_p (c) || syntax_spec_table[c] == -1)
                error (Einvalid_syntax_spec);
              r_last_start = b;
              *b++ = NOT_SYNTAX_SPEC;
              *b++ = syntax_spec_table[c];
              break;

            default:
              goto normal_char;
            }
          break;

        default:
        normal_char:
          if (!r_normal_char || r_normal_char + *r_normal_char + 1 != b
              || *r_normal_char >= CHAR_LIMIT - 1
              || (p < pe && (*p == '*' || *p == '+' || *p == '?'))
              || (p < pe - 1 && *p == '\\' && p[1] == '{'))
            {
              r_last_start = b;
              *b++ = NORMAL_CHARS;
              r_normal_char = b;
              *b++ = 0;
            }
          *b++ = ascii_char_p (c) ? r_translate[c] : c;
          (*r_normal_char)++;
        }
    }

  if (r_stackp != r_stackb)
    error (Ere_unmatched_lparen);

  if (r_remain_branch)
    {
      b = branch (b);
      *b++ = END_BRANCH;
    }

  r_used = b - r_buffer;

  check_inner_closure (r_buffer, b);

#ifdef DEBUG
  dump ();
#endif
}

int
regexp_compile::match_void_p (const Char *p, const Char *pe)
{
  while (p < pe)
    switch (*p++)
      {
      case BEGLINE:
      case BEGBUF:
        break;

      case NORMAL_CHARS:
      case ANYCHAR:
      case CHAR_CLASS:
      case CHAR_CLASS_NOT:
      case WORDCHAR:
      case NOT_WORDCHAR:
      case SYNTAX_SPEC:
      case NOT_SYNTAX_SPEC:
      case SYMBOLCHAR:
      case NOT_SYMBOLCHAR:
        return 0;

      case BEGWORD:
      case ENDWORD:
      case WORDBOUND:
      case NOT_WORDBOUND:
      case BEGSYMBOL:
      case ENDSYMBOL:
      case SYMBOLBOUND:
      case NOT_SYMBOLBOUND:
        break;

      case ENDLINE:
      case ENDBUF:
      case END_BRANCH:
        return 1;

      case CLOSURE:
      case CLOSURE_BACKTRACK:
      case CLOSURE_SIMPLE:
      case SHORTEST_CLOSURE:
      case SHORTEST_CLOSURE_BACKTRACK:
      case SHORTEST_CLOSURE_SIMPLE:
        if (*p && !match_void_p (p + 3, p + p[2] - 1))
          return 0;
        p += p[2] - 1;
        break;

      case BRANCH:
      case BRANCH_BACKTRACK:
        {
          int f = 0;
          do
            {
              if (match_void_p (p + 1, p + *p - 1))
                f = 1;
              p += *p - 1;
            }
          while (*p++ != END_BRANCH);
          if (!f)
            return 0;
        }
        break;

      case BACKREF:
        return 1;

      case START_SAVE_REGS:
      case END_SAVE_REGS:
        p++;
        break;
      }
  return 1;
}

int
regexp_compile::match_bol_p (const Char *p, const Char *pe)
{
  while (p < pe)
    switch (*p++)
      {
      default:
        return 0;

      case BEGLINE:
      case BEGBUF:
        return 1;

      case START_SAVE_REGS:
      case END_SAVE_REGS:
        p++;
        break;

      case BRANCH:
      case BRANCH_BACKTRACK:
        do
          {
            if (!match_bol_p (p + 1, p + *p - 1))
              return 0;
            p += *p - 1;
          }
        while (*p++ != END_BRANCH);
        return 1;

      case CLOSURE:
      case CLOSURE_BACKTRACK:
      case CLOSURE_SIMPLE:
      case SHORTEST_CLOSURE:
      case SHORTEST_CLOSURE_BACKTRACK:
      case SHORTEST_CLOSURE_SIMPLE:
        return *p && match_bol_p (p + 3, p + p[2] - 1);
      }
  return 0;
}

void
regexp_compile::char_class_fastmap (const Char *p, char *fastmap) const
{
  const Char *pe = p + *p;
  for (p++; p < pe;)
    {
      int h = *p >> 8;
      if (*p & 0xff)
        {
          if (h)
            fastmap[h] = p[1] == ((256 / NBITS) << 16) + 0 ? 1 : -1;
          else
            {
              for (int u = (p[1] >> 8) * NBITS, l = (p[1] & 0xff) * NBITS; l < u; l++)
                fastmap[l] = 1;
            }
          p += 2;
        }
      else
        {
          int u = p[1] >> 8, l = p[1] & 0xff;
          p += 2;
          if (h)
            fastmap[h] = -1;
          else
            {
              for (int i = u - l - 1; i >= 0; i--)
                {
                  int ii = (i + l) * NBITS;
                  for (int j = 0; j < NBITS; j++)
                    if (p[i] & (1 << j))
                      fastmap[ii + j] = 1;
                }
            }
          p += u - l;
        }
    }
}

void
regexp_compile::char_class_not_fastmap (const Char *p, char *fastmap) const
{
  char tem[256];
  bzero (tem, sizeof tem);
  char_class_fastmap (p, tem);
  for (int i = 0; i < 256; i++)
    if (tem[i] <= 0)
      fastmap[i] = 1;
}

int
regexp_compile::compile_fastmap (char *fastmap, const Char *p, const Char *pe) const
{
  while (p < pe)
    {
      switch (*p++)
        {
        case BEGLINE:
        case BEGBUF:
        case ENDBUF:
          break;

        case ENDLINE:
          fastmap['\n'] = 1;
          return 1;

        case ANYCHAR:
          {
            int n = fastmap['\n'];
            memset (fastmap, 1, 256);
            fastmap['\n'] = n;
            return 1;
          }

        case START_SAVE_REGS:
        case END_SAVE_REGS:
          p++;
          break;

        case BACKREF:
          return 0;

        case NORMAL_CHARS:
          {
            Char c = p[1];
            if (c < 256)
              fastmap[c] = 1;
            else
              fastmap[c >> 8] = 1;
            return 1;
          }

        case END_BRANCH:
          return 0;

        case BRANCH:
        case BRANCH_BACKTRACK:
          {
            int f = 1;
            do
              {
                if (!compile_fastmap (fastmap, p + 1, p + *p - 1))
                  f = 0;
                p += *p - 1;
              }
            while (*p++ != END_BRANCH);
            if (f)
              return 1;
            break;
          }

        case CLOSURE:
        case CLOSURE_BACKTRACK:
        case CLOSURE_SIMPLE:
        case SHORTEST_CLOSURE:
        case SHORTEST_CLOSURE_BACKTRACK:
        case SHORTEST_CLOSURE_SIMPLE:
          if (compile_fastmap (fastmap, p + 3, p + p[2] - 1) && *p)
            return 1;
          p += p[2] - 1;
          break;

        case CHAR_CLASS:
          char_class_fastmap (p, fastmap);
          return 1;

        case CHAR_CLASS_NOT:
          char_class_not_fastmap (p, fastmap);
          return 1;

        case BEGWORD:
        case ENDWORD:
        case WORDBOUND:
        case NOT_WORDBOUND:
        case BEGSYMBOL:
        case ENDSYMBOL:
        case SYMBOLBOUND:
        case NOT_SYMBOLBOUND:
          break;

        case WORDCHAR:
          {
            for (int i = 0; i < 256; i++)
              if (re_syntax_word_p (xchar_syntax (r_syntax_table, i)))
                fastmap[i] = 1;
            return 1;
          }

        case NOT_WORDCHAR:
          {
            for (int i = 0; i < 256; i++)
              if (!re_syntax_word_p (xchar_syntax (r_syntax_table, i)))
                fastmap[i] = 1;
            return 1;
          }

        case SYMBOLCHAR:
          {
            for (int i = 0; i < 256; i++)
              if (re_syntax_symbol_p (xchar_syntax (r_syntax_table, i)))
                fastmap[i] = 1;
            return 1;
          }

        case NOT_SYMBOLCHAR:
          {
            for (int i = 0; i < 256; i++)
              if (!re_syntax_symbol_p (xchar_syntax (r_syntax_table, i)))
                fastmap[i] = 1;
            return 1;
          }

        case SYNTAX_SPEC:
          {
            int n = *p++;
            for (int i = 0; i < 256; i++)
              if (xchar_syntax (r_syntax_table, i) == n)
                fastmap[i] = 1;
            return 1;
          }

        case NOT_SYNTAX_SPEC:
          {
            int n = *p++;
            for (int i = 0; i < 256; i++)
              if (xchar_syntax (r_syntax_table, i) != n)
                fastmap[i] = 1;
            return 1;
          }
        }
    }
  return 0;
}

struct re_point: public Point
{
  point_t p_min;
  point_t p_max;

  int bobp (const Regexp &re) const
    {
      return (p_point == re.last_match ()
              ? re.last_match_char () == lChar_EOF
              : p_point <= p_min);
    }
  int eobp () const {return p_point >= p_max;}
  void forward ()
    {
      if (++p_offset == p_chunk->c_used && p_chunk->c_next)
        {
          p_offset = 0;
          p_chunk = p_chunk->c_next;
        }
      p_point++;
    }
  void backward ()
    {
      if (--p_offset < 0 && p_chunk->c_prev)
        {
          p_chunk = p_chunk->c_prev;
          p_offset = p_chunk->c_used - 1;
        }
      p_point--;
    }
  Char prevch (const Regexp &re) const
    {return (p_point == re.last_match ()
             ? Char (re.last_match_char ())
             : Point::prevch ());}
  Char nextch () const {return ch ();}
  Char getch ()
    {
      Char c = nextch ();
      forward ();
      return c;
    }
  void back (int);
  int nextl ()
    {
      Char *p = p_chunk->c_text;
      for (int i = p_point; i < p_max; i++)
        if (p[i] == '\n')
          {
            p_point = p_offset = i + 1;
            return 1;
          }
      return 0;
    }
};

void
re_point::back (int d)
{
  Chunk *cp = p_chunk;
  while (p_offset + d < 0)
    {
      d += p_offset + 1;
      p_point -= p_offset + 1;
      cp = cp->c_prev;
      p_offset = cp->c_used - 1;
    }
  p_offset += d;
  p_point += d;
  p_chunk = cp;
}

inline int
Regexp::bobp (const re_point &point) const
{
  return (point.p_point == last_match ()
          ? last_match_char () == lChar_EOF
          : point.p_point <= range ().p1);
}

inline int
Regexp::eobp (const re_point &point) const
{
  return point.p_point >= range ().p2;
}

void
Regexp::compile (const Char *p, int size, int use_fastmap)
{
  regexp_compile re (re_translate, re_syntax_table);
  re.compile (p, size);
  re_pattern = re.r_buffer;
  re.r_buffer = 0;
  re_size = re.r_used;
  re_has_backref = re.r_has_backref;
  if (use_fastmap)
    {
      re_match_bol_p = regexp_compile::match_bol_p (re_pattern, re_pattern + re_size);
      re_match_void_p = regexp_compile::match_void_p (re_pattern, re_pattern + re_size);
      bzero (re_fastmap, sizeof re_fastmap);
      if (!re_match_void_p
          && !re.compile_fastmap (re_fastmap, re_pattern, re_pattern + re_size))
        re_match_void_p = 1;
    }
}

void
Regexp::compile (lisp object, int use_fastmap)
{
  assert (regexpp (object));
  assert (xregexp_pattern (object));
  re_object = object;
  re_pattern = xregexp_pattern (object);
  re_size = xregexp_length (object);
  re_translate = (xregexp_flags (object) & lregexp::TRANSLATE
                  ? char_translate_upcase_table : char_no_translate_table);
  re_match_void_p = xregexp_flags (object) & lregexp::MATCH_VOID;
  re_match_bol_p = xregexp_flags (object) & lregexp::MATCH_BOL;
  re_has_backref = xregexp_flags (object) & lregexp::HAS_BACKREF;
  if (use_fastmap)
    {
      regexp_compile re (re_translate, re_syntax_table, 0);
      bzero (re_fastmap, sizeof re_fastmap);
      if (!re_match_void_p
          && !re.compile_fastmap (re_fastmap, re_pattern, re_pattern + re_size))
        re_match_void_p = 1;
    }
}

int
Regexp::merge_fastmap (lisp object, char *fastmap, const syntax_table *tab)
{
  if (xregexp_flags (object) & lregexp::MATCH_VOID)
    return 0;
  const u_char *translate = (xregexp_flags (object) & lregexp::TRANSLATE
                             ? char_translate_upcase_table
                             : char_no_translate_table);

  regexp_compile re (translate, tab, 0);
  char buf[256];
  bzero (buf, sizeof buf);
  if (!re.compile_fastmap (buf, xregexp_pattern (object),
                           xregexp_pattern (object) + xregexp_length (object)))
    return 0;
  if (xregexp_flags (object) & lregexp::TRANSLATE)
    for (int u = 'A', l = 'a'; u <= 'Z'; u++, l++)
      buf[u] = buf[l] = buf[u] | buf[l];
  for (int i = 0; i < 256; i++)
    fastmap[i] |= buf[i];
  return 1;
}

lisp
Regexp::make_regexp (lisp source) const
{
  assert (re_pattern);
  lisp re = ::make_regexp ();
  xregexp_pattern (re) = (Char *)xmalloc (sizeof (Char) * re_size);
  bcopy (re_pattern, xregexp_pattern (re), re_size);
  xregexp_length (re) = re_size;
  xregexp_flags (re) = 0;
  if (re_match_void_p)
    xregexp_flags (re) |= lregexp::MATCH_VOID;
  if (re_match_bol_p)
    xregexp_flags (re) |= lregexp::MATCH_BOL;
  if (re_translate != char_no_translate_table)
    xregexp_flags (re) |= lregexp::TRANSLATE;
  if (re_has_backref)
    xregexp_flags (re) |= lregexp::HAS_BACKREF;
  xregexp_source (re) = source;
  return re;
}

int
Regexp::match (const re_point &point)
{
  re_failure.init ();
  re_regs.nregs = 0;
  re_point tem = point;
  if (!match (tem, re_pattern, re_pattern + re_size))
    return 0;
  re_regs.start[0] = point.p_point;
  re_regs.end[0] = tem.p_point;
  return 1;
}

int
Regexp::match_char_class (const Char *p, Char c) const
{
  c = ascii_char_p (c) ? re_translate[c] : c;
  int hi = c >> 8, lo = c & 0xff;
  const Char *pe = p + *p;
  for (p++; p < pe;)
    {
      if (*p & 0xff)
        {
          if ((*p >> 8) == hi)
            {
              int x = lo / NBITS;
              return x >= (p[1] & 0xff) && x < (p[1] >> 8);
            }
          p += 2;
        }
      else
        {
          int u = p[1] >> 8, l = p[1] & 0xff;
          if ((*p >> 8) == hi)
            {
              int x = lo / NBITS;
              if (x < l || x >= u)
                return 0;
              return p[2 + x - l] & (1 << (lo % NBITS));
            }
          p += 2 + u - l;
        }
    }
  return 0;
}

inline int
Regexp::backref (re_point &point, int no) const
{
  if (no > re_regs.nregs || re_regs.start[no] < 0 || re_regs.end[no] < 0)
    return 0;
  if (re_regs.start[no] > re_regs.end[no] || re_regs.end[no] > point.p_point)
    return 0;
  re_point ref = point;
  ref.back (re_regs.start[no] - ref.p_point);
  for (int n = re_regs.end[no] - re_regs.start[no]; n > 0; n--)
    {
      if (point.eobp ())
        return 0;
      Char c1 = point.getch ();
      if (ascii_char_p (c1))
        c1 = re_translate[c1];
      Char c2 = ref.getch ();
      if (ascii_char_p (c2))
        c2 = re_translate[c2];
      if (c1 != c2)
        return 0;
    }
  return 1;
}

static int
regs_equal1 (const Regexp::sregs &p, const Regexp::sregs &q)
{
  if (p.nregs != q.nregs)
    return 0;
  for (int i = 1; i <= p.nregs; i++)
    if (p.start[i] != q.start[i] || p.end[i] != q.end[i])
      return 0;
  return 1;
}

static void
copy_regs1 (Regexp::sregs &d, const Regexp::sregs &s)
{
  d.nregs = s.nregs;
  for (int i = 1; i <= d.nregs; i++)
    {
      d.start[i] = s.start[i];
      d.end[i] = s.end[i];
    }
}

inline int
Regexp::branch (re_point &point, const Char *p, const Char *pe)
{
  sregs save_regs;
  sregs match_regs;
  re_point longest;
  longest.p_point = -1;
  point_t longest_end = -1;
  const Char *prest = p;
  do
    prest += *prest - 1;
  while (*prest++ != END_BRANCH);

  copy_regs1 (save_regs, re_regs);

  do
    {
      re_point tem = point;
      copy_regs1 (re_regs, save_regs);
      if (p[-1] == BRANCH_BACKTRACK)
        {
          if (match (tem, p + 1, p + *p - 1))
            {
              point_t end = tem.p_point;
              while (1)
                {
                  if (match (tem, prest, pe))
                    {
                      if (tem.p_point > longest.p_point
                          || (tem.p_point == longest.p_point
                              && end > longest_end))
                        {
                          longest = tem;
                          longest_end = end;
                          copy_regs1 (match_regs, re_regs);
                        }
                    }

                  do
                    {
                      if (--end < point.p_point)
                        goto branch_backtrack_end;
                      tem = point;
                      tem.p_max = end;
                      copy_regs1 (re_regs, save_regs);
                    }
                  while (!match (tem, p + 1, p + *p - 1));
                  tem.p_max = point.p_max;
                }
            branch_backtrack_end:;
            }
        }
      else
        {
          if (match (tem, p + 1, p + *p - 1))
            {
              point_t end = tem.p_point;
              if (match (tem, prest, pe))
                {
                  if (tem.p_point > longest.p_point
                      || (tem.p_point == longest.p_point
                          && end > longest_end))
                    {
                      longest = tem;
                      longest_end = end;
                      copy_regs1 (match_regs, re_regs);
                    }
                }
            }
        }
      p += *p;
    }
  while (p < prest);
  if (longest.p_point == -1)
    return 0;
  point = longest;
  copy_regs1 (re_regs, match_regs);
  return 1;
}

void
Regexp::start_save_regs (int n, point_t point)
{
  if (n > re_regs.nregs)
    {
      for (int i = re_regs.nregs + 1; i < n; i++)
        {
          re_regs.start[i] = -1;
          re_regs.end[i] = -1;
        }
      re_regs.nregs = n;
    }
  re_regs.start[n] = point;
  re_regs.end[n] = -1;
}

inline void
Regexp::end_save_regs (int n, point_t point)
{
  re_regs.end[n] = point;
}

inline int
Regexp::repeat_max (Char n)
{
  return n == REPEAT_INFINITY ? INT_MAX - 1 : n;
}

inline int
Regexp::closure (re_point &point, const Char *p, const Char *pe, int shortest)
{
  sregs save_regs;
  sregs match_regs;
  re_point longest;
  longest.p_point = -1;
  int nmatches = 0;
  const int nmin = *p++;
  const int nmax = repeat_max (*p++);
  const Char *const prest = p + *p - 3;
  p++;

  while (1)
    {
      if (nmatches >= nmin)
        {
          re_point tem = point;
          copy_regs1 (save_regs, re_regs);
          if (match (tem, prest, pe))
            {
              if (shortest)
                {
                  point = tem;
                  return 1;
                }
              if (tem.p_point >= longest.p_point)
                {
                  longest = tem;
                  copy_regs1 (match_regs, re_regs);
                }
            }
          copy_regs1 (re_regs, save_regs);
        }
      if (nmatches >= nmax)
        break;
      point_t opoint = point.p_point;
      if (!match (point, p, prest))
        break;
      if (point.p_point == opoint)
        nmatches = nmax;
      else
        nmatches++;
    }

  if (longest.p_point == -1)
    return 0;
  point = longest;
  copy_regs1 (re_regs, match_regs);
  return 1;
}

Regexp::record_failure::record_failure ()
     : m_ep (m_entbuf)
{
  bzero (m_tab, sizeof m_tab);
}

inline u_int
Regexp::record_failure::hashval (const Char *pat, point_t point, point_t max)
{
  return (pointer_t (pat) << 24) + point + (max << 12);
}

inline void
Regexp::record_failure::init ()
{
  if (m_ep != m_entbuf)
    {
      m_ep = m_entbuf;
      bzero (m_tab, sizeof m_tab);
    }
}

inline int
Regexp::record_failure::find (const Char *pat, point_t point, point_t max) const
{
  for (const ent *e = m_tab[hashval (pat, point, max) % TABSIZE]; e; e = e->cdr)
    if (e->point == point && e->max == max && e->pat == pat)
      return 1;
  return 0;
}

inline void
Regexp::record_failure::add (const Char *pat, const re_point &point)
{
  if (m_ep != m_entbuf + numberof (m_entbuf))
    {
      u_int h = hashval (pat, point.p_point, point.p_max) % TABSIZE;
      m_ep->pat = pat;
      m_ep->point = point.p_point;
      m_ep->max = point.p_max;
      m_ep->cdr = m_tab[h];
      m_tab[h] = m_ep;
      m_ep++;
    }
}

class backtrack_stack
{
public:
  enum {MAX_STACK = 4096, STACK_GROW = 256};
  struct stack
    {
      Regexp::sregs regs;
      re_point point;
      int match_void;
    };
  stack b_initstack[STACK_GROW];
  stack *b_stack;
  int b_used;
  int b_allocated;

  backtrack_stack () : b_stack (b_initstack), b_used (0), b_allocated (STACK_GROW) {}
  ~backtrack_stack ()
    {if (b_stack != b_initstack) xfree (b_stack);}
  int push (const re_point &, const Regexp::sregs &, int);
  void clear () {b_used = 0;}
  int match (const re_point &, const Regexp::sregs &, int, int) const;
};

int
backtrack_stack::match (const re_point &point, const Regexp::sregs &regs,
                        int match_void, int has_backref) const
{
  for (int i = 0; i < b_used; i++)
    if (point.p_point == b_stack[i].point.p_point
        && match_void == b_stack[i].match_void
        && (!has_backref || regs_equal1 (regs, b_stack[i].regs)))
      return 1;
  return 0;
}

int
backtrack_stack::push (const re_point &point, const Regexp::sregs &regs,
                       int match_void)
{
  if (b_used == b_allocated)
    {
      if (b_allocated >= MAX_STACK)
        FEsimple_error (Ecomplex_regexp);
      b_allocated += STACK_GROW;
      if (b_stack == b_initstack)
        {
          b_stack = (stack *)xmalloc (sizeof *b_stack * b_allocated);
          memcpy (b_stack, b_initstack, sizeof b_initstack);
        }
      else
        b_stack = (stack *)xrealloc (b_stack, sizeof *b_stack * b_allocated);
    }
  copy_regs1 (b_stack[b_used].regs, regs);
  b_stack[b_used].point = point;
  b_stack[b_used].match_void = match_void;
  b_used++;
  return match_void;
}

class state_buf
{
  enum {BITMAP_SIZE = 4096};
  enum {MAX_BUF = 4096, BUF_GROW = 128};
  u_char s_bitmap[BITMAP_SIZE / CHAR_BIT];
  u_long *s_buf;
  int s_used;
  int s_allocated;

public:
  state_buf () : s_buf (0), s_used (0), s_allocated (0)
    {bzero (s_bitmap, sizeof s_bitmap);}
  ~state_buf () {xfree (s_buf);}
  void add (u_long);
  int test (u_long) const;
};

void
state_buf::add (u_long x)
{
  if (x < BITMAP_SIZE)
    s_bitmap[x / CHAR_BIT] |= 1 << (x % CHAR_BIT);
  else
    {
      if (s_used == s_allocated)
        {
          if (s_allocated >= MAX_BUF)
            FEsimple_error (Ecomplex_regexp);
          s_allocated += BUF_GROW;
          s_buf = (u_long *)xrealloc (s_buf, sizeof *s_buf * s_allocated);
        }
      s_buf[s_used++] = x;
    }
}

int
state_buf::test (u_long x) const
{
  if (x < BITMAP_SIZE)
    return s_bitmap[x / CHAR_BIT] & (1 << (x % CHAR_BIT));
  for (int i = 0; i < s_used; i++)
    if (x == s_buf[i])
      return 1;
  return 0;
}

inline int
Regexp::compare_regs (const sregs &r1, const sregs &r2)
{
  int i;
  for (i = 1; i <= r1.nregs; i++)
    {
      if (i > r2.nregs)
        return 1;
      int l1 = r1.end[i] - r1.start[i];
      int l2 = r2.end[i] - r2.start[i];
      if (l1 != l2)
        return l1 - l2;
      if (r1.start[i] != r2.start[i])
        return r1.start[i] - r2.start[i];
    }
  return i <= r2.nregs ? -1 : 0;
}

inline int
Regexp::closure_backtrack (re_point &point, const Char *p, const Char *pe,
                           int shortest)
{
  backtrack_stack stack_1, stack_2;
  backtrack_stack *fstack = &stack_1, *tstack = &stack_2;
  state_buf state;
  sregs match_regs;
  re_point longest;
  longest.p_point = -1;
  point_t beg = point.p_point;
  int nmatches = 0;
  const int nmin = *p++;
  const int nmax = repeat_max (*p++);
  const Char *const prest = p + *p - 3;
  p++;
  fstack->push (point, re_regs, 0);
  int match_void = 0;

  while (1)
    {
      if (nmatches >= nmin || match_void)
        for (int i = fstack->b_used - 1; i >= 0; i--)
          if (nmatches >= nmin || fstack->b_stack[i].match_void)
            {
              re_point tem = fstack->b_stack[i].point;
              copy_regs1 (re_regs, fstack->b_stack[i].regs);
              if (match (tem, prest, pe))
                {
                  if (shortest)
                    {
                      point = tem;
                      return 1;
                    }
                  if (tem.p_point > longest.p_point
                      || (tem.p_point == longest.p_point
                          && compare_regs (re_regs, match_regs) > 0))
                    {
                      longest = tem;
                      copy_regs1 (match_regs, re_regs);
                    }
                }
              else if (!re_has_backref)
                re_failure.add (prest, fstack->b_stack[i].point);
            }
      if (nmatches >= nmax)
        break;

      for (int i = 0; i < fstack->b_used; i++)
        if (!fstack->b_stack[i].match_void)
          {
            int l = fstack->b_stack[i].point.p_point - beg;
            if (state.test (l))
              fstack->b_stack[i].match_void = 1;
            else
              state.add (l);
          }

      match_void = 0;
      tstack->clear ();

      for (int i = 0; i < fstack->b_used; i++)
        if (!fstack->b_stack[i].match_void)
          {
            point = fstack->b_stack[i].point;
            point_t opoint = point.p_point;
            copy_regs1 (re_regs, fstack->b_stack[i].regs);
            if (match (point, p, prest))
              {
                if (re_has_backref || !re_failure.find (prest, point.p_point, point.p_max))
                  {
                    int eq = point.p_point == opoint;
                    if (!fstack->match (point, re_regs, eq, re_has_backref)
                        && !tstack->match (point, re_regs, eq, re_has_backref))
                      match_void |= tstack->push (point, re_regs, eq);
                  }
                point_t omax = point.p_max;
                point_t end = point.p_point;
                while (--end >= fstack->b_stack[i].point.p_point)
                  {
                    point = fstack->b_stack[i].point;
                    point.p_max = end;
                    copy_regs1 (re_regs, fstack->b_stack[i].regs);
                    if (match (point, p, prest)
                        && (re_has_backref
                            || !re_failure.find (prest, point.p_point, omax)))
                      {
                        int eq = point.p_point == opoint;
                        if (!fstack->match (point, re_regs, eq, re_has_backref)
                            && !tstack->match (point, re_regs, eq, re_has_backref))
                          {
                            point.p_max = omax;
                            match_void |= tstack->push (point, re_regs, eq);
                          }
                      }
                    QUIT;
                  }
              }
          }
      if (!tstack->b_used)
        break;
      nmatches++;
      swap (fstack, tstack);
    }
  if (longest.p_point == -1)
    return 0;
  point = longest;
  copy_regs1 (re_regs, match_regs);
  return 1;
}

int
Regexp::simple_closure (re_point &point, const Char *p, const Char *pe)
{
  const int nmin = *p++;
  const int nmax = min (repeat_max (*p++),
                        int (point.p_max - point.p_point));
  int nregs = re_regs.nregs;
  int nmatches = 0;
  const Char *const prest = p + *p - 3;
  p++;

  switch (*p)
    {
    case ANYCHAR:
      for (nmatches = 0; nmatches < nmax; nmatches++)
        {
          if (point.nextch () == '\n')
            break;
          point.forward ();
        }
      break;

    case NORMAL_CHARS:
      for (nmatches = 0; nmatches < nmax; nmatches++)
        {
          Char cc = point.nextch ();
          if (ascii_char_p (cc))
            cc = re_translate[cc];
          if (cc != p[2])
            break;
          point.forward ();
        }
      break;

    case WORDCHAR:
      for (nmatches = 0; nmatches < nmax; nmatches++)
        {
          if (!syntax_word_p (point.nextch ()))
            break;
          point.forward ();
        }
      break;

    case NOT_WORDCHAR:
      for (nmatches = 0; nmatches < nmax; nmatches++)
        {
          if (syntax_word_p (point.nextch ()))
            break;
          point.forward ();
        }
      break;

    case SYMBOLCHAR:
      for (nmatches = 0; nmatches < nmax; nmatches++)
        {
          if (!syntax_symbol_p (point.nextch ()))
            break;
          point.forward ();
        }
      break;

    case NOT_SYMBOLCHAR:
      for (nmatches = 0; nmatches < nmax; nmatches++)
        {
          if (syntax_symbol_p (point.nextch ()))
            break;
          point.forward ();
        }
      break;

    case SYNTAX_SPEC:
      for (nmatches = 0; nmatches < nmax; nmatches++)
        {
          if (char_syntax (point.nextch ()) != p[1])
            break;
          point.forward ();
        }
      break;

    case NOT_SYNTAX_SPEC:
      for (nmatches = 0; nmatches < nmax; nmatches++)
        {
          if (char_syntax (point.nextch ()) == p[1])
            break;
          point.forward ();
        }
      break;

    case CHAR_CLASS:
      for (nmatches = 0; nmatches < nmax; nmatches++)
        {
          if (!match_char_class (p + 1, point.nextch ()))
            break;
          point.forward ();
        }
      break;

    case CHAR_CLASS_NOT:
      for (nmatches = 0; nmatches < nmax; nmatches++)
        {
          if (match_char_class (p + 1, point.nextch ()))
            break;
          point.forward ();
        }
      break;
    }

  if (nmatches < nmin)
    return 0;

  sregs match_regs;
  re_point longest;
  longest.p_point = -1;

  while (1)
    {
      re_regs.nregs = nregs;
      re_point tem = point;
      if (match (tem, prest, pe) && tem.p_point > longest.p_point)
        {
          if (tem.p_point == tem.p_max || prest == pe)
            {
              point = tem;
              return 1;
            }
          longest = tem;
          copy_regs1 (match_regs, re_regs);
        }
      if (nmatches == nmin)
        break;
      nmatches--;
      point.backward ();
    }

  if (longest.p_point == -1)
    return 0;
  point = longest;
  copy_regs1 (re_regs, match_regs);
  return 1;
}

int
Regexp::shortest_simple_closure (re_point &point, const Char *p, const Char *pe)
{
  const int nmin = *p++;
  const int nmax = min (repeat_max (*p++),
                        int (point.p_max - point.p_point));
  int nregs = re_regs.nregs;
  const Char *const prest = p + *p - 3;
  p++;

  for (int nmatches = 0;; nmatches++)
    {
      if (nmatches >= nmin)
        {
          re_regs.nregs = nregs;
          re_point tem = point;
          if (match (tem, prest, pe))
            {
              point = tem;
              return 1;
            }
        }
      if (nmatches >= nmax)
        return 0;

      Char cc = point.nextch ();
      switch (*p)
        {
        case ANYCHAR:
          if (cc == '\n')
            return 0;
          break;

        case NORMAL_CHARS:
          if (ascii_char_p (cc))
            cc = re_translate[cc];
          if (cc != p[2])
            return 0;
          break;

        case WORDCHAR:
          if (!syntax_word_p (cc))
            return 0;
          break;

        case NOT_WORDCHAR:
          if (syntax_word_p (cc))
            return 0;
          break;

        case SYMBOLCHAR:
          if (!syntax_symbol_p (cc))
            return 0;
          break;

        case NOT_SYMBOLCHAR:
          if (syntax_symbol_p (cc))
            return 0;
          break;

        case SYNTAX_SPEC:
          if (char_syntax (cc) != p[1])
            return 0;
          break;

        case NOT_SYNTAX_SPEC:
          if (char_syntax (cc) == p[1])
            return 0;
          break;

        case CHAR_CLASS:
          if (!match_char_class (p + 1, cc))
            return 0;
          break;

        case CHAR_CLASS_NOT:
          if (match_char_class (p + 1, cc))
            return 0;
          break;
        }
      point.forward ();
    }
}

int
Regexp::match (re_point &point, const Char *p, const Char *pe)
{
  while (p < pe)
    {
      Char re = *p++;
      switch (re)
        {
        case BEGLINE:
          if (!bobp (point) && point.prevch (*this) != '\n')
            return 0;
          break;

        case ENDLINE:
          if (!eobp (point) && point.nextch () != '\n')
            return 0;
          break;

        case BEGBUF:
          if (!bobp (point))
            return 0;
          break;

        case ENDBUF:
          if (!eobp (point))
            return 0;
          break;

        case ANYCHAR:
          if (point.eobp () || point.getch () == '\n')
            return 0;
          break;

        case NORMAL_CHARS:
          {
            int n = *p++;
            for (int i = 0; i < n; i++)
              {
                if (point.eobp ())
                  return 0;
                Char c = point.getch ();
                if (ascii_char_p (c))
                  c = re_translate[c];
                if (c != *p++)
                  return 0;
              }
            break;
          }

        case BEGWORD:
          if ((point.bobp (*this) || !syntax_word_p (point.prevch (*this)))
              && (!point.eobp () && syntax_word_p (point.nextch ())))
            break;
          return 0;

        case ENDWORD:
          if ((!point.bobp (*this) && syntax_word_p (point.prevch (*this)))
              && (point.eobp () || !syntax_word_p (point.nextch ())))
            break;
          return 0;

        case WORDBOUND:
          if ((point.bobp (*this) || !syntax_word_p (point.prevch (*this)))
              != (point.eobp () || !syntax_word_p (point.nextch ())))
            break;
          return 0;

        case NOT_WORDBOUND:
          if (point.bobp (*this) && point.eobp ())
            return 0;
          if ((point.bobp (*this) || !syntax_word_p (point.prevch (*this)))
              == (point.eobp () || !syntax_word_p (point.nextch ())))
            break;
          return 0;

        case WORDCHAR:
          if (point.eobp () || !syntax_word_p (point.getch ()))
            return 0;
          break;

        case NOT_WORDCHAR:
          if (point.eobp () || syntax_word_p (point.getch ()))
            return 0;
          break;

        case BEGSYMBOL:
          if ((point.bobp (*this)
               || !syntax_symbol_p (point.prevch (*this)))
              && (!point.eobp ()
                  && syntax_symbol_p (point.nextch ())))
            break;
          return 0;

        case ENDSYMBOL:
          if ((!point.bobp (*this)
               && syntax_symbol_p (point.prevch (*this)))
              && (point.eobp ()
                  || !syntax_symbol_p (point.nextch ())))
            break;
          return 0;

        case SYMBOLBOUND:
          if ((point.bobp (*this)
               || !syntax_symbol_p (point.prevch (*this)))
              != (point.eobp ()
                  || !syntax_symbol_p (point.nextch ())))
            break;
          return 0;

        case NOT_SYMBOLBOUND:
          if (point.bobp (*this) && point.eobp ())
            return 0;
          if ((point.bobp (*this)
               || !syntax_symbol_p (point.prevch (*this)))
              == (point.eobp ()
                  || !syntax_symbol_p (point.nextch ())))
            break;
          return 0;

        case SYMBOLCHAR:
          if (point.eobp () || !syntax_symbol_p (point.getch ()))
            return 0;
          break;

        case NOT_SYMBOLCHAR:
          if (point.eobp () || syntax_symbol_p (point.getch ()))
            return 0;
          break;

        case SYNTAX_SPEC:
          {
            if (point.eobp ())
              return 0;
            if (char_syntax (point.getch ()) != *p++)
              return 0;
            break;
          }

        case NOT_SYNTAX_SPEC:
          {
            if (point.eobp ())
              return 0;
            if (char_syntax (point.getch ()) == *p++)
              return 0;
            break;
          }

        case START_SAVE_REGS:
          start_save_regs (*p++, point.p_point);
          break;

        case END_SAVE_REGS:
          end_save_regs (*p++, point.p_point);
          break;

        case CHAR_CLASS:
          if (point.eobp () || !match_char_class (p, point.getch ()))
            return 0;
          p += *p;
          break;

        case CHAR_CLASS_NOT:
          if (point.eobp () || match_char_class (p, point.getch ()))
            return 0;
          p += *p;
          break;

        case BACKREF:
          if (!backref (point, *p++))
            return 0;
          break;

        case END_BRANCH:
          return 1;

        case BRANCH:
        case BRANCH_BACKTRACK:
          return branch (point, p, pe);

        case CLOSURE_SIMPLE:
          return simple_closure (point, p, pe);

        case SHORTEST_CLOSURE_SIMPLE:
          return shortest_simple_closure (point, p, pe);

        case CLOSURE:
        case SHORTEST_CLOSURE:
          return closure (point, p, pe, re == SHORTEST_CLOSURE);

        case CLOSURE_BACKTRACK:
        case SHORTEST_CLOSURE_BACKTRACK:
          return closure_backtrack (point, p, pe, re == SHORTEST_CLOSURE_BACKTRACK);
        }
    }
  return 1;
}

class re_search
{
public:
  virtual int nextl (re_point &) const = 0;
};

class re_search_string: public re_search
{
public:
  virtual int nextl (re_point &point) const
    {return point.nextl ();}
};

class re_search_buffer: public re_search
{
  const Buffer *bufp;
public:
  re_search_buffer (const Buffer *bp) : bufp (bp) {}
  virtual int nextl (re_point &point) const
    {return bufp->line_forward (point, 1) && point.p_point <= point.p_max;}
};

int
Regexp::search (const re_search &re, re_point &point)
{
  if (re_match_bol_p)
    {
      if (!point.bobp (*this) && point.prevch (*this) != '\n' && !re.nextl (point))
        return 0;
      while (1)
        {
          if (!re_match_void_p)
            {
              if (point.eobp ())
                return 0;
              Char c = point.nextch ();
              if (c < 0x100)
                c = re_translate[c];
              else
                c >>= 8;
              if (!re_fastmap[c])
                goto fail;
            }
          if (match (point))
            return 1;
        fail:
          if (!re.nextl (point))
            return 0;
        }
    }

  if (!re_match_void_p)
    {
      for (; !point.eobp (); point.forward ())
        {
          Char c = point.nextch ();
          if (c < 0x100)
            c = re_translate[c];
          else
            c >>= 8;
          if (re_fastmap[c] && match (point))
            return 1;
        }
      return 0;
    }

  while (1)
    {
      if (match (point))
        return 1;
      if (point.eobp ())
        return 0;
      point.forward ();
    }
}

inline void
Regexp::init_match (const re_point &point, point_t last_match,
                    lChar last_match_char)
{
  re_range.p1 = point.p_min;
  re_range.p2 = point.p_max;
  re_last_match = last_match;
  re_last_match_char = last_match_char;
}

inline void
Regexp::init_match (const Buffer *bp, point_t last_match,
                    lChar last_match_char)
{
  re_range.p1 = bp->b_contents.p1;
  re_range.p2 = bp->b_contents.p2;
  re_last_match = last_match;
  re_last_match_char = last_match_char;
}

int
Regexp::search (const Char *string, int size, int offset)
{
  Chunk chunk;
  chunk.c_used = size;
  chunk.c_text = (Char *)string;
  chunk.c_prev = 0;
  chunk.c_next = 0;

  re_point point;
  point.p_point = offset;
  point.p_offset = offset;
  point.p_chunk = &chunk;
  point.p_min = 0;
  point.p_max = size;
  init_match (point, -1, 0);
  return search (re_search_string (), point);
}

int
Regexp::search (const Buffer *bp, const Point &start,
                point_t p1, point_t p2,
                point_t last_match, lChar last_match_char)
{
  re_point point;
  point.p_point = start.p_point;
  point.p_offset = start.p_offset;
  point.p_chunk = start.p_chunk;
  point.p_min = p1;
  point.p_max = p2;
  init_match (bp, last_match, last_match_char);
  return search (re_search_buffer (bp), point);
}

int
Regexp::search_backward (const Buffer *bp, const Point &start,
                         point_t p1, point_t p2,
                         point_t last_match, lChar last_match_char)
{
  re_point point;
  point.p_point = start.p_point;
  point.p_offset = start.p_offset;
  point.p_chunk = start.p_chunk;
  point.p_min = p1;
  point.p_max = p2;
  init_match (bp, last_match, last_match_char);

  if (re_match_bol_p)
    {
      if (!point.bobp (*this) && point.prevch (*this) != '\n')
        {
          bp->go_bol (point);
          if (point.p_point < p1)
            return 0;
        }

      while (1)
        {
          if (!re_match_void_p)
            {
              if (point.eobp ())
                goto fail;
              Char c = point.nextch ();
              if (c < 0x100)
                c = re_translate[c];
              else
                c >>= 8;
              if (!re_fastmap[c])
                goto fail;
            }
          if (match (point))
            return 1;
        fail:
          if (!bp->line_backward (point, 1) || point.p_point < p1)
            return 0;
        }
    }

  if (!re_match_void_p)
    {
      if (point.eobp ())
        {
          if (point.bobp (*this))
            return 0;
          point.backward ();
        }
      while (1)
        {
          Char c = point.nextch ();
          if (c < 0x100)
            c = re_translate[c];
          else
            c >>= 8;
          if (re_fastmap[c] && match (point))
            return 1;
          if (point.bobp (*this))
            return 0;
          point.backward ();
        }
    }

  while (1)
    {
      if (match (point))
        return 1;
      if (point.bobp (*this))
        return 0;
      point.backward ();
    }
}

int
Regexp::match (const Char *string, int size, int offset)
{
  Chunk chunk;
  chunk.c_used = size;
  chunk.c_text = (Char *)string;
  chunk.c_prev = 0;
  chunk.c_next = 0;

  re_point point;
  point.p_point = offset;
  point.p_offset = offset;
  point.p_chunk = &chunk;
  point.p_min = 0;
  point.p_max = size;
  init_match (point, -1, 0);
  return match (point);
}

int
Regexp::match (const Buffer *bp, const Point &start,
               point_t p1, point_t p2)
{
  re_point point;
  point.p_point = start.p_point;
  point.p_offset = start.p_offset;
  point.p_chunk = start.p_chunk;
  point.p_min = p1;
  point.p_max = p2;
  init_match (bp, -1, 0);
  return match (point);
}

int
Regexp::smart_case_fold_p (const Char *p, int l)
{
  const Char *const pe = p + l;
  while (p < pe)
    {
      Char c = *p++;
      switch (c)
        {
        default:
          if (upper_char_p (c))
            return 0;
          break;

        case '[':
          if (p < pe && *p == '^')
            p++;
          if (p < pe && *p == ']')
            p++;
          for (; p < pe && *p != ']'; p++)
            if (upper_char_p (*p))
              return 0;
          break;

        case '\\':
          if (p == pe)
            break;
          c = *p++;
          switch (c)
            {
            case 'B':
            case 'W':
              break;

            case 'S':
              p++;
              break;

            case '_':
              if (p == pe)
                break;
              c = *p++;
              switch (c)
                {
                case 'B':
                case 'S':
                  break;

                default:
                  if (upper_char_p (c))
                    return 0;
                  break;
                }
              break;

            default:
              if (upper_char_p (c))
                return 0;
              break;
            }
          break;
        }
    }
  return 1;
}
