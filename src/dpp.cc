#include "gen-stdafx.h"
#include <stdarg.h>
#include <io.h>

static FILE *fi, *fo;
#define MAXARGS 2
#define MAXIDSIZE 128
static char ident[MAXIDSIZE];
static int linenum = 1;
static char *output_file;
static char *input_file;

struct argments
{
  char name[MAXARGS][MAXIDSIZE];
  int nargs;
  int argtype;
  char optargs[1024];
  char optvars[128];
};

enum
{
  LSHORT,
  LLONG,
  LBIGNUM,
  LFRACT,
  LFLOAT_F,
  LFLOAT_D,
  LCOMPLEX,
  LMAX
};

const char typechar[] = "slbrFDc";

#define AT_INTEGER LBIGNUM
#define AT_RATIONAL LFRACT
#define AT_REAL LFLOAT_D
#define AT_NUMBER LCOMPLEX

struct
{
  const char *ltype;
  const char *fmt;
} typespec[] =
{
  {"Tshort_intP", "xshort_int_value"},
  {"Tlong_int", "xlong_int_value"},
  {"Tbignum", "xbignum_rep"},
  {"Tfraction", "(lfraction *)"},
  {"Tsingle_float", "xsingle_float_value"},
  {"Tdouble_float", "xdouble_float_value"},
  {"Tcomplex", "(lcomplex *)"},
};

#define NOT_DEFINED (-1)

void
error (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  fprintf (stderr, "%s: %d: ", input_file, linenum);
  vfprintf (stderr, fmt, ap);
  fprintf (stderr, "\n");
  va_end (ap);
  fclose (fo);
  fclose (fi);
  _unlink (output_file);
  exit (2);
}

static void
output_line (long ln)
{
  fprintf (fo, "//#line %d \"%s\"\n", ln, input_file);
}

static void
output_line ()
{
  output_line (linenum);
}

static void
output_literal ()
{
  putc ('\n', fo);
  output_line ();
  int c;
  while ((c = getc (fi)) != EOF)
    {
      putc (c, fo);
      if (c == '\n')
        {
          linenum++;
          break;
        }
      if (c == '\\')
        {
          c = getc (fi);
          if (c == EOF)
            break;
          putc (c, fo);
        }
    }
}

static int
readc ()
{
again:
  int c = getc (fi);
  if (c == '\n')
    {
      linenum++;
      int c2 = getc (fi);
      if (c2 == '#')
        {
          output_literal ();
          goto again;
        }
      ungetc (c2, fi);
    }
  return c;
}

static void
unreadc (int c)
{
  ungetc (c, fi);
  if (c == '\n')
    linenum--;
}

static int
skip_white ()
{
  while (1)
    {
      int c = readc ();
      switch (c)
        {
        case EOF:
          return 0;
        case ' ':
        case '\n':
        case '\t':
        case '\r':
        case '\f':
          break;
        default:
          unreadc (c);
          return 1;
        }
    }
}

int
read_ident ()
{
  *ident = 0;
  if (!skip_white ())
    return 0;
  char *p = ident;
  while (1)
    {
      int c = readc ();
      if (c == EOF)
        break;
      if (!isalnum (c) && c != '_')
        {
          unreadc (c);
          break;
        }
      *p++ = c;
    }
  *p = 0;
  return 1;
}

const char *
typestring (int type, int nargs)
{
  static char b[MAXARGS + 1];
  if (nargs == 1)
    {
      b[0] = typechar[type];
      b[1] = 0;
    }
  else
    {
      b[0] = typechar[type / LMAX];
      b[1] = typechar[type % LMAX];
      b[2] = 0;
    }
  return b;
}

void
parse_args (argments &args)
{
  skip_white ();
  if (readc () != '(')
    error ("( expected");
  args.nargs = 0;
  while (1)
    {
      if (!read_ident ())
        error ("unexpected EOF");
      if (*ident)
        {
          if (args.nargs < MAXARGS)
            strcpy (args.name[args.nargs], ident);
          args.nargs++;
        }
      else
        {
          int c = readc ();
          if (c == ')')
            break;
          if (c != ',')
            error (", expected");
        }
    }
}

int
lookup_typechar (int c)
{
  for (int i = 0; typechar[i]; i++)
    if (c == typechar[i])
      return i;
  error ("invalid type char: %c", c);
  return 0;
}

int
lookup_typechar (const char *s, int nargs)
{
  if (strlen (s) != nargs)
    error ("type char expected");
  if (nargs == 1)
    return lookup_typechar (*s);
  return lookup_typechar (*s) * LMAX + lookup_typechar (s[1]);
}

void
print_type (int type)
{
  switch (type)
    {
    case LSHORT:
    case LLONG:
      fprintf (fo, "long ");
      break;
    case LBIGNUM:
      fprintf (fo, "const bignum_rep *");
      break;
    case LFRACT:
      fprintf (fo, "const lfraction *");
      break;
    case LFLOAT_F:
      fprintf (fo, "float ");
      break;
    case LFLOAT_D:
      fprintf (fo, "double ");
      break;
    case LCOMPLEX:
      fprintf (fo, "const lcomplex *");
      break;
    }
}

void
output_head (int type, const char *rettype, const char *name, const argments &args)
{
  fprintf (fo, "static inline %s\n", rettype);
  fprintf (fo, "%s%s (", name, typestring (type, args.nargs));
  for (int i = 0; i < args.nargs; i++)
    {
      if (i)
        fprintf (fo, ", ");
      if (args.nargs == 1)
        print_type (type);
      else if (!i)
        print_type (type / LMAX);
      else
        print_type (type % LMAX);
      fprintf (fo, "%s", args.name[i]);
      fprintf (fo, ", lisp l%s", args.name[i]);
    }
  fprintf (fo, "%s", args.optargs);
  fprintf (fo, ")\n");
  output_line ();
}

void
output_refbody (int type, const char *rettype, const char *name,
                const argments &args)
{
  if (!read_ident ())
    error ("unexpected EOF");
  int ref = lookup_typechar (ident, args.nargs);
  argments refargs;
  strcpy (refargs.optargs, args.optargs);
  strcpy (refargs.optvars, args.optvars);
  parse_args (refargs);
  if (refargs.nargs != args.nargs)
    error ("argments mismatch");
  output_head (type, rettype, name, args);
  fprintf (fo, "{\n");
  fprintf (fo, "  return %s%s (", name, typestring (ref, args.nargs));
  fprintf (fo, "%s, l%s", refargs.name[0], refargs.name[0]);
  for (int i = 1; i < args.nargs; i++)
    fprintf (fo, ", %s, l%s", refargs.name[i], refargs.name[i]);
  fprintf (fo, "%s", refargs.optvars);
  fprintf (fo, ");\n");
  fprintf (fo, "}\n\n");
}

void
output_body (int type, const char *rettype, const char *name,
             const argments &args)
{
  output_head (type, rettype, name, args);
  fprintf (fo, "{");
  int paren = 1;
  int skip_indent = 0;
  while (1)
    {
      int c = readc ();
      if (c == EOF)
        error ("unexpected EOF");
      if (skip_indent-- > 0)
        {
          if (c == ' ') continue;
          skip_indent = 0;
        }
      putc (c, fo);
      if (c == '{')
        paren++;
      if (c == '\n')
        skip_indent = 2 + args.nargs;
      else if (c == '}' && !--paren)
        break;
    }
  fprintf (fo, "\n\n");
}

int
process_body (int *body, const char *rettype, const char *name,
              const argments &args)
{
  if (!read_ident ())
    error ("unexpected EOF");
  if (!*ident)
    {
      if (readc () != '}')
        error ("} expected");
      return 0;
    }
  int type = lookup_typechar (ident, args.nargs);
  skip_white ();
  if (readc () != ':')
    error (": expected");
  read_ident ();
  if (*ident)
    body[type] = lookup_typechar (ident, args.nargs);
  else
    {
      int c = readc ();
      switch (c)
        {
        case EOF:
          error ("unexpected EOF");

        case '@':
          body[type] = type;
          output_refbody (type, rettype, name, args);
          break;

        case '{':
          body[type] = type;
          output_body (type, rettype, name, args);
          break;

        default:
          error ("unexpected %c", c);
        }
    }
  return 1;
}

void
output_call (const int *body, const char *rettype, const char *name,
             const argments &args)
{
  fprintf (fo, "%s\n%s (", rettype, name);
  fprintf (fo, "lisp %s", args.name[0]);
  for (int i = 1; i < args.nargs; i++)
    fprintf (fo, ", lisp %s", args.name[i]);
  fprintf (fo, "%s", args.optargs);
  fprintf (fo, ")\n");
  fprintf (fo, "{\n");
  fprintf (fo, "  switch (number_typeof (%s))\n", args.name[0]);
  fprintf (fo, "    {\n");
  if (args.argtype == AT_INTEGER)
    fprintf (fo, "    default: FEtype_error (%s, Qinteger);\n", args.name[0]);
  else if (args.argtype == AT_RATIONAL)
    fprintf (fo, "    default: FEtype_error (%s, Qrational);\n", args.name[0]);
  else if (args.argtype == AT_REAL)
    fprintf (fo, "    default: FEtype_error (%s, Qreal);\n",
             args.name[0]);
  else
    fprintf (fo, "    default: FEtype_error (%s, Qnumber);\n", args.name[0]);
  fprintf (fo, "      /*NOTREACHED*/\n");
  if (args.nargs == 1)
    {
      for (int i = 0; i < LMAX; i++)
        if (i <= args.argtype)
          fprintf (fo, "    case %s: return %s%s (%s (%s), %s%s);\n",
                   typespec[i].ltype, name, typestring (body[i], 1),
                   typespec[i].fmt, args.name[0], args.name[0], args.optvars);
    }
  else
    {
      for (int i = 0; i < LMAX; i++)
        if (i <= args.argtype)
          {
            fprintf (fo, "    case %s:\n", typespec[i].ltype);
            fprintf (fo, "      switch (number_typeof (%s))\n", args.name[1]);
            fprintf (fo, "        {\n");
            if (args.argtype == AT_INTEGER)
              fprintf (fo, "        default: FEtype_error (%s, Qinteger);\n", args.name[1]);
            else if (args.argtype == AT_RATIONAL)
              fprintf (fo, "        default: FEtype_error (%s, Qrational);\n", args.name[1]);
            else if (args.argtype == AT_REAL)
              fprintf (fo, "        default: FEtype_error (%s, Qreal);\n",
                       args.name[1]);
            else
              fprintf (fo, "        default: FEtype_error (%s, Qnumber);\n", args.name[1]);
            fprintf (fo, "          /*NOTREACHED*/\n");
            for (int j = 0; j < LMAX; j++)
              if (j <= args.argtype)
                fprintf (fo, "        case %s: return %s%s (%s (%s), %s, %s (%s), %s%s);\n",
                         typespec[j].ltype, name, typestring (body[i * LMAX + j], 2),
                         typespec[i].fmt, args.name[0], args.name[0],
                         typespec[j].fmt, args.name[1], args.name[1],
                         args.optvars);
            fprintf (fo, "        }\n");
          }
    }
  fprintf (fo, "    }\n");
  fprintf (fo, "}\n\n");
}

void
read_optargs (argments &args)
{
  *args.optargs = 0;
  *args.optvars = 0;

  skip_white ();
  int c = readc ();
  if (c != '(')
    {
      unreadc (c);
      return;
    }
  args.optargs[0] = ',';
  int n = 1;
  while (1)
    {
      c = readc ();
      if (c == EOF)
        error ("unexpected EOF");
      if (c == ')')
        break;
      args.optargs[n++] = c;
      if (n == sizeof args.optargs)
        error ("optional args too long");
    }
  args.optargs[n] = 0;

  char *b = args.optvars;
  for (char *p0 = args.optargs + 1, *pe;; p0 = pe + 1)
    {
      pe = strchr (p0, ',');
      if (!pe)
        pe = p0 + strlen (p0);
      char *p;
      for (p = pe; p > p0 && (isalnum (p[-1]) || p[-1] == '_'); p--)
        ;
      if (p == pe || p == p0)
        break;
      for (*b++ = ','; p < pe; *b++ = *p++)
        ;
      if (!*p)
        break;
    }
  *b = 0;
}

int
process_proc ()
{
  char rettype[MAXIDSIZE], name[MAXIDSIZE];
  argments args;
  if (!read_ident ())
    return 0;
  if (!*ident)
    error ("return type expected");
  long proclinenum = linenum;
  strcpy (rettype, ident);
  if (!read_ident () || !*ident)
    error ("function name expected");
  strcpy (name, ident);
  parse_args (args);
  if (!args.nargs)
    error ("In function %s: no argments", name);
  if (args.nargs > MAXARGS)
    error ("In function %s: too many argments", name);
  read_optargs (args);
  read_ident ();
  if (!*ident || !strcmp (ident, "number"))
    args.argtype = AT_NUMBER;
  else if (!strcmp (ident, "real"))
    args.argtype = AT_REAL;
  else if (!strcmp (ident, "integer"))
    args.argtype = AT_INTEGER;
  else if (!strcmp (ident, "rational"))
    args.argtype = AT_RATIONAL;
  else
    error ("Invalid argtype: %s", ident);
  skip_white ();
  if (readc () != '{')
    error ("{ expected");

  int body[LMAX * LMAX];
  for (int i = 0; i < LMAX * LMAX; i++)
    body[i] = NOT_DEFINED;

  while (process_body (body, rettype, name, args))
    ;
  for (int i = 0; i < (args.nargs == 1 ? LMAX : LMAX * LMAX); i++)
    {
      if (i % LMAX <= args.argtype && i / LMAX <= args.argtype
          && body[i] == NOT_DEFINED)
        error ("In function %s: %s has no body", name, typestring (i, args.nargs));
    }
  output_line (proclinenum);
  output_call (body, rettype, name, args);
  return 1;
}

void
dpp (int argc, char **argv)
{
  if (argc != 3)
    {
      fprintf (stderr, "too few argments\n");
      exit (2);
    }
  input_file = argv[1];
  output_file = argv[2];
  fi = fopen (input_file, "r");
  if (!fi)
    {
      perror (input_file);
      exit (2);
    }
  fo = fopen (output_file, "w");
  if (!fo)
    {
      perror (output_file);
      exit (2);
    }
  while (process_proc ())
    ;
  fclose (fo);
  fclose (fi);
  exit (0);
}
