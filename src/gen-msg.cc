#include "gen-stdafx.h"
#include "chtype.h"

#include "chtab.cc"

struct msgdef
{
  const char *ident;
  const char *text;
};

#define MSG(a, b) {#a, b}

static const msgdef msg[] =
{
#include "msgdef.h"
};

static void
print_quote (const char *p)
{
  while (*p)
    {
      if ((*p & 0xff) < ' ')
        printf ("\\%03o", *p++);
      else
        {
          if (*p == '\\' || *p == '"')
            putchar ('\\');
          putchar (*p);
          if (SJISP (*p++ & 0xff))
            putchar (*p++);
        }
    }
}

static void
print_quote_rc (const char *p)
{
  while (*p)
    {
      if ((*p & 0xff) < ' ')
        printf ("\\%03o", *p++);
      else
        {
          if (*p == '\\' || *p == '"')
            putchar (*p);
          putchar (*p);
          if (SJISP (*p++ & 0xff))
            putchar (*p++);
        }
    }
}

void
gen_msg (int argc, char **argv)
{
  if (argc == 1)
    exit (2);
  if (!strcmp (argv[1], "-def"))
    {
      for (int i = 0; i < numberof (msg); i++)
        printf ("#define %s %d\n", msg[i].ident, i);
    }
  else if (!strcmp (argv[1], "-enum"))
    {
      printf ("enum message_code\n{\n");
      int i;
      for (i = 0; i < numberof (msg) - 1; i++)
        printf ("  %s,\n", msg[i].ident);
      printf ("  %s\n", msg[i].ident);
      printf ("};\n");
    }
  else if (!strcmp (argv[1], "-c"))
    {
      printf ("const char SSM[] =\n");
      for (int i = 0; i < numberof (msg); i++)
        {
          printf ("  \"");
          print_quote (msg[i].text);
          printf ("\\0\"\n");
        }
      printf (";\n\n");


      printf ("static const char *const message_string[] =\n");
      printf ("{\n");
      int l = 0;
      for (int i = 0; i < numberof (msg); i++)
        {
          printf ("  SSM + %d,\n", l);
          l += strlen (msg[i].text) + 1;
        }
      printf ("};\n\n");

      printf ("const char *\n"
              "get_message_string (int code)\n"
              "{return message_string[code];}\n");
    }
  else if (!strcmp (argv[1], "-stbl"))
    {
      printf ("STRINGTABLE DISCARDABLE\n");
      printf ("BEGIN\n");
      for (int i = 0; i < numberof (msg); i++)
        {
          printf ("  %d \"", i + 1024);
          print_quote_rc (msg[i].text);
          printf ("\"\n");
        }
      printf ("END\n");
    }
  else if (!strcmp (argv[1], "-rc"))
    {
      printf ("#include \"ed.h\"\n"
              "const char *\n"
              "get_message_string (int code)\n"
              "{\n"
              "  static char buf[256];\n"
              "  if (!LoadString (app.hinst, 1024 + code, buf, sizeof buf))\n"
              "    sprintf (buf, \"String resource %%d not found.\", code);\n"
              "  return buf;\n"
              "}\n");
    }
  exit (0);
}
