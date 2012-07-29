#include "gen-stdafx.h"
#define NOT_COMPILE_TIME
#include "chtype.h"

static void
dump (const char *b, int size, int hex = 1)
{
  for (int i = 0; i < size; i += 8)
    {
      printf (" ");
      for (int j = 0; j < 8; j++)
        if (hex)
          printf (" 0x%02x,", *b++ & 0xff);
        else
          printf (" %d,", *b++);
      printf ("\n");
    }
}

static void
ctype ()
{
  char buf[256];
  bzero (buf, sizeof buf);
  for (int i = '0'; i <= '9'; i++)
    buf[i] |= _CTN;
  for (int i = 'A'; i <= 'Z'; i++)
    buf[i] |= _CTU;
  for (int i = 'a'; i <= 'z'; i++)
    buf[i] |= _CTL;
  for (int i = 0xa1; i <= 0xdf; i++)
    buf[i] |= _CTK;
  for (int i = 0x81; i <= 0x9f; i++)
    buf[i] |= _CTK1;
  for (int i = 0xe0; i <= 0xfc; i++)
    buf[i] |= _CTK1;
  for (int i = 0x40; i <= 0x7e; i++)
    buf[i] |= _CTK2;
  for (int i = 0x80; i <= 0xfc; i++)
    buf[i] |= _CTK2;

  printf ("unsigned char char_type_table[] =\n");
  printf ("{\n");
  printf ("  0,\n");
  dump (buf, sizeof buf);
  printf ("};\n\n");
}

static void
translate ()
{
  char buf[256];
  for (int i = 0; i < 256; i++)
    buf[i] = i;
  for (int i = 'a'; i <= 'z'; i++)
    buf[i] = i - 'a' + 'A';

  printf ("unsigned char char_translate_upcase_table[] =\n");
  printf ("{\n");
  dump (buf, sizeof buf);
  printf ("};\n\n");

  for (int i = 0; i < 256; i++)
    buf[i] = i;
  for (int i = 'A'; i <= 'Z'; i++)
    buf[i] = i - 'A' + 'a';

  printf ("unsigned char char_translate_downcase_table[] =\n");
  printf ("{\n");
  dump (buf, sizeof buf);
  printf ("};\n\n");

  for (int i = 0; i < 256; i++)
    buf[i] = i;

  printf ("unsigned char char_no_translate_table[] =\n");
  printf ("{\n");
  dump (buf, sizeof buf);
  printf ("};\n\n");
}

static void
numeric ()
{
  char buf[128];
  for (int i = 0; i < 128; i++)
    buf[i] = 36;
  for (int i = '0'; i <= '9'; i++)
    buf[i] = i - '0';
  for (int i = 'A'; i <= 'Z'; i++)
    buf[i] = i - 'A' + 10;
  for (int i = 'a'; i <= 'z'; i++)
    buf[i] = i - 'a' + 10;

  printf ("char char_numeric_table[] =\n");
  printf ("{\n");
  dump (buf, sizeof buf, 0);
  printf ("};\n\n");
}

static void
ctlchars ()
{
  char buf[128], buf2[256];
  memset (buf, 0, sizeof buf);
  memset (buf2, 0, sizeof buf2);
  buf['!'] = static_cast<byte> (CCF_EXCLAM & 0xFF);
  buf['"'] = static_cast<byte> (CCF_DQUOTE & 0xFF);
  buf['#'] = static_cast<byte> (CCF_NUMBER & 0xFF);
  buf['$'] = static_cast<byte> (CCF_DOLLAR & 0xFF);
  buf['%'] = static_cast<byte> (CCF_PERCENT & 0xFF);
  buf['&'] = static_cast<byte> (CCF_AMPER & 0xFF);
  buf['\''] = static_cast<byte> (CCF_QUOTE & 0xFF);
  buf['('] = static_cast<byte> (CCF_LPAREN & 0xFF);
  buf[')'] = static_cast<byte> (CCF_RPAREN & 0xFF);
  buf['*'] = static_cast<byte> (CCF_ASTER & 0xFF);
  buf['+'] = static_cast<byte> (CCF_PLUS & 0xFF);
  buf[','] = static_cast<byte> (CCF_COMMA & 0xFF);
  buf['-'] = static_cast<byte> (CCF_MINUS & 0xFF);
  buf['.'] = static_cast<byte> (CCF_DOT & 0xFF);
  buf['/'] = static_cast<byte> (CCF_SLASH & 0xFF);
  buf['0'] = static_cast<byte> (CCF_0 & 0xFF);
  buf['1'] = static_cast<byte> (CCF_1 & 0xFF);
  buf['2'] = static_cast<byte> (CCF_2 & 0xFF);
  buf['3'] = static_cast<byte> (CCF_3 & 0xFF);
  buf['4'] = static_cast<byte> (CCF_4 & 0xFF);
  buf['5'] = static_cast<byte> (CCF_5 & 0xFF);
  buf['6'] = static_cast<byte> (CCF_6 & 0xFF);
  buf['7'] = static_cast<byte> (CCF_7 & 0xFF);
  buf['8'] = static_cast<byte> (CCF_8 & 0xFF);
  buf['9'] = static_cast<byte> (CCF_9 & 0xFF);
  buf[':'] = static_cast<byte> (CCF_COLON & 0xFF);
  buf[';'] = static_cast<byte> (CCF_SEMI & 0xFF);
  buf['<'] = static_cast<byte> (CCF_LT & 0xFF);
  buf['='] = static_cast<byte> (CCF_EQ & 0xFF);
  buf['>'] = static_cast<byte> (CCF_GT & 0xFF);
  buf['`'] = static_cast<byte> (CCF_BACKQ & 0xFF);
  buf['{'] = static_cast<byte> (CCF_LBRACE & 0xFF);
  buf['|'] = static_cast<byte> (CCF_VER & 0xFF);
  buf['}'] = static_cast<byte> (CCF_RBRACE & 0xFF);
  buf['~'] = static_cast<byte> (CCF_TILDE & 0xFF);

  for (int i = 0; i < sizeof buf; i++)
    if (buf[i])
      buf2[buf[i] & 0xff] = i;

  printf ("unsigned char pseudo_char2ctl_table[] =\n");
  printf ("{\n");
  dump (buf, sizeof buf);
  printf ("};\n\n");

  printf ("unsigned char pseudo_ctl2char_table[] =\n");
  printf ("{\n");
  dump (buf2, sizeof buf2);
  printf ("};\n\n");
}

static void
b64tab ()
{
  char buf[128];
  memset (buf, 65, sizeof buf);

  for (int i = 'A'; i <= 'Z'; i++)
    buf[i] = i - 'A';
  for (int i = 'a'; i <= 'z'; i++)
    buf[i] = i - 'a' + 26;
  for (int i = '0'; i <= '9'; i++)
    buf[i] = i - '0' + 52;
  buf['+'] = 62;
  buf['/'] = 63;
  buf['='] = 64;

  printf ("unsigned char base64_decode_table[] =\n");
  printf ("{\n");
  dump (buf, sizeof buf);
  printf ("};\n\n");

  buf['='] = 65;
  buf['/'] = 65;
  buf[','] = 63;

  printf ("unsigned char imap4_base64_decode_table[] =\n");
  printf ("{\n");
  dump (buf, sizeof buf);
  printf ("};\n\n");
}

static void
utf7tab ()
{
  char buf[128];
  memset (buf, 0, sizeof buf);

  for (int i = 'A'; i <= 'Z'; i++)
    buf[i] = UTF7_SET_D | UTF7_SET_B;
  for (int i = 'a'; i <= 'z'; i++)
    buf[i] = UTF7_SET_D | UTF7_SET_B;
  for (int i = '0'; i <= '9'; i++)
    buf[i] = UTF7_SET_D | UTF7_SET_B;
  for (const char *p = "'(),-./:?"; *p; p++)
    buf[*p] = UTF7_SET_D;

  for (const char *p = "!\"#$%&*;<=>@[]^_`{|}"; *p; p++)
    buf[*p] = UTF7_SET_O;

  buf['+'] |= UTF7_SET_B;
  buf['/'] |= UTF7_SET_B;

  buf[' '] = UTF7_WHITE;
  buf['\t'] = UTF7_WHITE;
  buf['\r'] = UTF7_WHITE;
  buf['\n'] = UTF7_WHITE;

  for (int i = 0x20; i <= 0x25; i++)
    buf[i] |= UTF7_IMAP4_MAILBOX_NAME;
  for (int i = 0x27; i <= 0x7e; i++)
    buf[i] |= UTF7_IMAP4_MAILBOX_NAME;

  buf['+'] |= UTF7_SHIFT_CHAR;
  buf['&'] |= UTF7_IMAP4_SHIFT_CHAR;

  printf ("unsigned char utf7_set_table[] =\n");
  printf ("{\n");
  dump (buf, sizeof buf);
  printf ("};\n\n");
}

static void
hqxtab ()
{
  char buf[128];
  memset (buf, 64, sizeof buf);

  for (int i = 0; i < 64; i++)
    buf["!\"#$%&'()*+,-012345689@ABCDEFGHIJKLMNPQRSTUVXYZ[`abcdefhijklmpqr"[i]] = i;

  printf ("unsigned char hqx_decode_table[] =\n");
  printf ("{\n");
  dump (buf, sizeof buf);
  printf ("};\n\n");
}

void
gen_ctab (int argc, char **argv)
{
  ctype ();
  translate ();
  numeric ();
  ctlchars ();
  b64tab ();
  utf7tab ();
  hqxtab ();
  exit (0);
}

