#include "gen-stdafx.h"

static const struct {const char *s; int ccs;} escseq[] =
{
  {"\033$@", ccs_jisx0208},
  {"\033$B", ccs_jisx0208},
  {"\033$A", ccs_gb2312},
  {"\033$(@", ccs_jisx0208},
  {"\033$(A", ccs_gb2312},
  {"\033$(B", ccs_jisx0208},
  {"\033$(C", ccs_ksc5601},
  {"\033$(D", ccs_jisx0212},
  {"\033$(G", ccs_cns11643_1},
  {"\033$(H", ccs_cns11643_2},
  {"\033$(0", ccs_big5_1},
  {"\033$(1", ccs_big5_2},
  {"\033(B", ccs_usascii},
  {"\033(J", ccs_usascii},
  {"\033(I", ccs_jisx0201_kana},
  {"\033,A", ccs_iso8859_1},
  {"\033,B", ccs_iso8859_2},
  {"\033,C", ccs_iso8859_3},
  {"\033,D", ccs_iso8859_4},
  {"\033,F", ccs_iso8859_7},
  {"\033,L", ccs_iso8859_5},
  {"\033,M", ccs_iso8859_9},
  {"\033,V", ccs_iso8859_10},
  {"\033,Y", ccs_iso8859_13},
};

static const char *const intermediate_chars[] =
{
  "()*+",
  ",-./",
};

#define STATE_INVALID 0x80
#define STATE_TERM 0x40
#define MAX_STATE 64

void
gen_iso2022state (int argc, char **argv)
{
  u_char chars_buf[257], *const chars = chars_buf + 1;
  u_char chars_rev[256];
  int chars_max = 1;

  memset (chars_buf, 0, sizeof chars_buf);
  for (int i = 0; i < numberof (escseq); i++)
    for (const char *p = escseq[i].s; *p; p++)
      if (!chars[*p])
        {
          chars[*p] = chars_max;
          chars_rev[chars_max] = *p;
          chars_max++;
        }

  u_char state[MAX_STATE][256];
  memset (state, 0, sizeof state);
  int state_max = 0;

  for (int i = 0; i < numberof (escseq); i++)
    {
      int cur_state = 0;
      for (const char *p = escseq[i].s; *p; p++)
        {
          int c = chars[*p];
          int next_state;
          if (!state[c][cur_state] || state[c][cur_state] & STATE_INVALID)
            next_state = p[1] ? ++state_max : 0;
          else
            next_state = state[c][cur_state];

          if (state_max == MAX_STATE)
            {
              fprintf (stderr, "Too many states\n");
              exit (2);
            }

          if (!next_state)
            state[c][cur_state] = STATE_TERM | escseq[i].ccs;
          else
            state[c][cur_state] = next_state;

          for (c = 0; c < chars_max; c++)
            if (!state[c][cur_state])
              state[c][cur_state] = STATE_INVALID;
          cur_state = next_state;
        }
    }

  for (int i = 0; i < numberof (escseq); i++)
    {
      int c = chars[*escseq[i].s];
      for (int j = 1; j <= state_max; j++)
        if (state[c][j] == STATE_INVALID)
          state[c][j] |= state[c][0];
    }

  for (int i = 0; i < numberof (intermediate_chars); i++)
    for (const char *p = intermediate_chars[i] + 1; *p; p++)
      chars[*p] = chars[intermediate_chars[i][0]];

  printf ("#define ISO2022STATE_TERM 0x%02x\n", STATE_TERM);
  printf ("#define ISO2022STATE_MASK 0x%02x\n", STATE_TERM - 1);
  printf ("\n");

  printf ("static const u_char iso2022state_chars[] =\n{");
  for (int i = 0; i < 256; i++)
    {
      if (!(i % 16))
        printf ("\n  ");
      printf ("%d,", chars[i]);
    }
  printf ("\n};\n\n");

  printf ("static const u_char iso2022state[][%d] =\n{\n", state_max + 1);
  for (int i = 0; i < chars_max; i++)
    {
      printf ("  {");
      for (int j = 0; j <= state_max; j++)
        printf ("0x%02x,", state[i][j] & ~STATE_INVALID & 0xff);
      printf ("},\n");
    }
  printf ("};\n");
  exit (0);
}
