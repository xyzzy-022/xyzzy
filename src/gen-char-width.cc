#include "gen-stdafx.h"
#include "cdecl.h"
#include "charset.h"
#include "jisx0212-width.h"

static void
print (const u_char *width)
{
  printf ("u_char char_width_table[] =\n{\n");
  for (int i = 0; i < 65536 / 8; i += 8)
    {
      putchar (' ');
      for (int j = 0; j < 8; j++)
        printf (" 0x%02x,", width[i + j]);
      putchar ('\n');
    }
  printf ("};\n");
}

void
gen_char_width (int argc, char **argv)
{
#define ON8(X) (width[(X) / 8] = 255)
#define ON(X) (width[(X) / 8] |= 1 << ((X) % 8))
#define OFF(X) (width[(X) / 8] &= ~(1 << ((X) % 8)))
  u_char width[65536 / 8];
  memset (width, 0, sizeof width);
  for (int i = 0; i < 32; i += 8)
    ON8 (i);
  ON (127);

  for (int i = (ccs_iso8859_13 << 7) + 128; i < 0x10000; i += 8)
    ON8 (i);

  memcpy (&width[CCS_JISX0212_MIN / 8], jisx0212_width_table,
          sizeof jisx0212_width_table);

  for (int i = 0; i < 256; i++)
    {
      OFF (CCS_UTF16_UNDEF_CHAR_HIGH + i);
      OFF (CCS_UTF16_UNDEF_CHAR_LOW + i);
    }

  for (int i = CCS_UTF16_SURROGATE_HIGH_MIN; i <= CCS_UTF16_SURROGATE_LOW_MAX; i++)
    OFF (i);

  /* Basic Georgian & Georgian Extended */
  for (int i = CCS_GEORGIAN_MIN; i < ((CCS_GEORGIAN_MAX + 127) & ~127); i++)
    OFF (i);
  /* IPA Extensions */
  for (int i = CCS_IPA_MIN; i < ((CCS_IPA_MAX + 127) & ~127); i++)
    OFF (i);
  /* Spacing Modifier Letters & Combining Diacritical Marks */
  for (int i = CCS_SMLCDM_MIN; i < ((CCS_SMLCDM_MAX + 127) & ~127); i++)
    OFF (i);
#ifdef CCS_ULATIN_MIN
  for (int i = CCS_ULATIN_MIN; i < ((CCS_ULATIN_MAX + 127) & ~127); i++)
    OFF (i);
#endif
#ifdef CCS_UJP_MIN
  for (int i = CCS_UJP_HALF_MIN; i < ((CCS_UJP_HALF_MAX + 127) & ~127); i++)
    OFF (i);
#endif

  print (width);

  exit (0);
}
