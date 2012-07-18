#include "gen-stdafx.h"
#include "ucs2tab.h"

#define SZ 0x500

static void
print (const u_char *width)
{
  printf ("static const u_char jisx0212_width_table[] =\n{\n");
  for (int i = 0; i < SZ / 8; i += 8)
    {
      putchar (' ');
      for (int j = 0; j < 8; j++)
        printf (" 0x%02x,", width[i + j]);
      putchar ('\n');
    }
  printf ("};\n");
}

void
gen_jisx0212_width (int argc, char **argv)
{
  HDC hdc = GetDC (0);
  LOGFONT lf;
  memset (&lf, 0, sizeof lf);
  lf.lfHeight = 16;
  lf.lfCharSet = SHIFTJIS_CHARSET;
  strcpy (lf.lfFaceName, "‚l‚r –¾’©");
  HGDIOBJ of = SelectObject (hdc, CreateFontIndirect (&lf));

  SIZE sz0;
  GetTextExtentPoint32 (hdc, "‚ ", 2, &sz0);

  u_char width[SZ / 8];
  memset (width, 255, sizeof width);
  for (int i = CCS_JISX0212_MIN; i < CCS_JISX0212_MIN + SZ; i++)
    {
      SIZE sz;
      ucs2_t wc = internal2wc_table[i];
      if (wc != ucs2_t (-1))
        {
          GetTextExtentPoint32W (hdc, &wc, 1, &sz);
          if (sz.cx != sz0.cx)
            width[(i - CCS_JISX0212_MIN) >> 3] &= ~(1 << (i & 7));
        }
    }
  print (width);

  DeleteObject (SelectObject (hdc, of));
  ReleaseDC (0, hdc);

  exit (0);
}
