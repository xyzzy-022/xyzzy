#include "gen-stdafx.h"

#define HEIGHT 128
#define FACE "Lucida Sans Unicode"

static void
print (const ABC *abc, int n)
{
  printf ("#define LUCIDA_BASE_HEIGHT %d\n", HEIGHT);
  printf ("#define LUCIDA_FACE_NAME \"%s\"\n\n", FACE);
  printf ("#define LUCIDA_SPACING 6\n");
  printf ("struct lucida_spacing {char a; u_char b;};\n");
  printf ("#define LUCIDA_OFFSET(C) \\\n"
          "  (-lucida_spacing_table[(C)].a - lucida_spacing_table[(C)].b / 2)\n");
  printf ("#ifdef DEFINE_LUCIDA_OFFSET_TABLE\n");
  printf ("lucida_spacing lucida_spacing_table[] =\n{");
  for (int i = 0; i < n; i++)
    {
      if (!(i % 8))
        printf ("\n ");
      printf (" {%d,%u},", abc[i].abcA, abc[i].abcB);
    }
  printf ("\n};\n");
  printf ("#else\n");
  printf ("extern lucida_spacing lucida_spacing_table[];\n");
  printf ("#endif\n");
}

void
gen_lucida_width (int argc, char **argv)
{
  HDC hdc = GetDC (0);
  LOGFONT lf;
  memset (&lf, 0, sizeof lf);
  lf.lfHeight = HEIGHT;
  strcpy (lf.lfFaceName, FACE);
  HGDIOBJ of = SelectObject (hdc, CreateFontIndirect (&lf));

  ABC abc[UNICODE_SMLCDM_MAX - UNICODE_SMLCDM_MIN + 1];
  if (!GetCharABCWidthsW (hdc, UNICODE_SMLCDM_MIN, UNICODE_SMLCDM_MAX, abc))
    {
      fprintf (stderr, "GetCharABCWidthsW: %d\n", GetLastError ());
      exit (2);
    }

  DeleteObject (SelectObject (hdc, of));
  ReleaseDC (0, hdc);

  /* fix bug? */
  abc[0x332 - UNICODE_SMLCDM_MIN] =
    abc[0x333 - UNICODE_SMLCDM_MIN] =
      abc[0x305 - UNICODE_SMLCDM_MIN];

  for (int i = 0; i < numberof (abc); i++)
    {
      if (abc[i].abcA < SCHAR_MIN || abc[i].abcA > SCHAR_MAX)
        {
          fprintf (stderr, "%04x: A overflow: %d\n",
                   i+ UNICODE_SMLCDM_MIN, abc[i].abcA);
          exit (2);
        }
      if (abc[i].abcB >= UCHAR_MAX)
        {
          fprintf (stderr, "%04x: B overflow: %d\n",
                   i+ UNICODE_SMLCDM_MIN, abc[i].abcB);
          exit (2);
        }
    }

  print (abc, numberof (abc));

  exit (0);
}
