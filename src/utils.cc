#include "stdafx.h"
#include "cdecl.h"
#include "ed.h"
#include "utils.h"
#include "chtype.h"

void *
xmalloc (size_t size)
{
  if (!size)
    size = 1;
  void *p = malloc (size);
  if (!p)
    FEstorage_error ();
  return p;
}

void *
xrealloc (void *p, size_t size)
{
  if (!size)
    size = 1;
  if (!p)
    p = malloc (size);
  else
    p = realloc (p, size);
  if (!p)
    FEstorage_error ();
  return p;
}

void
xfree (void *p)
{
  if (p)
    free (p);
}

char *
xstrdup (const char *s)
{
  return strcpy ((char *)xmalloc (strlen (s) + 1), s);
}

void *
xmemdup (const void *p, size_t size)
{
  return memcpy (xmalloc (size), p, size);
}

char *
stpcpy (char *d, const char *s)
{
  while ((*d++ = *s++))
    ;
  return d - 1;
}

char *
stpncpy (char *d, const char *s, int n)
{
  for (; n > 0; n--)
  {
    if (!(*d++ = *s++))
      return d - 1;
  }
  *d = 0;
  return d;
}

char *
jindex (const char *p, int c)
{
  for (const u_char *s = (const u_char *)p; *s;)
    {
      if (SJISP (*s) && s[1])
        s += 2;
      else
        {
          if (*s == c)
            return (char *)s;
          s++;
        }
    }
  return 0;
}

char *
jrindex (const char *p, int c)
{
  const u_char *save, *s;
  for (save = 0, s = (const u_char *)p; *s;)
    {
      if (SJISP (*s) && s[1])
        s += 2;
      else
        {
          if (*s == c)
            save = s;
          s++;
        }
    }
  return (char *)save;
}

char *
find_slash (const char *p)
{
  for (u_char *s = (u_char *)p; *s;)
    {
      if (SJISP (*s) && s[1])
        s += 2;
      else
        {
          if (*s == '/' || *s == '\\')
            return (char *)s;
          s++;
        }
    }
  return 0;
}

char *
find_last_slash (const char *p)
{
  u_char *save, *s;
  for (save = 0, s = (u_char *)p; *s;)
    {
      if (SJISP (*s) && s[1])
        s += 2;
      else
        {
          if (*s == '/' || *s == '\\')
            save = s;
          s++;
        }
    }
  return (char *)save;
}

long
log2 (u_long x)
{
  long l;
  for (l = 0; x; x >>= 1, l++)
    ;
  return l;
}

#define NF_SIGN 1
#define NF_LEADNUM 2
#define NF_DOT 4
#define NF_TRAILNUM 8
#define NF_EXPSIGN 16
#define NF_EXP (NF_EXPCHAR | NF_EXPNUM)
#define  NF_EXPCHAR 32
#define  NF_EXPNUM 64
#define NF_SLASH 128

int
default_float_format ()
{
  lisp f = xsymbol_value (Vread_default_float_format);
  if (f == Qshort_float)
    return NF_FLOAT_S;
  if (f == Qdouble_float)
    return NF_FLOAT_D;
  if (f == Qlong_float)
    return NF_FLOAT_L;
  return NF_FLOAT_F;
}

int
parse_number_format (const Char *p, const Char *pe, int base)
{
  Char expchar = 'e';
  int f = 0;
  if (p < pe && (*p == '+' || *p == '-'))
    p++;
  if (p < pe && digit_char (*p) < base)
    {
      f |= NF_LEADNUM;
      for (p++; p < pe && digit_char (*p) < base; p++)
        ;
    }
  if (p < pe && *p == '.')
    {
      f |= NF_DOT;
      p++;
    }
  if (p < pe && *p == '/')
    {
      f |= NF_SLASH;
      p++;
    }
  if (p < pe && digit_char (*p) < base)
    {
      f |= NF_TRAILNUM;
      for (p++; p < pe && digit_char (*p) < base; p++)
        ;
    }
  if (p < pe)
    {
      Char c = char_downcase (*p);
      switch (c)
        {
        case 'e':
        case 's':
        case 'f':
        case 'd':
        case 'l':
          f |= NF_EXPCHAR;
          expchar = c;
          p++;
        }
    }
  if (p < pe && (*p == '+' || *p == '-'))
    {
      f |= NF_EXPSIGN;
      p++;
    }
  if (p < pe && digit_char_p (*p))
    {
      f |= NF_EXPNUM;
      for (p++; p < pe && digit_char_p (*p); p++)
        ;
    }

  if ((f & (NF_EXP | NF_EXPSIGN)) == NF_EXPSIGN)
    return NF_BAD;
  f &= ~NF_EXPSIGN;

  if (p == pe)
    {
      switch (f)
        {
        case NF_LEADNUM:
          return NF_INTEGER;

        case NF_LEADNUM | NF_DOT:
          return NF_INTEGER_DOT;

        case NF_LEADNUM | NF_SLASH | NF_TRAILNUM:
          return NF_FRACTION;

        case NF_DOT | NF_TRAILNUM:
        case NF_DOT | NF_TRAILNUM | NF_EXP:
        case NF_LEADNUM | NF_DOT | NF_TRAILNUM:
        case NF_LEADNUM | NF_DOT | NF_TRAILNUM | NF_EXP:
        case NF_LEADNUM | NF_EXP:
        case NF_LEADNUM | NF_DOT | NF_EXP:
          return NF_FLOAT | expchar;
        }
    }
  return NF_BAD;
}

int
check_integer_format (const char *s, int *n)
{
  Char *b = (Char *)alloca (strlen (s) * 2);
  Char *be = s2w (b, s);
  for (; b < be && (*b == ' ' || *b == '\t'); b++)
    ;
  for (; be > b && (b[-1] == ' ' || b[-1] == '\t'); b--)
    ;

  switch (parse_number_format (b, be, 10))
    {
    case NF_INTEGER:
    case NF_INTEGER_DOT:
      *n = atoi (s);
      return 1;

    default:
      return 0;
    }
}

int
streq (const Char *p, int l, const char *s)
{
  for (const Char *pe = p + l; p < pe; p++, s++)
    if (*p != *s)
      return 0;
  return 1;
}

int
strequal (const char *cp, const Char *Cp)
{
  while (*cp)
    {
      Char c = *Cp++;
      if (DBCP (c))
        {
          if (!cp[1] || c != Char ((u_char (*cp) << 8) | u_char (cp[1])))
            return 0;
          cp += 2;
        }
      else
        {
          if (char_downcase (c) != char_downcase (u_char (*cp)))
            return 0;
          cp++;
        }
    }
  return 1;
}

int
strequal (const char *cp, const Char *Cp, int l)
{
  for (const Char *Ce = Cp + l; Cp < Ce; Cp++)
    {
      Char c = *Cp;
      if (DBCP (c))
        {
          if (c != Char ((u_char (*cp) << 8) | u_char (cp[1])))
            return 0;
          cp += 2;
        }
      else
        {
          if (char_downcase (c) != char_downcase (u_char (*cp)))
            return 0;
          cp++;
        }
    }
  return 1;
}

int
strcasecmp (const char *s1, const char *s2)
{
  const u_char *p1 = (const u_char *)s1;
  const u_char *p2 = (const u_char *)s2;
  while (1)
    {
      u_char c1 = *p1++;
      u_char c2 = *p2++;
      if (SJISP (c1))
        {
          if (c1 != c2)
            return c1 - c2;
          c1 = *p1++;
          c2 = *p2++;
        }
      else
        {
          c1 = char_downcase (c1);
          c2 = char_downcase (c2);
        }
      if (c1 != c2)
        return c1 - c2;
      if (!c1)
        return 0;
    }
}

void
convert_backsl_with_sl (char *path, int f, int t)
{
  for (u_char *s = (u_char *)path; *s;)
    {
      if (SJISP (*s) && s[1])
        s += 2;
      else
        {
          if (*s == f)
            *s = t;
          s++;
        }
    }
}

void
fill_rect (HDC hdc, const RECT &r, COLORREF c)
{
  COLORREF oc = SetBkColor (hdc, c);
  ExtTextOut (hdc, 0, 0, ETO_OPAQUE, &r, 0, 0, 0);
  SetBkColor (hdc, oc);
}

void
fill_rect (HDC hdc, int x, int y, int cx, int cy, COLORREF c)
{
  RECT r;
  r.left = x;
  r.top = y;
  r.right = x + cx;
  r.bottom = y + cy;
  COLORREF oc = SetBkColor (hdc, c);
  ExtTextOut (hdc, 0, 0, ETO_OPAQUE, &r, 0, 0, 0);
  SetBkColor (hdc, oc);
}

void
draw_hline (HDC hdc, int x1, int x2, int y, COLORREF c)
{
  RECT r;
  r.left = x1;
  r.top = y;
  r.right = x2;
  r.bottom = y + 1;
  COLORREF oc = SetBkColor (hdc, c);
  ExtTextOut (hdc, 0, 0, ETO_OPAQUE, &r, 0, 0, 0);
  SetBkColor (hdc, oc);
}

void
draw_vline (HDC hdc, int y1, int y2, int x, COLORREF c)
{
  RECT r;
  r.left = x;
  r.top = y1;
  r.right = x + 1;
  r.bottom = y2;
  COLORREF oc = SetBkColor (hdc, c);
  ExtTextOut (hdc, 0, 0, ETO_OPAQUE, &r, 0, 0, 0);
  SetBkColor (hdc, oc);
}

#if 0
void
paint_button_off (HDC hdc, const RECT &r)
{
  HGDIOBJ open = SelectObject (hdc, CreatePen (PS_SOLID, 0, sysdep.btn_highlight));
  MoveToEx (hdc, r.left, r.bottom - 1, 0);
  LineTo (hdc, r.left, r.top);
  LineTo (hdc, r.right - 1, r.top);
  DeleteObject (SelectObject (hdc, open));

  open = SelectObject (hdc, sysdep.hpen_black);
  LineTo (hdc, r.right - 1, r.bottom - 1);
  LineTo (hdc, r.left - 1, r.bottom - 1);
  SelectObject (hdc, open);

  open = SelectObject (hdc, CreatePen (PS_SOLID, 0, sysdep.btn_shadow));
  MoveToEx (hdc, r.left + 1, r.bottom - 2, 0);
  LineTo (hdc, r.right - 2, r.bottom - 2);
  LineTo (hdc, r.right - 2, r.top);
  DeleteObject (SelectObject (hdc, open));
}

void
paint_button_on (HDC hdc, const RECT &r)
{
  HGDIOBJ open = SelectObject (hdc, sysdep.hpen_black);
  MoveToEx (hdc, r.left, r.bottom - 1, 0);
  LineTo (hdc, r.left, r.top);
  LineTo (hdc, r.right - 1, r.top);
  SelectObject (hdc, open);

  open = SelectObject (hdc, CreatePen (PS_SOLID, 0, sysdep.btn_highlight));
  LineTo (hdc, r.right - 1, r.bottom - 1);
  LineTo (hdc, r.left - 1, r.bottom - 1);
  DeleteObject (SelectObject (hdc, open));

  open = SelectObject (hdc, CreatePen (PS_SOLID, 0, sysdep.btn_shadow));
  MoveToEx (hdc, r.left + 1, r.bottom - 3, 0);
  LineTo (hdc, r.left + 1, r.top + 1);
  LineTo (hdc, r.right - 2, r.top + 1);
  DeleteObject (SelectObject (hdc, open));

  SetPixel (hdc, r.left + 1, r.bottom - 2, sysdep.btn_face);
  SetPixel (hdc, r.right -2, r.top + 1, sysdep.btn_face);
}
#else
void
paint_button_off (HDC hdc, const RECT &r)
{
  draw_vline (hdc, r.top, r.bottom - 1, r.left, sysdep.btn_highlight);
  draw_hline (hdc, r.left, r.right - 1, r.top, sysdep.btn_highlight);
  draw_vline (hdc, r.top, r.bottom, r.right - 1, sysdep.btn_shadow);
  draw_hline (hdc, r.left, r.right, r.bottom - 1, sysdep.btn_shadow);
}

void
paint_button_on (HDC hdc, const RECT &r)
{
  draw_vline (hdc, r.top, r.bottom - 1, r.left, sysdep.btn_shadow);
  draw_hline (hdc, r.left, r.right - 1, r.top, sysdep.btn_shadow);
  draw_vline (hdc, r.top, r.bottom, r.right - 1, sysdep.btn_highlight);
  draw_hline (hdc, r.left, r.right, r.bottom - 1, sysdep.btn_highlight);
}
#endif

frameDC::frameDC (HWND hwnd, int flags)
     : f_hwnd (hwnd)
{
  f_hdc = GetDCEx (f_hwnd, 0,
                   flags | DCX_CACHE | (LockWindowUpdate (f_hwnd)
                                        ? DCX_LOCKWINDOWUPDATE : 0));
  HBITMAP hbm = LoadBitmap (app.hinst, MAKEINTRESOURCE (IDB_CHECK));
  f_obr = SelectObject (f_hdc, CreatePatternBrush (hbm));
  DeleteObject (hbm);
}

frameDC::~frameDC ()
{
  DeleteObject (SelectObject (f_hdc, f_obr));
  LockWindowUpdate (0);
  ReleaseDC (f_hwnd, f_hdc);
}

void
frameDC::frame_rect (const RECT &r, int w) const
{
  HRGN hrgn1 = CreateRectRgnIndirect (&r);
  HRGN hrgn2 = CreateRectRgn (r.left + w, r.top + w,
                              r.right - w, r.bottom - w);
  CombineRgn (hrgn1, hrgn1, hrgn2, RGN_XOR);
  DeleteObject (hrgn2);
  SelectClipRgn (f_hdc, hrgn1);
  DeleteObject (hrgn1);
  paint (r);
  SelectClipRgn (f_hdc, 0);
}

ucs2_t *
i2w (const Char *p, int l, ucs2_t *b)
{
  for (const Char *const pe = p + l; p < pe; p++)
    {
      ucs2_t c = i2w (*p);
      if (c == ucs2_t (-1))
        {
          if (utf16_undef_char_high_p (*p) && p < pe - 1
              && utf16_undef_char_low_p (p[1]))
            {
              c = utf16_undef_pair_to_ucs2 (*p, p[1]);
              p++;
            }
          else
            c = DEFCHAR;
        }
      *b++ = c;
    }
  *b = 0;
  return b;
}

int
i2wl (const Char *p, int l)
{
  int r = 0;
  for (const Char *const pe = p + l; p < pe; p++, r++)
    {
      ucs2_t c = i2w (*p);
      if (c == ucs2_t (-1)
          && utf16_undef_char_high_p (*p) && p < pe - 1
          && utf16_undef_char_low_p (p[1]))
        p++;
    }
  return r + 1;
}

