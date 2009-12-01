#ifndef _xcolor_h_
# define _xcolor_h_

class XCOLORREF
{
public:
  COLORREF rgb;

  XCOLORREF () {}
  XCOLORREF (const XCOLORREF &xc) : rgb (xc.rgb) {}
  XCOLORREF (COLORREF c) : rgb (c) {}
  XCOLORREF (BYTE r, BYTE g, BYTE b, int i)
       : rgb (RGB (r, g, b) | ((i + 1) << 24)) {}
  XCOLORREF (COLORREF c, int i) : rgb (c | ((i + 1) << 24)) {}
  int syscolor_index () const
    {
      return (rgb & 0xff000000
              ? (DWORD (rgb) >> 24) - 1
              : -1);
    }
  operator COLORREF () const
    {
      int i = syscolor_index ();
      if (i >= 0)
        return GetSysColor (i);
      return rgb;
    }
};

#endif /* _xcolor_h_ */
