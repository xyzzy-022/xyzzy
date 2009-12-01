#ifndef _oleconv_h_
#define _oleconv_h_

static inline wchar_t *
_a2w_helper (wchar_t *w, const char *a, int l)
{
  *w = 0;
  MultiByteToWideChar (CP_ACP, 0, a, -1, w, l);
  return w;
}

static inline char *
_w2a_helper (char *a, const wchar_t *w, int l)
{
  *a = 0;
  WideCharToMultiByte (CP_ACP, 0, w, -1, a, l, 0, 0);
  return a;
}

static inline wchar_t *
_i2w_helper (wchar_t *w, const Char *p, int l)
{
  i2w (p, l, w);
  return w;
}

#define USES_CONVERSION int _convert; _convert

#define A2W(a) \
  (_convert = (strlen (a) + 1),\
   _a2w_helper ((wchar_t *)alloca (_convert * sizeof (wchar_t)), (a), _convert))

#define W2A(w) \
  (_convert = (wcslen (w) + 1) * 2,\
   _w2a_helper ((char *)alloca (_convert), (w), _convert))

#define I2W(x) \
  (_i2w_helper ((wchar_t *)alloca ((xstring_length (x) + 1) * sizeof (wchar_t)), \
                xstring_contents (x), xstring_length (x)))

#endif /* _oleconv_h_ */
