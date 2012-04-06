#ifndef _guess_h_
# define _guess_h_

inline int
utf8_signature_p (const char *string, int size)
{
  return size >= 3 && !memcmp (string, "\xef\xbb\xbf", 3);
}

#endif
