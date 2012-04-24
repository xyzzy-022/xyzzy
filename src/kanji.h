#ifndef _kanji_h_
# define _kanji_h_

# define DCE_SJIS      0x01
# define DCE_JIS       0x02
# define DCE_EUC       0x04
# define DCE_UTF8N     0x08
# define DCE_UTF8      0x10
# define DCE_UTF16LE   0x20
# define DCE_UTF16BE   0x40

# define DEFAULT_DETECT_BUFFER_SIZE   0x40000

int check_kanji2 (const char *, u_int);
lisp detect_char_encoding (const char *, int);
lisp detect_char_encoding (const char *, int, int);

#endif
