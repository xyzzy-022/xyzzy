#ifndef _encoding_h_
#define _encoding_h_

#define XBUFDEF(X) char CONCAT (X, __) [sizeof (X)]

extern u_char escseq_euckr[];
extern u_char escseq_eucgb[];
extern u_int designatable_any[];
const Char *cjk_translate_table (int);

class xstream
{
public:
  enum {eof = -1};
  void *operator new (size_t, void *p) {return p;}
  void operator delete (void *) {}
#if _MSC_VER >= 1100
  void operator delete (void *, void *) {}
#endif
protected:
  xstream () {}
  virtual ~xstream () {}
};

template <class T>
class xinput_stream: public xstream
{
private:
  const T *s_bb;
  const T *s_be;
  const T *s_bp;

protected:
  xinput_stream (const T *b = 0, const T *e = 0) : s_bb (b), s_be (e), s_bp (b) {}
  virtual int refill () {return eof;}
  int setbuf (const T *b, const T *e)
    {
      s_bb = s_bp = b;
      s_be = e;
      if (b == e)
        return eof;
      return *s_bp++;
    }
public:
  int get () {return s_bp == s_be ? refill () : *s_bp++;}
  int peek () {return s_bp == s_be ? refill () : *s_bp;}
  void putback (int c) {if (c != eof && s_bp != s_bb) s_bp--;}
  int eofp ()
    {
      int c = get ();
      putback (c);
      return c == eof;
    }

  int total_length ()
    {
      int l = s_be - s_bp;
      while (refill () != eof)
        l += s_be - s_bp + 1;
      s_bp = s_be;
      return l;
    }

  void copyto (T *b, int size)
    {
      T *const be = b + size - 1;
      while (b < be)
        {
          int l = min (s_be - s_bp, be - b);
          memcpy (b, s_bp, sizeof *b * l);
          b += l;
          if (b == be)
            break;
          int c = refill ();
          if (c == eof)
            break;
          *b++ = c;
        }
      *b = 0;
    }

  int rest_chars () const {return s_be - s_bp;}
  void begin_direct_input (const T *&b, const T *&e) {b = s_bp, e = s_be;}
  void end_direct_input (const T *p) {s_bp = p;}
};

template <class T>
class xoutput_stream: public xstream
{
  T *const s_bb;
  T *const s_be;
  T *s_bp;
protected:
  xoutput_stream (T *b, T *e) : s_bb (b), s_be (e), s_bp (b) {}
  virtual T *sflush (T *, T *, int) = 0;
public:
  void put (int c)
    {
      *s_bp++ = c;
      if (s_bp == s_be)
        s_bp = sflush (s_bb, s_bp, 0);
    }
  void flush (int eofp)
    {
      if (s_bp != s_bb)
        s_bp = sflush (s_bb, s_bp, eofp);
    }
};

class xinput_strstream: public xinput_stream <u_char>
{
public:
  xinput_strstream (const char *s, int l)
       : xinput_stream <u_char> ((const u_char *)s, (const u_char *)s + l) {}
};

class xinput_wstrstream: public xinput_stream <Char>
{
public:
  xinput_wstrstream (const Char *s, int l) : xinput_stream <Char> (s, s + l) {}
};

template <class Ti, class To>
class xfilter_stream: public xinput_stream <To>
{
protected:
  xinput_stream <Ti> &s_in;
  xfilter_stream (xinput_stream <Ti> &in) : s_in (in) {}
public:
  const xinput_stream <Ti> &input_stream () const {return s_in;}
};

class xread_stream: public xfilter_stream <u_char, Char>
{
protected:
  xread_stream (xinput_stream <u_char> &in) : xfilter_stream <u_char, Char> (in) {}
};

template <class T, int SZ, int PAD>
class xtemp_buffer
{
private:
  T b_buf[SZ];
  T *b_bb;
  T *const b_be;
  T *b_bp;
public:
  xtemp_buffer () : b_be (b_buf + SZ - PAD) {}
  void begin () {b_bb = b_bp = b_buf;}
  void put (T c) {*b_bp++ = c;}
  int room () const {return b_be - b_bp;}
  int length () const {return b_bp - b_bb;}
  const T *base () const {return b_buf;}
  const T *head () const {return b_bb;}
  const T *tail () const {return b_bp;}
  void bflush () {b_bb = b_bp;}
  void begin_direct_output (T *&b, T *&e) {b = b_bp, e = b_be;}
  void end_direct_output (T *p) {b_bp = p;}
};

class xbuffered_read_stream: public xread_stream, protected xtemp_buffer <Char, 4096, 32>
{
protected:
  xbuffered_read_stream (xinput_stream <u_char> &in) : xread_stream (in) {}
  virtual int refill ();
  virtual void refill_internal () = 0;
public:
  void flush (const Char *&b, int &l)
    {
      b = head ();
      l = length ();
      bflush ();
    }
};

class sjis_to_internal_stream: public xbuffered_read_stream
{
protected:
  virtual void refill_internal ();
public:
  sjis_to_internal_stream (xinput_stream <u_char> &in) : xbuffered_read_stream (in) {}
};

class fast_sjis_to_internal_stream: public xbuffered_read_stream
{
protected:
  virtual void refill_internal ();
public:
  fast_sjis_to_internal_stream (xinput_stream <u_char> &in)
       : xbuffered_read_stream (in) {}
};

class iso2022_noesc_to_internal_stream: public xbuffered_read_stream
{
protected:
  u_char s_g[4];
  const int s_vender;
  void to_internal (u_char, int, int);
  virtual void refill_internal ();
public:
  iso2022_noesc_to_internal_stream (xinput_stream <u_char> &, const u_char *, int);
};

class iso2022_to_internal_stream: public iso2022_noesc_to_internal_stream
{
protected:
  u_char *s_gl, *s_gr;
  u_char *s_ss;

  virtual void refill_internal ();
  int designate94 (u_char &, int *);
  int designate96 (u_char &, int *);
  int designate94n (u_char &, int *);

public:
  iso2022_to_internal_stream (xinput_stream <u_char> &in, const u_char *g,
                              int flags)
       : iso2022_noesc_to_internal_stream (in, g, flags),
         s_gl (&s_g[0]), s_gr (&s_g[1]), s_ss (0) {}
};

class euckr_to_internal_stream: public iso2022_noesc_to_internal_stream
{
public:
  euckr_to_internal_stream (xinput_stream <u_char> &in)
       : iso2022_noesc_to_internal_stream (in, escseq_euckr, 0) {}
};

class eucgb_to_internal_stream: public iso2022_to_internal_stream
{
public:
  eucgb_to_internal_stream (xinput_stream <u_char> &in)
       : iso2022_to_internal_stream (in, escseq_eucgb, 0) {}
};

class big5_to_internal_stream: public xbuffered_read_stream
{
protected:
  virtual void refill_internal ();
public:
  big5_to_internal_stream (xinput_stream <u_char> &in) : xbuffered_read_stream (in) {}
};

class binary_to_internal_stream: public xbuffered_read_stream
{
protected:
  virtual void refill_internal ();
public:
  binary_to_internal_stream (xinput_stream <u_char> &in) : xbuffered_read_stream (in) {}
};

class utf_to_internal_stream: public xbuffered_read_stream
{
protected:
  typedef void (utf_to_internal_stream::*putw_t)(ucs2_t);
  const int s_flags;
  int s_has_bom;
  const int s_to_full_width;
  const putw_t s_putw;
  const Char *const s_cjk_translate;
  void putw_jp (ucs2_t);
  void putw_cn (ucs2_t);
  void putw_gen (ucs2_t);
  void putl (ucs4_t);
  void putw (ucs2_t c) {(this->*s_putw)(c);}
  static putw_t per_lang_putw (int);
public:
  utf_to_internal_stream (xinput_stream <u_char> &in, int flags, int lang)
       : xbuffered_read_stream (in), s_flags (flags),
         s_has_bom (flags & ENCODING_UTF_SIGNATURE ? -1 : 0),
         s_to_full_width (xsymbol_value (Vunicode_to_half_width) == Qnil),
         s_putw (per_lang_putw (lang)),
         s_cjk_translate (cjk_translate_table (lang)) {}
  int has_bom_p () const {return s_has_bom > 0;}
};

class utf16_to_internal_stream: public utf_to_internal_stream
{
protected:
  void refill_internal_le ();
  void refill_internal_be ();
  virtual void refill_internal () = 0;
public:
  utf16_to_internal_stream (xinput_stream <u_char> &in, int flags, int lang)
       : utf_to_internal_stream (in, flags, lang) {}
};

class utf16le_to_internal_stream: public utf16_to_internal_stream
{
protected:
  virtual void refill_internal () {refill_internal_le ();}
public:
  utf16le_to_internal_stream (xinput_stream <u_char> &in, int flags, int lang)
       : utf16_to_internal_stream (in, flags, lang) {}
};

class utf16be_to_internal_stream: public utf16_to_internal_stream
{
protected:
  virtual void refill_internal () {refill_internal_be ();}
public:
  utf16be_to_internal_stream (xinput_stream <u_char> &in, int flags, int lang)
       : utf16_to_internal_stream (in, flags, lang) {}
};

class utf16unknown_to_internal_stream: public utf16_to_internal_stream
{
protected:
  int &s_byte_order;
  virtual void refill_internal ();
public:
  utf16unknown_to_internal_stream (xinput_stream <u_char> &in,
                                   int flags, int lang, int &byte_order)
       : utf16_to_internal_stream (in, flags, lang),
         s_byte_order (byte_order)
    {s_byte_order = 0;}
};

class utf8_to_internal_stream: public utf_to_internal_stream
{
protected:
  virtual void refill_internal ();
public:
  utf8_to_internal_stream (xinput_stream <u_char> &in, int flags, int lang)
       : utf_to_internal_stream (in, flags, lang) {}
};

class utf7_to_internal_stream: public utf_to_internal_stream
{
protected:
  int s_direct_encoding;
  int s_cc;
  int s_nbytes;
  const int s_imap4p;
  const int s_shift_char;

  int unicode_shifted_encoding ();
  virtual void refill_internal ();
public:
  utf7_to_internal_stream (xinput_stream <u_char> &in, int flags, int lang);
};

class utf5_to_internal_stream: public utf_to_internal_stream
{
protected:
  virtual void refill_internal ();
public:
  utf5_to_internal_stream (xinput_stream <u_char> &in, int flags, int lang)
       : utf_to_internal_stream (in, flags, lang) {}
};

class iso8859_to_internal_stream: public xbuffered_read_stream
{
protected:
  const int s_charset;
  virtual void refill_internal ();
public:
  iso8859_to_internal_stream (xinput_stream <u_char> &in, int ccs)
       : xbuffered_read_stream (in), s_charset (ccs << 7) {}
};

class windows_codepage_to_internal_stream: public xbuffered_read_stream
{
protected:
  const Char *const s_translate;
  virtual void refill_internal ();
public:
  windows_codepage_to_internal_stream (xinput_stream <u_char> &in, const Char *translate)
       : xbuffered_read_stream (in), s_translate (translate) {}
};

class xwrite_stream: public xfilter_stream <Char, u_char>,
                     protected xtemp_buffer <u_char, 4096, 16>
{
protected:
  eol_code s_eol;
  long s_nlines;

  xwrite_stream (xinput_stream <Char> &in, eol_code eol)
       : xfilter_stream <Char, u_char> (in), s_eol (eol), s_nlines (0) {}
  void puteol ();
  int finish () {return setbuf (head (), tail ());}
public:
  long nlines () const {return s_nlines;}
};

class internal_to_sjis_stream: public xwrite_stream
{
protected:
  virtual int refill ();
public:
  internal_to_sjis_stream (xinput_stream <Char> &in, eol_code eol)
       : xwrite_stream (in, eol) {}
};

class internal_to_big5_stream: public xwrite_stream
{
protected:
  virtual int refill ();
public:
  internal_to_big5_stream (xinput_stream <Char> &in, eol_code eol)
       : xwrite_stream (in, eol) {init_wc2big5_table ();}
};

class internal_to_binary_stream: public xwrite_stream
{
protected:
  virtual int refill ();
public:
  internal_to_binary_stream (xinput_stream <Char> &in, eol_code eol)
       : xwrite_stream (in, eol) {}
};

class internal_to_iso2022_stream: public xwrite_stream
{
protected:
  virtual int refill ();

  enum {ctype94, ctype96, ctype94n, ctype96n};
  struct ccs_data
    {
      u_char final;
      u_char ctype;
      u_char fshort;
    };
  static const ccs_data s_ccs_data[];
  static const char s_inter94[];
  static const char s_inter96[];

  u_char *s_gl, *s_gr;
  u_char s_g[4];
  const int s_flags;
  const u_char *const s_initial;
  const u_int *const s_designatable;
  int s_designation[ccs_max];
  const Char *const s_cjk_translate;
  const int s_lang_cn;
  const vender_code_mapper_fn s_vender_code_mapper;

  void designate (int, u_char);
  int designate (u_char);
  int select_designation (int) const;
public:
  internal_to_iso2022_stream (xinput_stream <Char> &, eol_code, int, const u_char *,
                              const u_int *, int);
};

class internal_to_euckr_stream: public internal_to_iso2022_stream
{
public:
  internal_to_euckr_stream (xinput_stream <Char> &in, eol_code eol)
       : internal_to_iso2022_stream (in, eol, (ENCODING_ISO_ASCII_EOL
                                               | ENCODING_ISO_ASCII_CTRL
                                               | ENCODING_ISO_SHORT_FORM),
                                     escseq_euckr, designatable_any,
                                     ENCODING_LANG_KR) {}
};

class internal_to_eucgb_stream: public internal_to_iso2022_stream
{
public:
  internal_to_eucgb_stream (xinput_stream <Char> &in, eol_code eol)
       : internal_to_iso2022_stream (in, eol, (ENCODING_ISO_ASCII_EOL
                                               | ENCODING_ISO_ASCII_CTRL
                                               | ENCODING_ISO_SHORT_FORM),
                                     escseq_eucgb, designatable_any,
                                     ENCODING_LANG_CN_GB) {}
};

class internal_to_utf_stream: public xwrite_stream
{
protected:
  const int s_flags;
  internal_to_utf_stream (xinput_stream <Char> &in, eol_code eol, int flags)
       : xwrite_stream (in, eol), s_flags (flags) {}
  int getw () const;
};

class internal_to_utf16le_stream: public internal_to_utf_stream
{
protected:
  int s_bom;
  virtual int refill ();
public:
  internal_to_utf16le_stream (xinput_stream <Char> &in, eol_code eol, int flags)
       : internal_to_utf_stream (in, eol, flags),
         s_bom (flags & ENCODING_UTF_SIGNATURE) {}
};

class internal_to_utf16be_stream: public internal_to_utf_stream
{
protected:
  int s_bom;
  virtual int refill ();
public:
  internal_to_utf16be_stream (xinput_stream <Char> &in, eol_code eol, int flags)
       : internal_to_utf_stream (in, eol, flags),
         s_bom (flags & ENCODING_UTF_SIGNATURE) {}
};

class internal_to_utf8_stream: public internal_to_utf_stream
{
protected:
  int s_bom;
  virtual int refill ();
public:
  internal_to_utf8_stream (xinput_stream <Char> &in, eol_code eol, int flags)
       : internal_to_utf_stream (in, eol, flags),
         s_bom (flags & ENCODING_UTF_SIGNATURE) {}
};

class internal_to_utf7_stream: public internal_to_utf_stream
{
protected:
  u_char s_b[6];
  int s_nb;
  int s_nshift;
  int s_accept;
  const int s_imap4p;
  const ucs2_t s_shift_char;
  const char *const s_b64;

  virtual int refill ();
  void encode_b64 ();
public:
  internal_to_utf7_stream (xinput_stream <Char> &in, eol_code eol, int flags);
};

class internal_to_utf5_stream: public internal_to_utf_stream
{
protected:
  virtual int refill ();
public:
  internal_to_utf5_stream (xinput_stream <Char> &in, eol_code eol, int flags)
       : internal_to_utf_stream (in, eol, flags) {}
};

class internal_to_iso8859_stream: public xwrite_stream
{
protected:
  int s_charset;
  const wc2int_hash &s_hash;
  virtual int refill ();
  static const wc2int_hash &charset_hash (int);
public:
  internal_to_iso8859_stream (xinput_stream <Char> &in, eol_code eol, int charset)
       : xwrite_stream (in, eol), s_charset (charset),
         s_hash (charset_hash (charset)) {}
};

class internal_to_windows_codepage_stream: public xwrite_stream
{
protected:
  const wc2int_hash &s_hash;
  virtual int refill ();
public:
  internal_to_windows_codepage_stream (xinput_stream <Char> &in, eol_code eol, const wc2int_hash &hash)
       : xwrite_stream (in, eol), s_hash (hash) {}
};

#define XDECODE_STREAM_BUFSIZE 1024
#define XDECODE_STREAM_PADSIZE 16

class xdecode_stream: public xfilter_stream <u_char, u_char>,
                      protected xtemp_buffer <u_char, XDECODE_STREAM_BUFSIZE + XDECODE_STREAM_PADSIZE,
                                              XDECODE_STREAM_PADSIZE>
{
protected:
  xdecode_stream (xinput_stream <u_char> &in)
       : xfilter_stream <u_char, u_char> (in) {}
  int decode (int, const u_char *);
  int finish () {return setbuf (head (), tail ());}
};

class xdecode_b64_stream: public xdecode_stream
{
protected:
  virtual int refill ();
public:
  xdecode_b64_stream (xinput_stream <u_char> &in) : xdecode_stream (in) {}
};

class xdecode_qp_stream: public xdecode_stream
{
protected:
  const int s_underscore_to_space;
  virtual int refill ();
public:
  xdecode_qp_stream (xinput_stream <u_char> &in, int underscore_to_space = 0)
       : xdecode_stream (in), s_underscore_to_space (underscore_to_space) {}
};

class xdecode_uu_stream: public xdecode_stream
{
protected:
  virtual int refill ();
  static int uudecode (int c) {return (c - ' ') & 63;}
public:
  xdecode_uu_stream (xinput_stream <u_char> &in) : xdecode_stream (in) {}
};

class xencode_b64_stream: public xfilter_stream <u_char, u_char>
{
protected:
  enum {MAX_WIDTH = 1024};
  u_char s_buf[MAX_WIDTH + 2];
  const int s_width;
  const int s_fold_p;
  virtual int refill ();
public:
  xencode_b64_stream (xinput_stream <u_char> &in, int width)
       : xfilter_stream <u_char, u_char> (in),
         s_width (width <= 0 ? MAX_WIDTH : min (width, MAX_WIDTH)),
         s_fold_p (width > 0) {}
};

class xencode_uu_stream: public xfilter_stream <u_char, u_char>
{
protected:
  enum {BUFSIZE = 45};
  u_char s_buf[BUFSIZE / 3 * 4 + 2];
  int s_eofp;
  virtual int refill ();
  static int uuencode (int c) {return c ? (c & 63) + ' ' : '`';}
public:
  xencode_uu_stream (xinput_stream <u_char> &in)
       : xfilter_stream <u_char, u_char> (in), s_eofp (0) {}
};

class xencode_qp_stream: public xfilter_stream <u_char, u_char>
{
protected:
  enum {LINESIZE = 70};
  u_char s_buf[LINESIZE + 10];
  static u_char *encode (u_char *, int);
  const int s_space_to_underscore;
  virtual int refill ();
public:
  xencode_qp_stream (xinput_stream <u_char> &in, int space_to_underscore = 0)
       : xfilter_stream <u_char, u_char> (in),
         s_space_to_underscore (space_to_underscore) {}
};

class xdecode_url_stream: public xdecode_stream
{
protected:
  virtual int refill ();
public:
  xdecode_url_stream (xinput_stream <u_char> &in) : xdecode_stream (in) {}
};

class xencode_url_stream: public xdecode_stream
{
protected:
  const char *const s_literal;
  virtual int refill ();
public:
  xencode_url_stream (xinput_stream <u_char> &in, const char *literal)
       : xdecode_stream (in), s_literal (literal) {}
};

class xdecode_hqx_stream: public xdecode_stream
{
protected:
  class hqx7: public xdecode_stream
    {
    protected:
      virtual int refill ();
    public:
      hqx7 (xinput_stream <u_char> &in) : xdecode_stream (in) {}
    };

private:
  hqx7 s_hqx7;
  u_long s_rest_bytes;
  int s_corrupted;

  char s_name[64];
  u_char s_version;
  u_long s_type;
  u_long s_creator;
  u_short s_flags;
  u_long s_data_len;
  u_long s_res_len;
  u_short s_crc1;
  u_short s_crc2;
  u_short s_crc3;

  enum {s_marker = 0x90};
  int s_rep;
  int s_cc;

  void corrupted () {s_corrupted = 1; s_rest_bytes = 0;}
  int get ();
  int read (u_long &);
  int read (u_short &);
protected:
  virtual int refill ();
public:
  xdecode_hqx_stream (xinput_stream <u_char> &in);
  const char *name () const {return s_name;}
  u_long type () const {return s_type;}
  u_long creator () const {return s_creator;}
  int corrupted_p () const {return s_corrupted;}
};

class encoding_input_stream_helper
{
  union
    {
      XBUFDEF (sjis_to_internal_stream);
      XBUFDEF (fast_sjis_to_internal_stream);
      XBUFDEF (big5_to_internal_stream);
      XBUFDEF (binary_to_internal_stream);
      XBUFDEF (iso2022_to_internal_stream);
      XBUFDEF (iso2022_noesc_to_internal_stream);
      XBUFDEF (iso8859_to_internal_stream);
      XBUFDEF (euckr_to_internal_stream);
      XBUFDEF (eucgb_to_internal_stream);
      XBUFDEF (windows_codepage_to_internal_stream);
      XBUFDEF (utf5_to_internal_stream);
      XBUFDEF (utf7_to_internal_stream);
      XBUFDEF (utf8_to_internal_stream);
      XBUFDEF (utf16unknown_to_internal_stream);
      XBUFDEF (utf16le_to_internal_stream);
      XBUFDEF (utf16be_to_internal_stream);
      XBUFDEF (utf_to_internal_stream);
      XBUFDEF (utf16_to_internal_stream);
    } s_xbuf;
  xread_stream *s_stream;
  int s_byte_order;
public:
  encoding_input_stream_helper (lisp, xinput_stream <u_char> &, int = 0);
  ~encoding_input_stream_helper () {delete s_stream;}
  operator xread_stream & () const {return *s_stream;}
  xread_stream *operator -> () const {return s_stream;}
  int utf16_byte_order () const {return s_byte_order;}
};

class encoding_output_stream_helper
{
  union
    {
      XBUFDEF (internal_to_sjis_stream);
      XBUFDEF (internal_to_big5_stream);
      XBUFDEF (internal_to_binary_stream);
      XBUFDEF (internal_to_iso2022_stream);
      XBUFDEF (internal_to_euckr_stream);
      XBUFDEF (internal_to_eucgb_stream);
      XBUFDEF (internal_to_utf_stream);
      XBUFDEF (internal_to_utf16le_stream);
      XBUFDEF (internal_to_utf16be_stream);
      XBUFDEF (internal_to_utf8_stream);
      XBUFDEF (internal_to_utf7_stream);
      XBUFDEF (internal_to_utf5_stream);
      XBUFDEF (internal_to_iso8859_stream);
      XBUFDEF (internal_to_windows_codepage_stream);
    } s_xbuf;
  xwrite_stream *s_stream;
public:
  encoding_output_stream_helper (lisp, xinput_stream <Char> &, eol_code);
  ~encoding_output_stream_helper () {delete s_stream;}
  operator xwrite_stream & () const {return *s_stream;}
  xwrite_stream *operator -> () const {return s_stream;}
};

#endif
