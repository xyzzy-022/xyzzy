#ifndef _charset_h_
#define _charset_h_

/*
  INTERNAL CODE

  0000-007F  US-ASCII
  0080-00FF  JIS X 0201-KANA
  0100-017F  ISO 8859-1
  0180-01FF  ISO 8859-2
  0200-027F  ISO 8859-3
  0280-02FF  ISO 8859-4
  0300-037F  ISO 8859-5
  0380-03FF  ISO 8859-7
  0400-047F  ISO 8859-9
  0480-04FF  ISO 8859-10
  0500-057F  ISO 8859-13
  ...
  0D00-0FFF  misc chars (half width)
  1000-12FF  misc chars (full width)
  1300-14FF  misc latin chars
  1500-157F  Basic Georgian & Georgian Extended
  1580-15FF  IPA Extensions
  1600-16FF  Spacing Modifier Letters & Combining Diacritical Marks
  1700-39FF  KS C 5601
  3A00-5CFF  GB 2312
  5D00-7FFF  JIS X 0212
  8000-80FF  Meta Characters
  8100-9FFF  CP932
  A000-D5FF  BIG5
  D600-D6FF  UTF16 UNDEFINED HIGH
  D700-D7FF  UTF16 UNDEFINED LOW
  D800-DBFF  UTF16 SURROGATE HIGH
  DC00-DFFF  UTF16 SURROGATE LOW
  E000-FCFF  CP932
  ...
  FE00-FEFF  Meta Function Character
  FF00-FFFF  Function Character
 */

#define DEFCHAR '?'

#define ccs_usascii                 0
#define ccs_jisx0201_kana           1
#define ccs_iso8859_1               2
#define ccs_iso8859_2               3
#define ccs_iso8859_3               4
#define ccs_iso8859_4               5
#define ccs_iso8859_5               6
#define ccs_iso8859_7               7
#define ccs_iso8859_9               8
#define ccs_iso8859_10              9
#define ccs_iso8859_13             10
#define ccs_jisx0208               11
#define  ccs_cp932                 ccs_jisx0208
#define ccs_jisx0212               12
#define ccs_gb2312                 13
#define ccs_ksc5601                14
#define ccs_big5_1                 15
#define ccs_big5_2                 16
#define  ccs_big5                  ccs_big5_1
#define ccs_utf16_undef_char_high  17
#define ccs_utf16_undef_char_low   18
#define ccs_utf16_surrogate_high   19
#define ccs_utf16_surrogate_low    20
#define ccs_cns11643_1             21
#define ccs_cns11643_2             22
#define ccs_ipa                    23
#define ccs_smlcdm                 24
#define ccs_georgian               25
#define ccs_ujp                    26
#define ccs_ulatin                 27
#define ccs_max                    28
#define ccs_pseudo_cp932           30
#define ccs_invalid                31

#define ccs_1byte_charset_p(ccs)     ((ccs) < ccs_jisx0208)
#define ccs_2byte_charset_p(ccs)     (!ccs_1byte_charset_p (ccs))
#define ccs_1byte_94_charset_p(ccs)  ((ccs) <= ccs_jisx0201_kana)
#define ccs_1byte_96_charset_p(ccs)  (!ccs_1byte_94_charset_p (ccs))

#define CCS_UJP_MIN                  0x0d00
# define CCS_UJP_HALF_MIN            CCS_UJP_MIN
# define CCS_UJP_HALF_MAX            (CCS_UJP_HALF_MIN + 0x2ff)
# define CCS_UJP_FULL_MIN            (CCS_UJP_HALF_MAX + 1)
# define CCS_UJP_FULL_MAX            CCS_UJP_MAX
#define CCS_UJP_MAX                  0x12ff
#define CCS_ULATIN_MIN               0x1300
#define CCS_ULATIN_MAX               0x14ff
#define CCS_GEORGIAN_MIN             0x1500
#define CCS_GEORGIAN_MAX             0x157F
#define CCS_IPA_MIN                  0x1580
#define CCS_IPA_MAX                  0x15df
#define CCS_SMLCDM_MIN               0x1600
#define CCS_SMLCDM_MAX               0x16bf
#define CCS_KSC5601_MIN              0x1700
#define CCS_KSC5601_MAX              0x3983
#define CCS_GB2312_MIN               0x3a00
#define CCS_GB2312_MAX               0x5c83
#define CCS_JISX0212_MIN             0x5d00
#define CCS_JISX0212_MAX             0x7f83
#define CCS_BIG5_MIN                 0xa000
#define CCS_BIG5_MAX                 0xd5f7

#define CCS_UTF16_UNDEF_CHAR_HIGH    0xd600
#define CCS_UTF16_UNDEF_CHAR_LOW     0xd700

#define CCS_UTF16_SURROGATE_HIGH_MIN 0xd800
#define CCS_UTF16_SURROGATE_HIGH_MAX 0xdbff
#define CCS_UTF16_SURROGATE_LOW_MIN  0xdc00
#define CCS_UTF16_SURROGATE_LOW_MAX  0xdfff

#define UNICODE_BOM 0xfeff
#define UNICODE_REVBOM 0xfffe
#define UNICODE_CHAR_LIMIT 0x110000

#define UNICODE_IPA_MIN      0x0250
#define UNICODE_IPA_MAX      0x02af
#define UNICODE_SMLCDM_MIN   0x02b0
#define UNICODE_SMLCDM_MAX   0x036f
#define UNICODE_GEORGIAN_MIN 0x10a0
#define UNICODE_GEORGIAN_MAX 0x10f0

#define ccsf_iso8859_1              (1 << ccs_iso8859_1)
#define ccsf_iso8859_2              (1 << ccs_iso8859_2)
#define ccsf_iso8859_3              (1 << ccs_iso8859_3)
#define ccsf_iso8859_4              (1 << ccs_iso8859_4)
#define ccsf_iso8859_5              (1 << ccs_iso8859_5)
#define ccsf_iso8859_7              (1 << ccs_iso8859_7)
#define ccsf_iso8859_9              (1 << ccs_iso8859_9)
#define ccsf_iso8859_10             (1 << ccs_iso8859_10)
#define ccsf_iso8859_13             (1 << ccs_iso8859_13)
#define ccsf_cp932                  (1 << ccs_cp932)
#define ccsf_jisx0212               (1 << ccs_jisx0212)
#define ccsf_ksc5601                (1 << ccs_ksc5601)
#define ccsf_gb2312                 (1 << ccs_gb2312)
#define ccsf_big5                   (1 << ccs_big5)
#define ccsf_utf16_undef_char_high  (1 << ccs_utf16_undef_char_high)
#define ccsf_utf16_undef_char_low   (1 << ccs_utf16_undef_char_low)
#define ccsf_utf16_surrogate_high   (1 << ccs_utf16_surrogate_high)
#define ccsf_utf16_surrogate_low    (1 << ccs_utf16_surrogate_low)
#define ccsf_georgian               (1 << ccs_georgian)
#define ccsf_ipa                    (1 << ccs_ipa)
#define ccsf_smlcdm                 (1 << ccs_smlcdm)
#define ccsf_ujp                    (1 << ccs_ujp)
#define ccsf_ulatin                 (1 << ccs_ulatin)

#define ccsf_iso8859 \
  (ccsf_iso8859_1 | ccsf_iso8859_2 | ccsf_iso8859_3 | ccsf_iso8859_4 \
   | ccsf_iso8859_5 | ccsf_iso8859_7 | ccsf_iso8859_9 | ccsf_iso8859_10 \
   | ccsf_iso8859_13)
#define ccsf_utf16_undef_char \
  (ccsf_utf16_undef_char_high | ccsf_utf16_undef_char_low)
#define ccsf_utf16_surrogate \
  (ccsf_utf16_surrogate_high | ccsf_utf16_surrogate_low)

#define ccsf_possible_cp932 \
  (ccsf_iso8859 | ccsf_jisx0212 | ccsf_gb2312 | ccsf_ksc5601 | ccsf_big5 \
   | ccsf_georgian | ccsf_ipa | ccsf_smlcdm | ccsf_ujp | ccsf_ulatin)
#define ccsf_not_cp932 \
  (ccsf_utf16_surrogate | ccsf_utf16_undef_char)

#define CP_JAPANESE       932
#define CP_KOREAN         949
#define CP_CN_TRADITIONAL 950
#define CP_CN_SIMPLIFIED  936
#define CP_LATIN1         1252
#define CP_LATIN2         1250
#define CP_CYRILLIC       1251
#define CP_GREEK          1253
#define CP_TURKISH        1254
#define CP_BALTIC         1257
//#define CP_ARABIC         1256
//#define CP_HEBREW         1255
//#define CP_VIETNAMESE     1258
#define CP_KOI8R          878
#define CP_PSEUDO_KOI8U   100878

static inline u_char
code_charset (Char cc)
{
  extern u_char code_charset_table[];
  return code_charset_table[cc >> 7];
}

static inline u_int
code_charset_bit (Char cc)
{
  return 1 << u_int (code_charset (cc));
}

static inline int
j2sh (int c1, int /*c2*/)
{
  return ((c1 - 1) >> 1) + (c1 <= 0x5e ? 0x71 : 0xb1);
}

static inline int
j2sl (int c1, int c2)
{
  return c2 + ((c1 & 1) ? (c2 < 0x60 ? 0x1f : 0x20) : 0x7e);
}

static inline void
s2j (int &h, int &l)
{
  h -= h > 0x9f ? 0xb1 : 0x71;
  h += h + 1;
  if (l > 0x9e)
    {
      l -= 0x7e;
      h++;
    }
  else
    l -= l > 0x7f ? 0x20 : 0x1f;
}

static inline int
ccs_check_range (Char c, Char min, Char max)
{
  return c >= min && c <= max;
}

static inline Char
ccs_94x94_to_int (int c1, int c2, int min)
{
  return Char (c1 * 94 + c2 + (min - 0x21 * 94 - 0x21));
}

static inline void
ccs_int_to_94x94 (Char c, int &c1, int &c2, int min)
{
  c -= min;
  c1 = c / 94 + 0x21;
  c2 = c % 94 + 0x21;
}

static inline int
ccs_iso8859_p (Char c)
{
  return ccs_check_range (c, ccs_iso8859_1 << 7, (ccs_iso8859_13 << 7) + 127);
}

static inline Char
iso8859_to_int (int c, int ccs)
{
  return ccs | (c & 127);
}

static inline int
int_to_iso8859 (Char c)
{
  return 128 | (c & 127);
}

static inline int
ccs_jisx0212_p (Char c)
{
  return ccs_check_range (c, CCS_JISX0212_MIN, CCS_JISX0212_MAX);
}

static inline Char
jisx0212_to_int (int c1, int c2)
{
  return ccs_94x94_to_int (c1, c2, CCS_JISX0212_MIN);
}

static inline void
int_to_jisx0212 (Char c, int &c1, int &c2)
{
  ccs_int_to_94x94 (c, c1, c2, CCS_JISX0212_MIN);
}

static inline int
ccs_ksc5601_p (Char c)
{
  return ccs_check_range (c, CCS_KSC5601_MIN, CCS_KSC5601_MAX);
}

static inline Char
ksc5601_to_int (int c1, int c2)
{
  return ccs_94x94_to_int (c1, c2, CCS_KSC5601_MIN);
}

static inline void
int_to_ksc5601 (Char c, int &c1, int &c2)
{
  ccs_int_to_94x94 (c, c1, c2, CCS_KSC5601_MIN);
}

static inline int
ccs_gb2312_p (Char c)
{
  return ccs_check_range (c, CCS_GB2312_MIN, CCS_GB2312_MAX);
}

static inline Char
gb2312_to_int (int c1, int c2)
{
  return ccs_94x94_to_int (c1, c2, CCS_GB2312_MIN);
}

static inline void
int_to_gb2312 (Char c, int &c1, int &c2)
{
  ccs_int_to_94x94 (c, c1, c2, CCS_GB2312_MIN);
}

/* Big5 charset range:
   High: A1-C7 C9-F9
   Low:  40-7E A1-FE */
static inline int
ccs_big5_p (Char c)
{
  return ccs_check_range (c, CCS_BIG5_MIN, CCS_BIG5_MAX);
}

static inline int
big5_lead_p (int c)
{
  return c >= 0xa1 && c <= 0xc7 || c >= 0xc9 && c <= 0xf9;
}

static inline int
big5_trail_p (int c)
{
  return c >= 0x40 && c <= 0x7e || c >= 0xa1 && c <= 0xfe;
}

static inline Char
big5_to_int (int c1, int c2)
{
  c1 -= c1 >= 0xc9 ? 0xa2 : 0xa1;
  c2 -= c2 >= 0xa1 ? 0x62 : 0x40;
  return CCS_BIG5_MIN + c1 * 157 + c2;
}

static inline void
int_to_big5 (Char c, int &c1, int &c2)
{
  c -= CCS_BIG5_MIN;
  c1 = c / 157;
  c2 = c % 157;
  c1 += c1 >= 0x27 ? 0xa2 : 0xa1;
  c2 += c2 >= 0x3f ? 0x62 : 0x40;
}

static inline void
mule_g2b (int ccs, int &c1, int &c2)
{
  u_int tem = (c1 - 0x21) * (0xff - 0xa1) + (c2 - 0x21);
  if (ccs == ccs_big5_2)
    tem += (0xff - 0xa1 + 0x7f - 0x40) * (0xc9 - 0xa1);
  c1 = tem / (0xff - 0xa1 + 0x7f - 0x40) + 0xa1;
  c2 = tem % (0xff - 0xa1 + 0x7f - 0x40);
  c2 += c2 < 0x3f ? 0x40 : 0x62;
}

static inline void
mule_b2g (int &ccs, int &c1, int &c2)
{
  u_int tem = (c1 - 0xa1) * (0xff - 0xa1 + 0x7f - 0x40) + c2 - (c2 < 0x7f ? 0x40 : 0x62);
  if (c1 < 0xc9)
    ccs = ccs_big5_1;
  else
    {
      ccs = ccs_big5_2;
      tem -= (0xff - 0xa1 + 0x7f - 0x40) * (0xc9 - 0xa1);
    }
  c1 = tem / (0xff - 0xa1) + 0x21;
  c2 = tem % (0xff - 0xa1) + 0x21;
}

static inline int
char_width (Char cc)
{
  extern u_char char_width_table[];
  return char_width_table[cc >> 3] & (1 << (cc & 7)) ? 2 : 1;
}

static inline const ucs2_t &
i2w (Char cc)
{
  extern ucs2_t internal2wc_table[];
  return internal2wc_table[cc];
}

static inline const Char &
w2i (ucs2_t wc)
{
  extern Char wc2internal_table[];
  return wc2internal_table[wc];
}

static inline const Char &
wc2cp932 (ucs2_t wc)
{
  extern Char wc2cp932_table[];
  return wc2cp932_table[wc];
}

static inline int
utf16_surrogate_high_p (ucs2_t c)
{
  return c >= CCS_UTF16_SURROGATE_HIGH_MIN && c <= CCS_UTF16_SURROGATE_HIGH_MAX;
}

static inline int
utf16_surrogate_low_p (ucs2_t c)
{
  return c >= CCS_UTF16_SURROGATE_LOW_MIN && c <= CCS_UTF16_SURROGATE_LOW_MAX;
}

static inline ucs4_t
utf16_pair_to_ucs4 (ucs2_t hi, ucs2_t lo)
{
  return (hi * 1024 + lo
          - (CCS_UTF16_SURROGATE_HIGH_MIN * 1024 + CCS_UTF16_SURROGATE_LOW_MIN - 0x10000));
}

static inline ucs2_t
utf16_ucs4_to_pair_high (ucs4_t c)
{
  return ucs2_t ((((c - 0x10000) >> 10) & 1023) + CCS_UTF16_SURROGATE_HIGH_MIN);
}

static inline ucs2_t
utf16_ucs4_to_pair_low (ucs4_t c)
{
  return ucs2_t (((c - 0x10000) & 1023) + CCS_UTF16_SURROGATE_LOW_MIN);
}

static inline int
utf16_undef_char_high_p (ucs2_t c)
{
  return (c & 0xff00) == CCS_UTF16_UNDEF_CHAR_HIGH;
}

static inline int
utf16_undef_char_low_p (ucs2_t c)
{
  return (c & 0xff00) == CCS_UTF16_UNDEF_CHAR_LOW;
}

static inline ucs2_t
utf16_undef_pair_to_ucs2 (ucs2_t hi, ucs2_t lo)
{
  return hi * 256 + lo - (CCS_UTF16_UNDEF_CHAR_HIGH * 256 + CCS_UTF16_UNDEF_CHAR_LOW);
}

static inline ucs2_t
utf16_ucs2_to_undef_pair_high (ucs2_t c)
{
  return ucs2_t (((c >> 8) & 255) + CCS_UTF16_UNDEF_CHAR_HIGH);
}

static inline ucs2_t
utf16_ucs2_to_undef_pair_low (ucs2_t c)
{
  return ucs2_t ((c & 255) + CCS_UTF16_UNDEF_CHAR_LOW);
}

static inline int
ucs2_PUA_p (ucs2_t c)
{
  return c >= 0xe000 && c <= 0xf8ff;
}

struct wc2int_hash_rep
{
  ucs2_t wc;
  Char cc;
};

struct wc2int_hash
{
  u_int size;
  wc2int_hash_rep *rep;
};

static inline Char
lookup_wc2int_hash (const wc2int_hash &hash, ucs2_t wc)
{
  int n = wc % hash.size;
  if (hash.rep[n].wc == wc)
    return hash.rep[n].cc;
  return Char (-1);
}

extern wc2int_hash wc2int_iso8859_1_hash;
extern wc2int_hash wc2int_iso8859_2_hash;
extern wc2int_hash wc2int_iso8859_3_hash;
extern wc2int_hash wc2int_iso8859_4_hash;
extern wc2int_hash wc2int_iso8859_5_hash;
extern wc2int_hash wc2int_iso8859_7_hash;
extern wc2int_hash wc2int_iso8859_9_hash;
extern wc2int_hash wc2int_iso8859_10_hash;
extern wc2int_hash wc2int_iso8859_13_hash;

extern Char windows_latin1_to_internal[];
extern Char windows_latin2_to_internal[];
extern Char windows_cyrillic_to_internal[];
extern Char windows_greek_to_internal[];
extern Char windows_turkish_to_internal[];
extern Char windows_baltic_to_internal[];
extern Char koi8r_to_internal[];
extern Char koi8u_to_internal[];

extern ucs2_t internal2wc_table[];

extern wc2int_hash wc2int_windows_latin1_hash;
extern wc2int_hash wc2int_windows_latin2_hash;
extern wc2int_hash wc2int_windows_cyrillic_hash;
extern wc2int_hash wc2int_windows_greek_hash;
extern wc2int_hash wc2int_windows_turkish_hash;
extern wc2int_hash wc2int_windows_baltic_hash;
extern wc2int_hash wc2int_koi8r_hash;
extern wc2int_hash wc2int_koi8u_hash;

extern Char wc2cp932_table[];
extern Char wc2big5_table[];
extern Char wc2ksc5601_table[];
extern Char wc2gb2312_table[];

extern Char cns11643_1_to_internal[];
extern Char cns11643_2_to_internal[];
extern Char big5cns_table[];

#define BIG5CNS_GB2312     0
#define BIG5CNS_CNS11643_1 0x8000
#define BIG5CNS_CNS11643_2 0x8080

void init_wc2cp932_table ();
void init_wc2big5_table ();
void init_wc2ksc5601_table ();
void init_wc2gb2312_table ();
void init_big5cns_table ();
void init_cns11643_table ();
void init_ucs2_table ();
Char convert_ibmext (Char);
Char convert_ibmext2necext (Char);
Char convert_osfjvc (Char);
Char w2i_half_width (ucs2_t);
Char jisx0212_to_internal (int, int, int);

typedef Char (*vender_code_mapper_fn)(Char);
vender_code_mapper_fn select_vender_code_mapper (int);
int vender_depend_code (int);

#endif /* _charset_h_ */
