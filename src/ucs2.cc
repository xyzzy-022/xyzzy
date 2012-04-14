#include "stdafx.h"
#include "cdecl.h"
#include "charset.h"
#include "ucs2tab.h"
#include "char-width.h"

wc2int_hash wc2int_iso8859_1_hash;
wc2int_hash wc2int_iso8859_2_hash;
wc2int_hash wc2int_iso8859_3_hash;
wc2int_hash wc2int_iso8859_4_hash;
wc2int_hash wc2int_iso8859_5_hash;
wc2int_hash wc2int_iso8859_7_hash;
wc2int_hash wc2int_iso8859_9_hash;
wc2int_hash wc2int_iso8859_10_hash;
wc2int_hash wc2int_iso8859_13_hash;

static wc2int_hash_rep iso8859_1_rep[ISO8859_1_HASHSIZE];
static wc2int_hash_rep iso8859_2_rep[ISO8859_2_HASHSIZE];
static wc2int_hash_rep iso8859_3_rep[ISO8859_3_HASHSIZE];
static wc2int_hash_rep iso8859_4_rep[ISO8859_4_HASHSIZE];
static wc2int_hash_rep iso8859_5_rep[ISO8859_5_HASHSIZE];
static wc2int_hash_rep iso8859_7_rep[ISO8859_7_HASHSIZE];
static wc2int_hash_rep iso8859_9_rep[ISO8859_9_HASHSIZE];
static wc2int_hash_rep iso8859_10_rep[ISO8859_10_HASHSIZE];
static wc2int_hash_rep iso8859_13_rep[ISO8859_13_HASHSIZE];

wc2int_hash wc2int_windows_latin1_hash;
wc2int_hash wc2int_windows_latin2_hash;
wc2int_hash wc2int_windows_cyrillic_hash;
wc2int_hash wc2int_windows_greek_hash;
wc2int_hash wc2int_windows_turkish_hash;
wc2int_hash wc2int_windows_baltic_hash;
wc2int_hash wc2int_koi8r_hash;
wc2int_hash wc2int_koi8u_hash;

static wc2int_hash_rep windows_latin1_rep[WINDOWS_LATIN1_HASHSIZE];
static wc2int_hash_rep windows_latin2_rep[WINDOWS_LATIN2_HASHSIZE];
static wc2int_hash_rep windows_cyrillic_rep[WINDOWS_CYRILLIC_HASHSIZE];
static wc2int_hash_rep windows_greek_rep[WINDOWS_GREEK_HASHSIZE];
static wc2int_hash_rep windows_turkish_rep[WINDOWS_TURKISH_HASHSIZE];
static wc2int_hash_rep windows_baltic_rep[WINDOWS_BALTIC_HASHSIZE];
static wc2int_hash_rep koi8r_rep[KOI8R_HASHSIZE];
static wc2int_hash_rep koi8u_rep[KOI8U_HASHSIZE];

Char wc2internal_table[0x10000];
Char wc2cp932_table[0x10000];
Char wc2big5_table[0x10000];
Char wc2ksc5601_table[0x10000];
Char wc2gb2312_table[0x10000];
Char cns11643_1_to_internal[94 * 94];
Char cns11643_2_to_internal[94 * 94];
Char big5cns_table[157 * 88];
u_char code_charset_table[512];

static void
init_ucs2tab (int min, int max)
{
  Char o = wc2internal_table[0xffff];
  for (Char cc = max; cc >= min; cc--)
    wc2internal_table[i2w (cc)] = cc;
  wc2internal_table[0xffff] = o;
}

static void
init_iso8859 (wc2int_hash &hash, wc2int_hash_rep *rep, int size, int ccs)
{
  const ucs2_t *wc = &i2w (ccs);

  hash.size = size;
  hash.rep = rep;

  for (int i = 0; i < size; i++)
    rep[i].wc = ucs2_t (-1);

  for (int i = 0; i < 128; i++)
    if (wc[i] != ucs2_t (-1))
      {
        ucs2_t w = wc[i];
        int n = w % size;
        rep[n].wc = w;
        rep[n].cc = ccs | i;
        Char ic = wc2internal_table[w];
        if (ic == Char (-1) || w < 0x2000 || code_charset (ic) != ccs_jisx0208)
          wc2internal_table[w] = ccs | i;
      }
}

static void
init_wincp (wc2int_hash &hash, wc2int_hash_rep *rep, int size, const Char *to_int)
{
  hash.size = size;
  hash.rep = rep;

  for (int i = 0; i < size; i++)
    rep[i].wc = ucs2_t (-1);

  for (int i = 0; i < 128; i++)
    {
      ucs2_t wc = i2w (to_int[i]);
      if (wc != ucs2_t (-1))
        {
          int n = wc % size;
          rep[n].wc = wc;
          rep[n].cc = i | 128;
        }
    }
}

static inline void
init_jisx0212 ()
{
  init_ucs2tab (CCS_JISX0212_MIN, CCS_JISX0212_MAX);
}

static inline void
init_gb2312 ()
{
  init_ucs2tab (CCS_GB2312_MIN, CCS_GB2312_MAX);
}

static void
init_big5 ()
{
  init_ucs2tab (CCS_BIG5_MIN, CCS_BIG5_MAX);
}

static inline void
init_ksc5601 ()
{
  init_ucs2tab (CCS_KSC5601_MIN, CCS_KSC5601_MAX);
}

static void
init_cp932 ()
{
  for (int i = 0; i < 0x10000; i++)
    {
      Char cc = wc2cp932 (i);
      if (cc != Char (-1))
        wc2internal_table[i] = cc;
    }
}

#ifndef RUNTIME_TEST_DIFF_TABLE
#define COPY_DIFF_TABLE(tab, name, diff, ndiff, both, nboth, wsame, nwsame, \
                        iincr, niincr, wincr, nwincr, rest, nrest) \
     copy_diff_table (tab, both, nboth, wsame, nwsame, \
                      iincr, niincr, wincr, nwincr, rest, nrest)
#else
#define COPY_DIFF_TABLE copy_diff_table
#endif

static void
copy_diff_table (Char *const tab,
#ifdef RUNTIME_TEST_DIFF_TABLE
                 const char *name, const Char *diff, int ndiff,
#endif
                 const Char *both, int nboth,
                 const Char *wsame, int nwsame,
                 const Char *iincr, int niincr,
                 const Char *wincr, int nwincr,
                 const Char *rest, int nrest)
{
  const Char *p, *pe;

#ifdef RUNTIME_TEST_DIFF_TABLE
  Char test[65536];
  memcpy (test, tab, sizeof test);
  for (p = diff, pe = p + ndiff; p < pe; p += 2)
    test[p[0]] = p[1];
#endif

  if (nboth != 1)
    {
      for (p = both, pe = p + nboth; p < pe;)
        {
          int n = *p++;
          Char s = *p++;
          Char d = *p++;
          for (int i = 0; i < n; i++, s++, d++)
            tab[s] = d;
        }
#ifdef RUNTIME_TEST_DIFF_TABLE
      if (p != pe)
        printf ("bad %s_both\n", name);
#endif
    }

  if (nwsame != 1)
    {
      for (p = wsame, pe = p + nwsame; p < pe;)
        {
          int n = *p++;
          Char s = *p++;
          Char d = *p++;
          for (int i = 0; i < n; i++, s++)
            tab[s] = d;
        }
#ifdef RUNTIME_TEST_DIFF_TABLE
      if (p != pe)
        printf ("bad %s_wsame\n", name);
#endif
    }

  if (niincr != 1)
    {
      for (p = iincr, pe = p + niincr; p < pe;)
        {
          int n = *p++;
          Char s = *p++;
          for (int i = 0; i < n; i++, s++, p++)
            tab[s] = *p;
        }
#ifdef RUNTIME_TEST_DIFF_TABLE
      if (p != pe)
        printf ("bad %s_iincr\n", name);
#endif
    }

  if (nwincr != 1)
    {
      for (p = wincr, pe = p + nwincr; p < pe;)
        {
          int n = *p++;
          Char d = *p++;
          for (int i = 0; i < n; i++, d++, p++)
            tab[*p] = d;
        }
#ifdef RUNTIME_TEST_DIFF_TABLE
      if (p != pe)
        printf ("bad %s_wincr\n", name);
#endif
    }

  for (p = rest, pe = p + nrest; p < pe; p += 2)
    tab[p[0]] = p[1];

#ifdef RUNTIME_TEST_DIFF_TABLE
  printf ("Create %s table %s\n", name, !memcmp (test, tab, sizeof test) ? "succeeded" : "failed");
  fflush (stdout);
#endif
}

static void
make_wc2cp932_table ()
{
  for (int i = 0; i < 0x10000; i++)
    wc2cp932_table[i] = Char (-1);
  for (int i = 0; i < 0x100; i++)
    if (i2w (i) != Char (-1))
      wc2cp932_table[i2w (i)] = i;
  for (int i = 0x8100; i <= 0x9fff; i++)
    wc2cp932_table[i2w (i)] = i;
  for (int i = 0xe000; i <= 0xfcff; i++)
    wc2cp932_table[i2w (i)] = i;
  wc2cp932_table[0xffff] = Char (-1);
  for (int i = CCS_UTF16_SURROGATE_HIGH_MIN; i <= CCS_UTF16_SURROGATE_LOW_MAX; i++)
    wc2cp932_table[i] = i;

  COPY_DIFF_TABLE (wc2cp932_table,
                   "wc2cp932", wc2cp932_diff, numberof (wc2cp932_diff),
                   wc2cp932_diff_both_incr, numberof (wc2cp932_diff_both_incr),
                   wc2cp932_diff_wsame, numberof (wc2cp932_diff_wsame),
                   wc2cp932_diff_iincr, numberof (wc2cp932_diff_iincr),
                   wc2cp932_diff_wincr, numberof (wc2cp932_diff_wincr),
                   wc2cp932_diff_rest, numberof (wc2cp932_diff_rest));
}

#ifndef RUNTIME_TEST_DIFF_TABLE
#define INIT_WC2PERLANG_TABLE(tab, min, max, name, diff, ndiff, both, nboth, wsame, nwsame, \
                              iincr, niincr, wincr, nwincr, rest, nrest) \
     init_wc2perlang_table (tab, min, max, both, nboth, wsame, nwsame, \
                            iincr, niincr, wincr, nwincr, rest, nrest)
#else
#define INIT_WC2PERLANG_TABLE init_wc2perlang_table
#endif

static void
init_wc2perlang_table (Char *const tab, int min, int max,
#ifdef RUNTIME_TEST_DIFF_TABLE
                       const char *name, const Char *diff, int ndiff,
#endif
                       const Char *both, int nboth,
                       const Char *wsame, int nwsame,
                       const Char *iincr, int niincr,
                       const Char *wincr, int nwincr,
                       const Char *rest, int nrest)
{
  int i;
  for (i = 0; i < 0x80; i++)
    tab[i] = i;
  for (; i < 0x10000; i++)
    tab[i] = Char (-1);
  for (i = min; i <= max; i++)
    tab[i2w (i)] = i;
  tab[0xffff] = Char (-1);

  COPY_DIFF_TABLE (tab, name, diff, ndiff, both, nboth, wsame, nwsame,
                   iincr, niincr, wincr, nwincr, rest, nrest);
}

void
init_wc2big5_table ()
{
  static int initialized;
  if (!initialized)
    {
      INIT_WC2PERLANG_TABLE (wc2big5_table, CCS_BIG5_MIN, CCS_BIG5_MAX,
                             "wc2big5", wc2big5_diff, numberof (wc2big5_diff),
                             wc2big5_diff_both_incr, numberof (wc2big5_diff_both_incr),
                             wc2big5_diff_wsame, numberof (wc2big5_diff_wsame),
                             wc2big5_diff_iincr, numberof (wc2big5_diff_iincr),
                             wc2big5_diff_wincr, numberof (wc2big5_diff_wincr),
                             wc2big5_diff_rest, numberof (wc2big5_diff_rest));
      initialized = 1;
    }
}

void
init_wc2ksc5601_table ()
{
  static int initialized;
  if (!initialized)
    {
      INIT_WC2PERLANG_TABLE (wc2ksc5601_table, CCS_KSC5601_MIN, CCS_KSC5601_MAX,
                             "wc2ksc5601", wc2ksc5601_diff, numberof (wc2ksc5601_diff),
                             wc2ksc5601_diff_both_incr, numberof (wc2ksc5601_diff_both_incr),
                             wc2ksc5601_diff_wsame, numberof (wc2ksc5601_diff_wsame),
                             wc2ksc5601_diff_iincr, numberof (wc2ksc5601_diff_iincr),
                             wc2ksc5601_diff_wincr, numberof (wc2ksc5601_diff_wincr),
                             wc2ksc5601_diff_rest, numberof (wc2ksc5601_diff_rest));
      initialized = 1;
    }
}

void
init_wc2gb2312_table ()
{
  static int initialized;
  if (!initialized)
    {
      INIT_WC2PERLANG_TABLE (wc2gb2312_table, CCS_GB2312_MIN, CCS_GB2312_MAX,
                             "wc2gb2312", wc2gb2312_diff, numberof (wc2gb2312_diff),
                             wc2gb2312_diff_both_incr, numberof (wc2gb2312_diff_both_incr),
                             wc2gb2312_diff_wsame, numberof (wc2gb2312_diff_wsame),
                             wc2gb2312_diff_iincr, numberof (wc2gb2312_diff_iincr),
                             wc2gb2312_diff_wincr, numberof (wc2gb2312_diff_wincr),
                             wc2gb2312_diff_rest, numberof (wc2gb2312_diff_rest));
      initialized = 1;
    }
}

static int
make_cns_table (Char *buf, Char cc, const cns_table *cnstab, int csize)
{
  int i = 0;
  for (const cns_table *cp = cnstab, *const ce = cnstab + csize; cp < ce; cp++)
    {
      int e = cp->i;
      for (; i < e; i++, cc++)
        buf[i] = cc;
      if (cp->c >= CNS_NIL_THRESHOLD)
        {
          buf[i++] = cp->c;
          cc = cp->c + 1;
        }
      else
        {
          for (e = i + cp->c; i < e; i++)
            buf[i] = Char (-1);
        }
    }
  return i;
}

void
init_cns11643_table ()
{
  static int initialized;
  if (!initialized)
    {
      make_cns_table (cns11643_1_to_internal, CNS11643_1_START_CHAR,
                      cns11643_1_tab, numberof (cns11643_1_tab));
      make_cns_table (cns11643_2_to_internal, CNS11643_2_START_CHAR,
                      cns11643_2_tab, numberof (cns11643_2_tab));
      initialized = 1;
    }
}

void
init_big5cns_table ()
{
  static int initialized;
  if (!initialized)
    {
      make_cns_table (big5cns_table, BIG5CNS_START_CHAR,
                      big5cns_tab, numberof (big5cns_tab));
      initialized = 1;
    }
}

#ifdef RUNTIME_TEST_CNS_TABLE
static void
test_cns_table ()
{
  int x;
  x = make_cns_table (cns11643_1_to_internal, CNS11643_1_START_CHAR,
                      cns11643_1_tab, numberof (cns11643_1_tab));
  if (x != numberof (cns11643_1_to_internal)
      || sizeof cns11643_1_to_internal != sizeof test_cns11643_1_to_internal
      || memcmp (cns11643_1_to_internal, test_cns11643_1_to_internal,
                 sizeof cns11643_1_to_internal))
    printf ("Create cns11643_1_to_internal failed\n");

  x = make_cns_table (cns11643_2_to_internal, CNS11643_2_START_CHAR,
                      cns11643_2_tab, numberof (cns11643_2_tab));
  if (x != numberof (cns11643_2_to_internal)
      || sizeof cns11643_2_to_internal != sizeof test_cns11643_2_to_internal
      || memcmp (cns11643_2_to_internal, test_cns11643_2_to_internal,
                 sizeof cns11643_2_to_internal))
    printf ("Create cns11643_2_to_internal failed\n");

  x = make_cns_table (big5cns_table, BIG5CNS_START_CHAR,
                      big5cns_tab, numberof (big5cns_tab));
  if (x != numberof (big5cns_table)
      || sizeof big5cns_table != sizeof test_big5cns_table
      || memcmp (big5cns_table, test_big5cns_table, sizeof big5cns_table))
    printf ("Create big5cns_table failed\n");
  printf ("test_cns_table done\n");
  fflush (stdout);
}
#endif

static void
init_unicode (int min, int max, int off)
{
  for (int i = min; i <= max; i++)
    {
      Char c = wc2internal_table[i];
      if (c == Char (-1) || char_width (c) == 2)
        wc2internal_table[i] = i + off;
    }
}

static void
init_unicode ()
{
  init_unicode (UNICODE_IPA_MIN, UNICODE_IPA_MAX, CCS_IPA_MIN - UNICODE_IPA_MIN);
  init_unicode (UNICODE_SMLCDM_MIN, UNICODE_SMLCDM_MAX, CCS_SMLCDM_MIN - UNICODE_SMLCDM_MIN);
  init_unicode (UNICODE_GEORGIAN_MIN, UNICODE_GEORGIAN_MAX,
                CCS_GEORGIAN_MIN - UNICODE_GEORGIAN_MIN);
}

#ifdef CCS_ULATIN_MIN
static void
init_ulatin ()
{
  for (Char cc = CCS_ULATIN_MIN; cc <= CCS_ULATIN_MAX; cc++)
    {
      ucs2_t wc = i2w (cc);
      if (wc != ucs2_t (-1))
        wc2internal_table[wc] = cc;
    }
}
#endif
#ifdef CCS_UJP_MIN
static void
init_ujp ()
{
  for (Char cc = CCS_UJP_MIN; cc <= CCS_UJP_MAX; cc++)
    {
      ucs2_t wc = i2w (cc);
      if (wc != ucs2_t (-1))
        wc2internal_table[wc] = cc;
    }
}
#endif

static void
init_charset_category ()
{
  memset (code_charset_table, ccs_invalid, sizeof code_charset_table);

  for (int ccs = ccs_usascii; ccs_1byte_charset_p (ccs); ccs++)
    code_charset_table[ccs] = ccs;

#define SET_CCS_RANGE(MIN, MAX, CCS) \
  {for (int i = (MIN) >> 7; i <= (MAX) >> 7; i++) code_charset_table[i] = (CCS);}

  SET_CCS_RANGE (0x8100, 0x9fff, ccs_cp932);
  SET_CCS_RANGE (0xe000, 0xfcff, ccs_cp932);
  SET_CCS_RANGE (CCS_JISX0212_MIN, CCS_JISX0212_MAX, ccs_jisx0212);
  SET_CCS_RANGE (CCS_KSC5601_MIN, CCS_KSC5601_MAX, ccs_ksc5601);
  SET_CCS_RANGE (CCS_GB2312_MIN, CCS_GB2312_MAX, ccs_gb2312);
  SET_CCS_RANGE (CCS_BIG5_MIN, CCS_BIG5_MAX, ccs_big5);

  code_charset_table[CCS_UTF16_UNDEF_CHAR_HIGH >> 7] = ccs_utf16_undef_char_high;
  code_charset_table[(CCS_UTF16_UNDEF_CHAR_HIGH >> 7) + 1] = ccs_utf16_undef_char_high;
  code_charset_table[CCS_UTF16_UNDEF_CHAR_LOW >> 7] = ccs_utf16_undef_char_low;
  code_charset_table[(CCS_UTF16_UNDEF_CHAR_LOW >> 7) + 1] = ccs_utf16_undef_char_low;

  SET_CCS_RANGE (CCS_UTF16_SURROGATE_HIGH_MIN, CCS_UTF16_SURROGATE_HIGH_MAX,
                 ccs_utf16_surrogate_high);
  SET_CCS_RANGE (CCS_UTF16_SURROGATE_LOW_MIN, CCS_UTF16_SURROGATE_LOW_MAX,
                 ccs_utf16_surrogate_low);

  code_charset_table[CCS_GEORGIAN_MIN >> 7] = ccs_georgian;

  code_charset_table[CCS_IPA_MIN >> 7] = ccs_ipa;
  SET_CCS_RANGE (CCS_SMLCDM_MIN, CCS_SMLCDM_MAX, ccs_smlcdm);

#ifdef CCS_UJP_MIN
  SET_CCS_RANGE (CCS_UJP_MIN, CCS_UJP_MAX, ccs_ujp);
#endif
#ifdef CCS_ULATIN_MIN
  SET_CCS_RANGE (CCS_ULATIN_MIN, CCS_ULATIN_MAX, ccs_ulatin);
#endif
}

void
init_ucs2_table ()
{
  make_wc2cp932_table ();

  for (int i = 0; i < numberof (wc2internal_table); i++)
    wc2internal_table[i] = Char (-1);

  init_charset_category ();

  init_big5 ();
  init_gb2312 ();
  init_ksc5601 ();
  init_jisx0212 ();
#ifdef CCS_ULATIN_MIN
  init_ulatin ();
#endif
#ifdef CCS_UJP_MIN
  init_ujp ();
#endif
  init_cp932 ();
  init_iso8859 (wc2int_iso8859_13_hash, iso8859_13_rep, ISO8859_13_HASHSIZE, ccs_iso8859_13 << 7);
  init_iso8859 (wc2int_iso8859_10_hash, iso8859_10_rep, ISO8859_10_HASHSIZE, ccs_iso8859_10 << 7);
  init_iso8859 (wc2int_iso8859_9_hash, iso8859_9_rep, ISO8859_9_HASHSIZE, ccs_iso8859_9 << 7);
  init_iso8859 (wc2int_iso8859_7_hash, iso8859_7_rep, ISO8859_7_HASHSIZE, ccs_iso8859_7 << 7);
  init_iso8859 (wc2int_iso8859_5_hash, iso8859_5_rep, ISO8859_5_HASHSIZE, ccs_iso8859_5 << 7);
  init_iso8859 (wc2int_iso8859_4_hash, iso8859_4_rep, ISO8859_4_HASHSIZE, ccs_iso8859_4 << 7);
  init_iso8859 (wc2int_iso8859_3_hash, iso8859_3_rep, ISO8859_3_HASHSIZE, ccs_iso8859_3 << 7);
  init_iso8859 (wc2int_iso8859_2_hash, iso8859_2_rep, ISO8859_2_HASHSIZE, ccs_iso8859_2 << 7);
  init_iso8859 (wc2int_iso8859_1_hash, iso8859_1_rep, ISO8859_1_HASHSIZE, ccs_iso8859_1 << 7);

  init_unicode ();

  init_wincp (wc2int_windows_latin1_hash, windows_latin1_rep,
              WINDOWS_LATIN1_HASHSIZE, windows_latin1_to_internal);
  init_wincp (wc2int_windows_latin2_hash, windows_latin2_rep,
              WINDOWS_LATIN2_HASHSIZE, windows_latin2_to_internal);
  init_wincp (wc2int_windows_cyrillic_hash, windows_cyrillic_rep,
              WINDOWS_CYRILLIC_HASHSIZE, windows_cyrillic_to_internal);
  init_wincp (wc2int_windows_greek_hash, windows_greek_rep,
              WINDOWS_GREEK_HASHSIZE, windows_greek_to_internal);
  init_wincp (wc2int_windows_turkish_hash, windows_turkish_rep,
              WINDOWS_TURKISH_HASHSIZE, windows_turkish_to_internal);
  init_wincp (wc2int_windows_baltic_hash, windows_baltic_rep,
              WINDOWS_BALTIC_HASHSIZE, windows_baltic_to_internal);
  init_wincp (wc2int_koi8r_hash, koi8r_rep, KOI8R_HASHSIZE, koi8r_to_internal);
  init_wincp (wc2int_koi8u_hash, koi8u_rep, KOI8U_HASHSIZE, koi8u_to_internal);

#ifdef RUNTIME_TEST_CNS_TABLE
  test_cns_table ();
#endif
#ifdef RUNTIME_TEST_DIFF_TABLE
  init_wc2big5_table ();
  init_wc2ksc5601_table ();
  init_wc2gb2312_table ();
#endif
}

static wc2int_hash *to_half_width_hashtabs[] =
{
  &wc2int_iso8859_1_hash,
  &wc2int_iso8859_2_hash,
  &wc2int_iso8859_3_hash,
  &wc2int_iso8859_4_hash,
  &wc2int_iso8859_5_hash,
  &wc2int_iso8859_7_hash,
  &wc2int_iso8859_9_hash,
  &wc2int_iso8859_10_hash,
  &wc2int_iso8859_13_hash,
  &wc2int_windows_latin1_hash,
  &wc2int_windows_latin2_hash,
  &wc2int_windows_cyrillic_hash,
  &wc2int_windows_greek_hash,
  &wc2int_windows_turkish_hash,
  &wc2int_windows_baltic_hash,
  &wc2int_koi8r_hash,
  &wc2int_koi8u_hash,
};

Char
w2i_half_width (ucs2_t wc)
{
  Char cc = w2i (wc);
  if (cc != Char (-1) && char_width (cc) != 1)
    for (int i = 0; i < numberof (to_half_width_hashtabs); i++)
      {
        Char t = lookup_wc2int_hash (*to_half_width_hashtabs[i], wc);
        if (t != Char (-1))
          return t;
      }
  return cc;
}
