#ifndef _conf_h_
#define _conf_h_

#ifndef DECLARE_CONF
#define DECLARE_CONF(NAME, VALUE) extern char NAME[];
#endif

DECLARE_CONF (cfgAscii, "ascii");
DECLARE_CONF (cfgBackColor, "backColor");
DECLARE_CONF (cfgBackslash, "backslash");
DECLARE_CONF (cfgBg, "bg");
DECLARE_CONF (cfgBig5, "big5");
DECLARE_CONF (cfgCaretColor, "caretColor");
DECLARE_CONF (cfgColumnLeft, "columnLeft");
DECLARE_CONF (cfgColumnRight, "columnRight");
DECLARE_CONF (cfgColumnSep, "columnSep");
DECLARE_CONF (cfgColumns, "columns");
DECLARE_CONF (cfgColumn, "column");
DECLARE_CONF (cfgCommentColor, "commentColor");
DECLARE_CONF (cfgCtlColor, "ctlColor");
DECLARE_CONF (cfgCursorColor, "cursorColor");
DECLARE_CONF (cfgCustColor, "custColor");
DECLARE_CONF (cfgCyrillic, "cyrillic");
DECLARE_CONF (cfgFg, "fg");
DECLARE_CONF (cfgFnkeyLabels, "fnkeyLabels");
DECLARE_CONF (cfgFoldColumns, "foldColumns");
DECLARE_CONF (cfgFoldLineNumMode, "foldLinenumMode");
DECLARE_CONF (cfgFoldMode, "foldMode");
DECLARE_CONF (cfgFooterOffset, "footerOffset");
DECLARE_CONF (cfgFooterMargin, "footerMargin");
DECLARE_CONF (cfgFooterOn, "footerOn");
DECLARE_CONF (cfgFooter, "footer");
DECLARE_CONF (cfgGb2312, "gb2312");
DECLARE_CONF (cfgGreek, "greek");
DECLARE_CONF (cfgHeaderOffset, "headerOffset");
DECLARE_CONF (cfgHeaderMargin, "headerMargin");
DECLARE_CONF (cfgHeaderOn, "headerOn");
DECLARE_CONF (cfgHeader, "header");
DECLARE_CONF (cfgImeCaretColor, "imeCaretColor");
DECLARE_CONF (cfgJapanese, "japanese");
DECLARE_CONF (cfgKsc5601, "ksc5601");
DECLARE_CONF (cfgKwdColor1, "kwdColor1");
DECLARE_CONF (cfgKwdColor2, "kwdColor2");
DECLARE_CONF (cfgKwdColor3, "kwdColor3");
DECLARE_CONF (cfgLatin, "latin");
DECLARE_CONF (cfgLineFeed, "lineFeed");
DECLARE_CONF (cfgLineNumber, "lineNumber");
DECLARE_CONF (cfgLineSpacing, "lineSpacing");
DECLARE_CONF (cfgMargin, "margin");
DECLARE_CONF (cfgTextMargin, "textMargin");
DECLARE_CONF (cfgModeLineBg, "modeLineBg");
DECLARE_CONF (cfgModeLineFg, "modeLineFg");
DECLARE_CONF (cfgRecommendSize, "recommendSize");
DECLARE_CONF (cfgSizePixel, "sizePixel");
DECLARE_CONF (cfgShowProportional, "showProportional");
DECLARE_CONF (cfgUseBitmap, "useBitmap");
DECLARE_CONF (cfgRestoreWindowPosition, "restoreWindowPosition");
DECLARE_CONF (cfgRestoreWindowSize, "restoreWindowSize");
DECLARE_CONF (cfgSaveWindowPosition, "saveWindowPosition");
DECLARE_CONF (cfgSaveWindowSize, "saveWindowSize");
DECLARE_CONF (cfgSaveWindowSnapSize, "saveWindowSnapSize");
DECLARE_CONF (cfgScale, "scale");
DECLARE_CONF (cfgSortLeft, "sortLeft");
DECLARE_CONF (cfgSortRight, "sortRight");
DECLARE_CONF (cfgSort, "sort");
DECLARE_CONF (cfgStringColor, "stringColor");
DECLARE_CONF (cfgTagColor, "tagColor");
DECLARE_CONF (cfgTextColor, "textColor");
DECLARE_CONF (cfgWindowFlags, "windowFlags");
DECLARE_CONF (cfgColors, "Colors");
DECLARE_CONF (cfgGeometry, "geometry");
DECLARE_CONF (cfgShowCmd, "showCmd");
DECLARE_CONF (cfgLeft, "left");
DECLARE_CONF (cfgTop, "top");
DECLARE_CONF (cfgRight, "right");
DECLARE_CONF (cfgBottom, "bottom");
DECLARE_CONF (cfgFiler, "Filer");
DECLARE_CONF (cfgPrintPreview, "PrintPreview");
DECLARE_CONF (cfgMisc, "Misc");
DECLARE_CONF (cfgBufferSelector, "BufferSelector");
DECLARE_CONF (cfgFont, "Font");
DECLARE_CONF (cfgPrint, "Print");
DECLARE_CONF (cfgSystemRoot, "systemRoot");
DECLARE_CONF (cfgLinenum, "linenum");
DECLARE_CONF (cfgReverse, "reverse");
DECLARE_CONF (cfgSelectionBackColor, "selectionBackColor");
DECLARE_CONF (cfgSelectionTextColor, "selectionTextColor");
DECLARE_CONF (cfgUnselectedModeLineBg, "unselectedModeLineBg");
DECLARE_CONF (cfgUnselectedModeLineFg, "unselectedModeLineFg");

struct PRLOGFONT;

void write_conf (const char *, const char *, const char *);
void write_conf (const char *, const char *, long, int = 0);
void write_conf (const char *, const char *, const int *, int, int = 0);
void write_conf (const char *, const char *, const RECT &);
void write_conf (const char *, const char *, const LOGFONT &);
void write_conf (const char *, const char *, const PRLOGFONT &);
void write_conf (const char *, const char *, const WINDOWPLACEMENT &);
int read_conf (const char *, const char *, char *, int);
int read_conf (const char *, const char *, int &);
#if INT_MAX != LONG_MAX
int read_conf (const char *, const char *, u_long &);
#else
static inline int
read_conf (const char *section, const char *name, u_long &value)
{
  return read_conf (section, name, *(int *)&value);
}
#endif
int read_conf (const char *, const char *, int *, int);
int read_conf (const char *, const char *, RECT &);
int read_conf (const char *, const char *, LOGFONT &);
int read_conf (const char *, const char *, PRLOGFONT &);
int read_conf (const char *, const char *, WINDOWPLACEMENT &);
void flush_conf ();
int conf_load_geometry (HWND, const char *, const char * = 0, int = 1, int = 1);
void conf_save_geometry (HWND, const char *, const char * = 0, int = 1, int = 1);
void adjust_snap_window_size (HWND, WINDOWPLACEMENT &);
void make_geometry_key (char* buf, size_t bufsize, const char *prefix);

void conf_write_string (const char *, const char *, const char *);
void delete_conf (const char *);

int reg2ini ();
void reg_delete_tree ();

#endif /* _conf_h_ */
