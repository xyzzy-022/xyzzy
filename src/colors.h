#ifndef _colors_h_
#define _colors_h_

enum
{
  MC_FILER_FG,
  MC_FILER_BG,
  MC_FILER_HIGHLIGHT_FG,
  MC_FILER_HIGHLIGHT_BG,
  MC_FILER_CURSOR,
  MC_BUFTAB_SEL_FG,
  MC_BUFTAB_SEL_BG,
  MC_BUFTAB_DISP_FG,
  MC_BUFTAB_DISP_BG,
  MC_BUFTAB_FG,
  MC_BUFTAB_BG,
  MC_TAB_SEL_FG,
  MC_TAB_SEL_BG,
  MC_TAB_FG,
  MC_TAB_BG,

  MC_NCOLORS
};

void load_misc_colors ();
void modify_misc_colors (const XCOLORREF *, int);
const char *misc_color_name (int);
XCOLORREF get_misc_color (int);

#endif /* _colors_h_ */
