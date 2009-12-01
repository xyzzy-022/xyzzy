#ifndef _ColorDialog_h_
# define _ColorDialog_h_

# include "colors.h"

class PropSheet;

class ChangeColorsPageP
{
protected:
  enum {NPROPS = GLYPH_TEXTPROP_NCOLORS - 1};
  enum {NMODELINES = 2};
  enum {NMISCS = MC_NCOLORS};
  enum {MAX_CCP_COLORS = USER_DEFINABLE_COLORS + NMODELINES + NMISCS + NPROPS * 2};
  enum
    {
      MODELINE_OFFSET = USER_DEFINABLE_COLORS,
      MODELINE_FG_OFFSET = MODELINE_OFFSET,
      MODELINE_BG_OFFSET = MODELINE_OFFSET + 1,
      MISC_OFFSET = MODELINE_OFFSET + NMODELINES,
      PROP_FG_OFFSET = MISC_OFFSET + NMISCS,
      PROP_BG_OFFSET = PROP_FG_OFFSET + NPROPS
    };

  static int prop_fg_p (int x)
    {return x >= PROP_FG_OFFSET && x < PROP_FG_OFFSET + NPROPS;}
  static int prop_bg_p (int x)
    {return x >= PROP_BG_OFFSET && x < PROP_BG_OFFSET + NPROPS;}
  static int misc_p (int x)
    {return x >= MISC_OFFSET && x < MISC_OFFSET + NMISCS;}

private:
  HGLOBAL ccp_hg;

protected:
  HWND ccp_hwnd;
  PropSheet *ccp_parent;
  int ccp_page_no;
  XCOLORREF ccp_curcc[MAX_CCP_COLORS];
  const int ccp_ncolors;

  ChangeColorsPageP (int);
  ~ChangeColorsPageP ();

  void init_page (UINT, PropSheet *, int, PROPSHEETPAGE *);

  virtual BOOL dialog_proc (UINT, WPARAM, LPARAM);

  virtual void set_active () const;
  virtual void reset () const;
  virtual int get_result ();

  virtual void init_dialog ();
  virtual BOOL do_command (int, int);
  virtual BOOL do_notify (int, NMHDR *);
  virtual BOOL draw_item (int, DRAWITEMSTRUCT *);
  virtual void do_destroy () {}

  virtual void notify_color (int) {}

  static void measure_item (HWND, int, MEASUREITEMSTRUCT *);
  static BOOL CALLBACK ccp_dialog_proc (HWND, UINT, WPARAM, LPARAM);

public:
  int ccp_modified;
  XCOLORREF ccp_cc[MAX_CCP_COLORS];

  static const XCOLORREF *colors (const XCOLORREF *x) {return x;}
  static const XCOLORREF *ml_colors (const XCOLORREF *x) {return x + MODELINE_OFFSET;}
  static const XCOLORREF *fg_colors (const XCOLORREF *x) {return x + PROP_FG_OFFSET - 1;}
  static const XCOLORREF *bg_colors (const XCOLORREF *x) {return x + PROP_BG_OFFSET - 1;}
  static const XCOLORREF *misc_colors (const XCOLORREF *x) {return x + MISC_OFFSET;}

  const XCOLORREF *colors () const {return colors (ccp_cc);}
  const XCOLORREF *ml_colors () const {return ml_colors (ccp_cc);}
  const XCOLORREF *fg_colors () const {return fg_colors (ccp_cc);}
  const XCOLORREF *bg_colors () const {return bg_colors (ccp_cc);}
  const XCOLORREF *misc_colors () const {return misc_colors (ccp_cc);}
};

inline
ChangeColorsPageP::ChangeColorsPageP (int ncolors)
     : ccp_hg (0), ccp_ncolors (ncolors)
{
}

inline
ChangeColorsPageP::~ChangeColorsPageP ()
{
  if (ccp_hg)
    {
      GlobalUnlock (ccp_hg);
      GlobalFree (ccp_hg);
    }
}

#include "ChooseFont.h"

class ChooseFontPage: public ChangeColorsPageP
{
protected:
  virtual void init_dialog ();
  virtual void do_destroy ();
  virtual int get_result ();
  virtual void notify_color (int);
  virtual BOOL do_command (int, int);

  virtual BOOL draw_item (int, DRAWITEMSTRUCT *);

  ChooseFontP cfp_font;
public:
  FontSetParam cfp_param;
  XCOLORREF cfp_org_cc[MAX_CCP_COLORS];
  FontSetParam cfp_org_param;
  int cfp_restore;

  ChooseFontPage ();
  void init_page (PropSheet *, int, PROPSHEETPAGE *);
};

class ChangeColorsDialog: public ChangeColorsPageP
{
protected:
  virtual int get_result ();
  virtual void init_dialog ();
  virtual BOOL do_command (int, int);
  virtual void notify_color (int);

public:
  int ccd_dir;
  int ccd_subdir;
  int ccd_default;
  ChangeColorsDialog ()
       : ChangeColorsPageP (USER_DEFINABLE_COLORS),
         ccd_dir (0), ccd_subdir (0), ccd_default (0)
    {}
  ~ChangeColorsDialog () {}
  void init_page (PropSheet *, int, PROPSHEETPAGE *);
};

#endif
