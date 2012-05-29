#ifndef _filer_h_
# define _filer_h_

#define DnD_TEST

# include "glob.h"
# include "dialogs.h"
# include "privctrl.h"
# include "DnD.h"

class Filer;
class FilerView;

struct filer_data
{
  DWORD attr;
  FILETIME time;
  double bytes;
  enum
    {
      ICON_INVALID = -100,
      ICON_INVALID_REF = ICON_INVALID + 1,
      ICON_DIRECTORY = 0,
      ICON_REGULAR_FILE = 1,
      ICON_DOTDOT = 3
    };
  volatile int icon_index;
  char name[MAX_PATH];

  void *operator new (size_t, FilerView *);
#if _MSC_VER >= 1100
  void operator delete (void *, FilerView *) {}
#endif

  filer_data (const WIN32_FIND_DATA &);
  filer_data (const FILETIME &);
  filer_data () {}
};

class ex_lock
{
public:
  class object
    {
    public:
      object ()
        {::InitializeCriticalSection (&m_crisec);}
      ~object ()
        {::DeleteCriticalSection (&m_crisec);}
      friend class ex_lock;
    private:
      void lock ()
        {::EnterCriticalSection (&m_crisec);}
      void unlock ()
        {LeaveCriticalSection (&m_crisec);}
    private:
      object (const object &);
      void operator = (const object &);
    private:
      CRITICAL_SECTION m_crisec;
    };

public:
  ex_lock (object &obj)
       : m_obj (obj)
    {m_obj.lock ();}
  ~ex_lock ()
    {m_obj.unlock ();}

private:
  ex_lock (const ex_lock &);
  void operator = (const ex_lock &);

private:
  object &m_obj;
};

class FilerView
{
public:
  enum {CHUNK_SIZE = 10200 / sizeof (filer_data)};
  enum {NHEADER_COLUMNS = 4};
  enum
    {
      SORT_NAME,
      SORT_SIZE,
      SORT_DATE,
      SORT_EXT,
      SORT_MASK = 3,
      SORT_REV = 4,
      SORT_CASE = 8,
      SORT_IDIR = 16,
      SORT_NUM = 32,
    };

public:
  struct find_chunk
    {
      find_chunk *fc_cdr;
      int fc_used;
      filer_data fc_data[CHUNK_SIZE];

      find_chunk ();
    };

public:
  HWND fv_hwnd;
  HWND fv_hwnd_mask;
  HWND fv_hwnd_path;
  HWND fv_hwnd_marks;

protected:
  enum
    {
      fv_edir,
      fv_elastdir,
      fv_emask,
      fv_efile_mask,
      fv_emax
    };
#define fv_ldir fv_lobjs[fv_edir]
#define fv_llastdir fv_lobjs[fv_elastdir]
#define fv_lmask fv_lobjs[fv_emask]
#define fv_lfile_mask fv_lobjs[fv_efile_mask]
  lisp fv_lobjs[fv_emax];
  dyn_protect_gc fv_gcpro;

  find_chunk *fv_chunk;
  int fv_sort;

  Filer *fv_parent;

  file_masks fv_masks;

  int fv_subscribed;
  int fv_marks_changed;

  int fv_retrieve_icon;
  char fv_buf[64];

#ifdef DnD_TEST
  filer_drop_target dropt;
#endif

  void cleanup_chunk ();
  int load_contents (const char *);

  void add_list_view (const char *);
  void set_mask_text (const char *) const;
  static int chdir (lisp);
  static int chdevdir (lisp);
  lisp filename (const filer_data *) const;
  static void disk_space (double, char *, int);

public:
  void *alloc_filer_data ();

  void init_view (HWND, HWND, HWND, HWND, int);
  void save_column () const;
  void sort (int);
  void sort () const;
  int get_sort () const {return fv_sort;}

  FilerView (lisp, lisp);
  ~FilerView ();
  void set_parent (Filer *);
  void setdir (lisp);
  void delayed_reload ();
  void show_marks (int = 0);

  void display_disk_info (HWND, int) const;

  void dispinfo (LV_ITEM *);

protected:
  int find_focused (LV_ITEM *);
  void set_title (const char *) const;
  void set_title () const;
  void set_path () const;

public:
  void echo_filename ();
  int forward_line (int);
  int forward_page (int);
  int goto_bof ();
  int goto_eof ();
  int scroll_left ();
  int scroll_right ();
  int set_directory (lisp);
  void reload (lisp);
  lisp get_directory () const;
  lisp get_drive () const;
  int mark (int);
  int mark_all (int);
  int toggle_mark (int);
  int toggle_all_marks (int);
  void clear_all_marks ();
  int count_marks (int);

  lisp get_mark_files (int);
  int isearch (lisp, int);
  lisp get_current_file ();
  int current_file_directory_p ();
  int current_file_dot_dot_p ();
  void set_file_mask (lisp);
  lisp get_file_mask () const;
  int mark_match_files (lisp);
  int calc_directory_size (int);

  int isearch (Char, int);
  int search (lisp, lisp, lisp, lisp);

  void activate (int);
  void subscribe_reload (lisp, int);
  void measure_item (MEASUREITEMSTRUCT *mis) const;
  void set_font () const;
  void set_colors () const;
  int modify_column_width (int, int) const;

private:
  static unsigned __stdcall thread_entry (void *);
  void thread_main ();
  void interrupt_thread ();
  void restart_thread ();

private:
  ex_lock::object fv_lockobj;
  HANDLE fv_hevent;
  HANDLE fv_hthread;
  volatile int fv_stop_thread;
  volatile int fv_sequence;
  char *fv_icon_path;
  int fv_regular_file_index;
  int fv_directory_index;
};

class ViewerWindow;

class ViewerBuffer: public Buffer
{
public:
  ViewerBuffer ();
  int readin (ViewerWindow *, const char *);
  void clean (ViewerWindow *wp);
};

class ViewerWindow: public Window
{
  static int vw_initialized;
  static const char vw_classname[];
public:
  ViewerWindow ();
  ~ViewerWindow ();
  int init (HWND, ViewerBuffer *);
  void resize (int, int, int, int);
  void update_window ();
  void repaint ();
  void update (int, int);
};

interface IContextMenu2;

class Filer: public IdleDialog
{
  static Filer *f_chain;
  static Filer *f_mlfiler;
  static int f_mlactive;
  Filer *f_last;
  void chain ();
  void unchain ();

public:
  HWND f_hwnd_status;

  static Filer *current_filer ();
  static FilerView *pview ();
  static FilerView *sview ();
  static FilerView *view (lisp);
  static int filer_ancestor_p (HWND);

protected:
  enum
    {
      f_eresult,
      f_edefmask,
      f_eguide_text,
      f_title,
      f_keymap,
      f_emax
    };
#define f_lresult f_lobjs[f_eresult]
#define f_ldefmask f_lobjs[f_edefmask]
#define f_lguide_text f_lobjs[f_eguide_text]
#define f_ltitle f_lobjs[f_title]
#define f_lkeymap f_lobjs[f_keymap]
  lisp f_lobjs[f_emax];
  dyn_protect_gc f_gcpro;

  enum {TID_GC = 1, TID_RELOAD, TID_IDLE};
  int f_multi;
  int f_fdispatch;

  RECT f_guide_area;
  int f_guide_nlines;
  int f_guide_height;
  enum {MAX_GUIDE_NLINES = 5};

  FilerView f_fv1;
  FilerView f_fv2;
  FilerView *f_pview;
  FilerView *f_sview;

  ViewerBuffer f_vbuffer;
  ViewerWindow f_vwindow;
  int f_viewer_on;
  FilerView *f_changed_view;
  int f_idle_timer;

  IContextMenu2 *f_ctx_menu2;
  key_sequence f_keyseq;

protected:
  int dispatch (lChar);
  void save_geometry_p (int &, int &) const;
  void save_geometry () const;
  int ancestor_p (HWND) const;
  virtual int IsDialogMessage (MSG *msg);

public:
  lisp result ();
  Filer (lisp, lisp, lisp, lisp, lisp, lisp, lisp, lisp, int);
  ~Filer ();
  virtual BOOL WndProc (UINT, WPARAM, LPARAM);
  void restore_dir () const;
  int left_window_p (const FilerView *) const;
  int primary_window_p (const FilerView *) const;
  virtual void IdleProc ();
  void end_dispatch ();
  void demand_reload () const;
  void set_colors () const;
  int check_idle ();

protected:
  BOOL InitDialog ();
  BOOL Notify (NMHDR *);
  BOOL Command (WPARAM, LPARAM);
  int process_keys (const LV_PROCESSKEY *);
  void Size (int, int);
  int adjust_control (int, int, int);
  BOOL GetMinMaxInfo (MINMAXINFO *);
  void Paint ();
  void item_changed (NM_LISTVIEW *);
  void do_keyup ();
  void context_menu (NMHDR *);

public:
  lisp title () const;
  void close (lisp);
  void quit ();
  lisp get_text ();
  void set_text (lisp);
  int dual_window_p () const;
  int left_window ();
  int right_window ();
  int left_window_p () const;
  void subscribe_reload (lisp, int) const;
  void show_disk_info (const FilerView *) const;
  void show_viewer ();
  FilerView *pv () const {return f_pview;};
  FilerView *sv () const {return f_sview;};
  int modeless_p () const {return IdleDialog::modeless_p ();}
  int context_menu (FilerView *);
  int swap_windows (FilerView *);
  static int activate_modeless ();
  static void close_mlfiler ();
  static void modify_colors ();
  lChar read_char () const;
};

///////////////////////////////////////////////////////////////
// filer_data inline functions
///////////////////////////////////////////////////////////////

inline void *
filer_data::operator new (size_t, FilerView *fv)
{
  return fv->alloc_filer_data ();
}

inline
filer_data::filer_data (const WIN32_FIND_DATA &fd)
{
  attr = fd.dwFileAttributes;
  time = fd.ftLastWriteTime;
  bytes = (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY
           ? -1.0
           : fd.nFileSizeHigh * 4294967296.0 + fd.nFileSizeLow);
  strcpy (name, fd.cFileName);
  icon_index = ICON_INVALID;
}

inline
filer_data::filer_data (const FILETIME &ft)
{
  attr = FILE_ATTRIBUTE_DIRECTORY;
  time = ft;
  bytes = -1.0;
  *name = 0;
  icon_index = ICON_INVALID;
}

///////////////////////////////////////////////////////////////
// FilerView inline functions
///////////////////////////////////////////////////////////////

inline
FilerView::find_chunk::find_chunk ()
     : fc_used (0)
{
}

inline void *
FilerView::alloc_filer_data ()
{
  if (!fv_chunk || fv_chunk->fc_used == CHUNK_SIZE)
    {
      find_chunk *p = new find_chunk ();
      p->fc_cdr = fv_chunk;
      fv_chunk = p;
    }
  return &fv_chunk->fc_data[fv_chunk->fc_used++];
}

inline void
FilerView::set_parent (Filer *p)
{
  fv_parent = p;
}

inline void
FilerView::setdir (lisp d)
{
  fv_ldir = d;
}

inline int
FilerView::forward_line (int n)
{
  int x = ListView_ForwardLine (fv_hwnd, n);
  echo_filename ();
  return x;
}

inline int
FilerView::forward_page (int n)
{
  int x = ListView_ForwardPage (fv_hwnd, n);
  echo_filename ();
  return x;
}

inline int
FilerView::goto_bof ()
{
  int x = ListView_GotoBOF (fv_hwnd);
  echo_filename ();
  return x;
}

inline int
FilerView::goto_eof ()
{
  int x = ListView_GotoEOF (fv_hwnd);
  echo_filename ();
  return x;
}

inline int
FilerView::scroll_left ()
{
  POINT p1, p2;
  ListView_GetItemPosition (fv_hwnd, 0, &p1);
  int x = ListView_Scroll (fv_hwnd, -6, 0);
  ListView_GetItemPosition (fv_hwnd, 0, &p2);
  return x && p1.x != p2.x;
}

inline int
FilerView::scroll_right ()
{
  POINT p1, p2;
  ListView_GetItemPosition (fv_hwnd, 0, &p1);
  int x = ListView_Scroll (fv_hwnd, 6, 0);
  ListView_GetItemPosition (fv_hwnd, 0, &p2);
  return x && p1.x != p2.x;
}

inline lisp
FilerView::get_directory () const
{
  return fv_ldir;
}

inline int
FilerView::isearch (Char c, int wrap)
{
  if (c < ' ')
    c += '@';
  return ListView_ISearch (fv_hwnd, c, wrap);
}

inline void
FilerView::set_file_mask (lisp lmasks)
{
  fv_masks = lmasks;
  fv_lfile_mask = lmasks;
}

inline lisp
FilerView::get_file_mask () const
{
  return fv_lfile_mask;
}

///////////////////////////////////////////////////////////////
// Filer inline functions
///////////////////////////////////////////////////////////////

inline
Filer::~Filer ()
{
  unchain ();
}

inline void
Filer::chain ()
{
  if (!auto_delete_p ())
    {
      f_last = f_chain;
      f_chain = this;
    }
  else
    f_mlfiler = this;
}

inline void
Filer::unchain ()
{
  if (!auto_delete_p ())
    f_chain = f_last;
  else if (f_mlfiler == this)
    f_mlfiler = 0;
}

inline Filer *
Filer::current_filer ()
{
  if ((f_mlactive || !f_chain) && f_mlfiler)
    return f_mlfiler;
  if (!f_chain)
    FEsimple_error (Efiler_is_not_open);
  return f_chain;
}

inline FilerView *
Filer::pview ()
{
  return current_filer ()->f_pview;
}

inline FilerView *
Filer::sview ()
{
  if (!current_filer ()->f_sview)
    FEsimple_error (Efiler_is_not_in_dual_window_mode);
  return current_filer ()->f_sview;
}

inline FilerView *
Filer::view (lisp v)
{
  return !v || v == Qnil ? pview () : sview ();
}

inline int
Filer::left_window_p (const FilerView *v) const
{
  return v == &f_fv1;
}

inline int
Filer::primary_window_p (const FilerView *v) const
{
  return v == f_pview;
}

inline int
Filer::dual_window_p () const
{
  return int (f_sview);
}

inline void
Filer::restore_dir () const
{
  WINFS::SetCurrentDirectory (sysdep.curdir);
}

inline lisp
Filer::title () const
{
  return f_ltitle;
}

inline void
Filer::close (lisp result)
{
  f_lresult = result;
  EndDialog (result == Qnil ? IDCANCEL : IDOK);
}

inline int
Filer::left_window ()
{
  return swap_windows (&f_fv1);
}

inline int
Filer::right_window ()
{
  return swap_windows (&f_fv2);
}

inline int
Filer::left_window_p () const
{
  return f_pview == &f_fv1;
}

#endif
