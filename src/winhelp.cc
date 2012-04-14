#include "stdafx.h"
#include "ed.h"
#include "resource.h"
#include "oleconv.h"

lisp
Frun_winhelp (lisp file, lisp topic)
{
  char path[PATH_MAX + 1];
  pathname2cstr (file, path);
  if (!topic || topic == Qnil)
    return boole (WinHelp (app.toplev, path, HELP_CONTENTS, 0));

  check_string (topic);
  char *b = (char *)alloca (xstring_length (topic) * 2 + 1);
  w2s (b, topic);
  return boole (WinHelp (app.toplev, path, HELP_PARTIALKEY, DWORD (b)));
}

lisp
Fkill_winhelp (lisp file)
{
  char path[PATH_MAX + 1];
  pathname2cstr (file, path);
  return boole (WinHelp (app.toplev, path, HELP_QUIT, 0));
}

struct iheader
{
  enum
    {
      nomatch = 0,
      strict = 1,
      ambigous = 2
    };
  int ih_match;
  enum {FILE_LENGTH = 12};
  char ih_file[FILE_LENGTH + 1];
  enum {TITLE_LENGTH = 64};
  char ih_title[TITLE_LENGTH + 1];
};

class ifile
{
public:
  ifile *if_next;
  FILE *if_fp;
  iheader *if_headers;
  long if_offset;
  short if_nfiles;

  ifile ();
  ~ifile ();
  int get_titles (FILE *);
};

class iset
{
  ifile *is_files;
  int is_match_all;
  int is_nmatches;
  int is_nambigous;
  int is_tplus;
public:
  iheader *is_match;
  char *is_topic;

protected:
  void find_topic (ifile *);
  int topic_index (int);
  int topic_index ();
  long topic_offset (const ifile *);
public:

  int find_index ();
  void init_files (HWND);
  iset (char *);
  ~iset ();
  void load (lisp);
  int lookup ();

};

inline
ifile::ifile ()
     : if_fp (0), if_headers (0)
{
}

ifile::~ifile ()
{
  if (if_fp)
    fclose (if_fp);
  if (if_headers)
    delete [] if_headers;
}

int
ifile::get_titles (FILE *fp)
{
  if_fp = fp;
  if (fread (&if_nfiles, sizeof if_nfiles, 1, if_fp) != 1)
    return 0;
  if_headers = new iheader [if_nfiles];
  for (int i = 0; i < if_nfiles; i++)
    {
      if_headers[i].ih_match = iheader::nomatch;
      if (fread (if_headers[i].ih_file, iheader::FILE_LENGTH, 1, if_fp) != 1
          || fread (if_headers[i].ih_title, iheader::TITLE_LENGTH, 1, if_fp) != 1)
        return 0;
      if_headers[i].ih_file[iheader::FILE_LENGTH] = 0;
      if_headers[i].ih_title[iheader::TITLE_LENGTH] = 0;
      for (char *p = if_headers[i].ih_title, *pe = p + iheader::TITLE_LENGTH;
           p < pe; p = CharNext (p))
        if (*p && *(u_char *)p < ' ')
          *p = ' ';
    }
  if_offset = (sizeof if_nfiles
               + if_nfiles * (iheader::FILE_LENGTH + iheader::TITLE_LENGTH) + 3) & ~3L;
  return 1;
}

void
iset::find_topic (ifile *f)
{
  const char *topic = is_topic + is_tplus;
  int len = strlen (topic);

  short nbytes;
  if (fread (&nbytes, sizeof nbytes, 1, f->if_fp) != 1)
    return;
  nbytes -= sizeof nbytes;

  for (int i = 0; i < f->if_nfiles; i++)
    f->if_headers[i].ih_match = iheader::nomatch;

#define GETC() (--nbytes < 0 ? EOF : getc (f->if_fp))
  int help_index;
  while ((help_index = GETC ()) != EOF)
    {
      char buf[300];
      int nchars = GETC ();
      for (int i = 0; i < nchars; i++)
        buf[i] = GETC ();
      if (help_index >= 0 && help_index < f->if_nfiles)
        {
          if (nchars == len && !memicmp (buf, topic, len))
            {
              is_match = &f->if_headers[help_index];
              f->if_headers[help_index].ih_match = iheader::strict;
              is_nmatches++;
            }
          else if (nchars > len
                   && f->if_headers[help_index].ih_match == iheader::nomatch
                   && !memicmp (buf, topic, len))
            {
              f->if_headers[help_index].ih_match = iheader::ambigous;
              is_nambigous++;
            }
        }
    }
}

int
iset::topic_index (int c)
{
  c &= 0xff;
  int n = digit_char (c);
  if (n < 36)
    return n;
  if (c == '_')
    return 36;
  return -1;
}

int
iset::topic_index ()
{
  int n1 = topic_index (*is_topic);
  if (n1 == -1)
    {
      is_tplus = 0;
      return 38 * 38; /* ??? */
    }
  if (!is_topic[1])
    {
      is_tplus = 1;
      return 1 + n1;
    }

  int n2 = topic_index (is_topic[1]);
  if (n2 == -1)
    {
      is_tplus = 0;
      return 38 * 38;
    }
  is_tplus = 2;
  return 1 + 38 + n1 * 38 + n2;
}

long
iset::topic_offset (const ifile *f)
{
  int n = topic_index ();
  if (n == -1)
    return -1;
  if (fseek (f->if_fp, f->if_offset + n * 4L, SEEK_SET))
    return -1;
  long offset;
  return fread (&offset, sizeof offset, 1, f->if_fp) == 1 ? offset : -1;
}

int
iset::find_index ()
{
  is_match_all = 0;
  is_nmatches = 0;
  is_nambigous = 0;
  if (*is_topic)
    for (ifile *f = is_files; f; f = f->if_next)
      {
        long offset = topic_offset (f);
        if (offset != -1 && !fseek (f->if_fp, offset, SEEK_SET))
          find_topic (f);
      }

  if (is_nmatches)
    {
      for (const ifile *f = is_files; f; f = f->if_next)
        for (int i = 0; i < f->if_nfiles; i++)
          if (f->if_headers[i].ih_match == iheader::ambigous)
            f->if_headers[i].ih_match = iheader::nomatch;
    }
  else if (!is_nambigous)
    is_match_all = 1;

  return is_nmatches;
}

void
iset::init_files (HWND dlg)
{
  SendDlgItemMessage (dlg, IDC_FILES, LB_RESETCONTENT, 0, 0);
  for (ifile *f = is_files; f; f = f->if_next)
    for (int i = 0; i < f->if_nfiles; i++)
      if (is_match_all || f->if_headers[i].ih_match)
        {
          int idx = SendDlgItemMessage (dlg, IDC_FILES, LB_ADDSTRING, 0,
                                        LPARAM (f->if_headers[i].ih_title));
          if (idx != LB_ERR)
            SendDlgItemMessage (dlg, IDC_FILES, LB_SETITEMDATA,
                                idx, LPARAM (&f->if_headers[i]));
        }
  SendDlgItemMessage (dlg, IDC_FILES, LB_SETCURSEL, 0, 0);
  SetDlgItemText (dlg, IDC_TOPIC, is_topic);
}

inline
iset::iset (char *topic)
     : is_topic (topic), is_files (0)
{
}

iset::~iset ()
{
  for (ifile *f = is_files, *next; f; f = next)
    {
      next = f->if_next;
      delete f;
    }
}

void
iset::load (lisp filename)
{
  char path[PATH_MAX + 1];
  pathname2cstr (filename, path);
  ifile *f = new ifile;
  FILE *fp = _fsopen (path, "rb", _SH_DENYNO);
  if (!fp)
    {
      delete f;
      return;
    }
  f->get_titles (fp);
  f->if_next = is_files;
  is_files = f;
}

static BOOL CALLBACK
select_dialog_proc (HWND dlg, UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_INITDIALOG:
      {
        iset *is = (iset *)lparam;
        SetWindowLong (dlg, DWL_USER, LONG (is));
        center_window (dlg);
        set_window_icon (dlg);
        is->init_files (dlg);
        if (HWND (wparam) == GetDlgItem (dlg, IDC_FILES))
          return 1;
        SetFocus (GetDlgItem (dlg, IDC_FILES));
        return 0;
      }

    case WM_COMMAND:
      switch (LOWORD (wparam))
        {
        case IDC_FILES:
          if (HIWORD (wparam) != LBN_DBLCLK)
            break;
          PostMessage (dlg, WM_COMMAND, MAKELONG (IDOK, 0), 0);
          return 0;

        case IDOK:
          {
            iset *is = (iset *)GetWindowLong (dlg, DWL_USER);
            char buf[256];
            GetDlgItemText (dlg, IDC_TOPIC, buf, sizeof buf);
            if (strcmp (buf, is->is_topic))
              {
                strcpy (is->is_topic, buf);
                is->find_index ();
                is->init_files (dlg);
                SetFocus (GetDlgItem (dlg, IDC_FILES));
                return 1;
              }
            int idx = SendDlgItemMessage (dlg, IDC_FILES, LB_GETCURSEL, 0, 0);
            if (idx == LB_ERR)
              return 1;
            is->is_match = (iheader *)SendDlgItemMessage (dlg, IDC_FILES, LB_GETITEMDATA,
                                                          idx, 0);
            EndDialog (dlg, IDOK);
            return 1;
          }

        case IDCANCEL:
          EndDialog (dlg, IDCANCEL);
          return 1;
        }
      return 0;

    default:
      return 0;
    }
}

int
iset::lookup ()
{
  return (is_files
          && (find_index () == 1
              || DialogBoxParam (app.hinst, MAKEINTRESOURCE (IDD_HELPDLG),
                                 get_active_window (), select_dialog_proc,
                                 LPARAM (this)) == IDOK));
}

static void
trim_spaces (char *p)
{
  for (; *p && *p != ' ' && *p != '\t'; p = CharNext (p))
    ;
  *p = 0;
}

lisp
Ffind_winhelp_path (lisp index_file, lisp ltopic)
{
  char topic[256];

  if (ltopic == Qnil)
    *topic = 0;
  else
    {
      check_string (ltopic);
      w2s (topic, topic + sizeof topic, ltopic);
    }

  iset is (topic);
  if (stringp (index_file))
    is.load (index_file);
  else
    for (lisp p = index_file; consp (p); p = xcdr (p))
      is.load (xcar (p));

  int r = is.lookup ();
  Fdo_events ();
  if (!r)
    return Qnil;

  trim_spaces (is.is_match->ih_file);

  multiple_value::value (1) = make_string (topic);
  multiple_value::count () = 2;
  return make_string (is.is_match->ih_file);
}

typedef HWND (WINAPI *HTMLHELPPROC)(HWND, LPCSTR, UINT, DWORD);

#define HH_KEYWORD_LOOKUP 0xd
#define HH_GET_LAST_ERROR 0x14

typedef struct tagHH_AKLINK
{
  int cbStruct;
  BOOL fReserved;
  LPCTSTR pszKeywords;
  LPCTSTR pszUrl;
  LPCTSTR pszMsgText;
  LPCTSTR pszMsgTitle;
  LPCTSTR pszWindow;
  BOOL fIndexOnFail;
} HH_AKLINK;

typedef struct tagHH_LAST_ERROR
{
  int cbStruct;
  HRESULT hr;
  BSTR description;
} HH_LAST_ERROR;

lisp
Fhtml_help (lisp lfile, lisp lkeyword)
{
  check_string (lfile);
  check_string (lkeyword);

  static HTMLHELPPROC HtmlHelp = (HTMLHELPPROC)GetProcAddress (LoadLibrary ("hhctrl.ocx"),
                                                               "HtmlHelpA");
  if (!HtmlHelp)
    FEsimple_error (Ehtml_help_does_not_supported);

  char *file = (char *)alloca (xstring_length (lfile) * 2 + 1);
  w2s (file, lfile);
  char *keyword = (char *)alloca (xstring_length (lkeyword) * 2 + 1);
  w2s (keyword, lkeyword);

  HH_AKLINK link = {sizeof link};
  link.pszKeywords = keyword;
  link.fIndexOnFail = 1;

  if (HtmlHelp (GetDesktopWindow (), file, HH_KEYWORD_LOOKUP, (DWORD)&link))
    return Qt;

  HH_LAST_ERROR err = {sizeof err};
  if (HtmlHelp (0, 0, HH_GET_LAST_ERROR, (DWORD)&err)
      && FAILED (err.hr))
    {
      if (err.description)
        {
          USES_CONVERSION;
          char *desc = W2A (err.description);
          SysFreeString (err.description);
          FEsimple_error (Ehtml_help_error, make_string (desc));
        }
      else
        FEsimple_win32_error (err.hr);
    }
  return Qnil;
}
