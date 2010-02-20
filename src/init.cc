#include "ed.h"
#include <float.h>
#include <new.h>
#include <io.h>
#include <eh.h>
#include <fcntl.h>
#include <objbase.h>
#include "ctl3d.h"
#include "environ.h"
#include "except.h"
#include "privctrl.h"
#include "xdde.h"
#include "fnkey.h"
#include "syntaxinfo.h"
#include "ipc.h"
#include "sock.h"
#include "conf.h"
#include "colors.h"
#ifdef DEBUG
# include "mainframe.h"
# include <crtdbg.h>
#endif

#ifndef M_PI
# define M_PI 3.141592653589793
#endif

const char Application::ToplevelClassName[] = "Å@";
const char Application::FrameClassName[] = "  ";
const char Application::ClientClassName[] = "   ";
const char Application::ModelineClassName[] = "    ";
const char FunctionKeyClassName[] = "     ";

Application app;

char enable_quit::q_enable;

Application::Application ()
     : mouse (kbdq)
{
  default_tab_columns = 8;
  auto_save_count = 0;
  toplevel_is_active = 0;
  ime_composition = 0;
  ime_open_mode = kbd_queue::IME_MODE_OFF;
  sleep_timer_exhausted = 0;
  last_vkeycode = -1;
  kbd_repeat_count = 0;
  wait_cursor_depth = 0;
  f_in_drop = 0;
  drop_window = 0;
  drag_window = 0;
  drag_buffer = 0;
  f_protect_quit = 0;
  hwnd_clipboard = 0;
  last_cmd_tick = GetTickCount ();
  f_auto_save_pending = 0;
  default_caret_blink_time = 0;
  last_blink_caret = 0;
  lquit_char = make_char ('G' - '@');
  quit_vkey = 'G';
  quit_mod = MOD_CONTROL;
  ini_file_path = 0;
  minibuffer_prompt_column = -1;

  int tem;
  initial_stack = &tem;
  in_gc = 0;
}

Application::~Application ()
{
  xfree (ini_file_path);
}

static lisp
make_path (const char *s, int append_slash = 1)
{
  Char *b = (Char *)alloca ((strlen (s) + 1) * sizeof (Char));
  Char *be = s2w (b, s);
  map_backsl_to_sl (b, be - b);
  if (append_slash && be != b && be[-1] != '/')
    *be++ = '/';
  return make_string (b, be - b);
}

static void
init_module_dir ()
{
  char path[PATH_MAX];
  GetModuleFileName (0, path, sizeof path);
  char *p = jrindex (path, '\\');
  if (p)
    p[1] = 0;
  xsymbol_value (Qmodule_dir) = make_path (path);
}

static inline void
init_current_dir ()
{
  xsymbol_value (Qdefault_dir) = make_path (sysdep.curdir);
}

static void
init_windows_dir ()
{
  char path[PATH_MAX];
  GetWindowsDirectory (path, sizeof path);
  xsymbol_value (Qwindows_dir) = make_path (path);

  GetSystemDirectory (path, sizeof path);
  xsymbol_value (Qsystem_dir) = make_path (path);
}

static int
init_home_dir (const char *path)
{
  char home[PATH_MAX], *tem;
  int l = WINFS::GetFullPathName (path, sizeof home, home, &tem);
  if (!l || l >= sizeof home)
    return 0;
  DWORD f = WINFS::GetFileAttributes (home);
  if (f == -1 || !(f & FILE_ATTRIBUTE_DIRECTORY))
    return 0;
  xsymbol_value (Qhome_dir) = make_path (home);
  return 1;
}

static void
init_home_dir ()
{
  char path[PATH_MAX];
  static const char xyzzyhome[] = "XYZZYHOME";
  static const char cfgInit[] = "init";

  if (read_conf (cfgInit, "homeDir", path, sizeof path)
      && init_home_dir (path))
    return;

  for (int i = 0; i <= 5; i += 5)
    {
      char *e = getenv (xyzzyhome + i);
      if (e && init_home_dir (e))
        return;
    }

  char *drive = getenv ("HOMEDRIVE");
  char *dir = getenv ("HOMEPATH");
  if (drive && dir && strlen (drive) + strlen (dir) < sizeof path - 1)
    {
      strcpy (stpcpy (path, drive), dir);
      if (init_home_dir (path))
        return;
    }

  if (read_conf (cfgInit, "logDir", path, sizeof path)
      && init_home_dir (path))
    return;

  xsymbol_value (Qhome_dir) = xsymbol_value (Qmodule_dir);
}

static void
init_load_path ()
{
  lisp l = make_string ("lisp");
  xsymbol_value (Vload_path) = Qnil;
  if (Fequalp (xsymbol_value (Qmodule_dir),
               xsymbol_value (Qdefault_dir)) == Qnil)
    xsymbol_value (Vload_path) =
      xcons (Fmerge_pathnames (l, xsymbol_value (Qdefault_dir)),
             xsymbol_value (Vload_path));

  xsymbol_value (Vload_path) =
    xcons (Fmerge_pathnames (l, xsymbol_value (Qmodule_dir)),
           xsymbol_value (Vload_path));

  xsymbol_value (Vload_path) =
    xcons (Fmerge_pathnames (make_string ("site-lisp"),
                             xsymbol_value (Qmodule_dir)),
           xsymbol_value (Vload_path));
}

static void
init_user_config_path (const char *config_path)
{
  if (!config_path)
    config_path = getenv ("XYZZYCONFIGPATH");
  if (config_path)
    {
      char path[PATH_MAX], *tem;
      int l = WINFS::GetFullPathName (config_path, sizeof path, path, &tem);
      if (l && l < sizeof path)
        {
          DWORD a = WINFS::GetFileAttributes (path);
          if (a != DWORD (-1) && a & FILE_ATTRIBUTE_DIRECTORY)
            {
              xsymbol_value (Quser_config_path) = make_path (path);
              return;
            }
        }
    }

  char *path = (char *)alloca (w2sl (xsymbol_value (Qmodule_dir))
                               + w2sl (xsymbol_value (Vuser_name))
                               + 32);
  char *p = stpcpy (w2s (path, xsymbol_value (Qmodule_dir)), "usr");
  WINFS::CreateDirectory (path, 0);
  *p++ = '/';
  p = w2s (p, xsymbol_value (Vuser_name));
  WINFS::CreateDirectory (path, 0);
  *p++ = '/';
  strcpy (p, sysdep.windows_short_name);
  WINFS::CreateDirectory (path, 0);
  DWORD a = WINFS::GetFileAttributes (path);
  if (a != DWORD (-1) && a & FILE_ATTRIBUTE_DIRECTORY)
    xsymbol_value (Quser_config_path) = make_path (path);
  else
    xsymbol_value (Quser_config_path) = xsymbol_value (Qmodule_dir);
}

static void
init_user_inifile_path (const char *ini_file)
{
  if (!ini_file)
    ini_file = getenv ("XYZZYINIFILE");
  if (ini_file && find_slash (ini_file))
    {
      char path[PATH_MAX], *tem;
      int l = WINFS::GetFullPathName (ini_file, sizeof path, path, &tem);
      if (l && l < sizeof path)
        {
          HANDLE h = CreateFile (path, GENERIC_READ, 0, 0, OPEN_ALWAYS,
                                 FILE_ATTRIBUTE_ARCHIVE, 0);
          if (h != INVALID_HANDLE_VALUE)
            {
              CloseHandle (h);
              app.ini_file_path = xstrdup (path);
              return;
            }
        }
    }

  if (!ini_file)
    ini_file = "xyzzy.ini";

  char *path = (char *)alloca (w2sl (xsymbol_value (Quser_config_path))
                               + strlen (ini_file) + 32);
  strcpy (w2s (path, xsymbol_value (Quser_config_path)), ini_file);
  app.ini_file_path = xstrdup (path);
}

static void
init_dump_path ()
{
  if (!*app.dump_image)
    {
      int l = GetModuleFileName (0, app.dump_image, PATH_MAX);
      char *e = app.dump_image + l;
      if (l > 4 && !_stricmp (e - 4, ".exe"))
        e -= 3;
      else
        *e++ = '.';
      strcpy (e, sysdep.windows_short_name);
    }
}

static void
init_env_symbols (const char *config_path, const char *ini_file)
{
  xsymbol_value (Vfeatures) = xcons (Kxyzzy, xcons (Kieee_floating_point, Qnil));
  xsymbol_value (Qdump_image_path) = make_path (app.dump_image, 0);
  init_module_dir ();
  init_current_dir ();
  init_environ ();
  init_user_config_path (config_path);
  init_user_inifile_path (ini_file);
  init_home_dir ();
  init_load_path ();
  init_windows_dir ();
}

#pragma optimize ("g", off)
static void
init_math_symbols ()
{
#define CP(T, F) (xsymbol_value (T) = xsymbol_value (F))

  xsymbol_value (Qmost_positive_single_float) = make_single_float (FLT_MAX);
  xsymbol_value (Qmost_negative_single_float) = make_single_float (-FLT_MAX);
  for (float fl = 1.0F, fe = 1.1F; fl && fe > fl; fe = fl, fl /= 2.0F)
    ;
  xsymbol_value (Qleast_positive_single_float) = make_single_float (fe);
  xsymbol_value (Qleast_negative_single_float) = make_single_float (-fe);
  xsymbol_value (Qleast_positive_normalized_single_float) =
    make_single_float (FLT_MIN);
  xsymbol_value (Qleast_negative_normalized_single_float) =
    make_single_float (-FLT_MIN);
  for (fl = 1.0F, fe = 1.1F; 1.0F + fl != 1.0F && fe > fl; fe = fl, fl /= 2.0F)
    ;
  xsymbol_value (Qsingle_float_epsilon) = make_single_float (fe);
  for (fl = 1.0F, fe = 1.1F; 1.0F - fl != 1.0F && fe > fl; fe = fl, fl /= 2.0F)
    ;
  xsymbol_value (Qsingle_float_negative_epsilon) = make_single_float (fe);

  CP (Qmost_positive_short_float, Qmost_positive_single_float);
  CP (Qmost_negative_short_float, Qmost_negative_single_float);
  CP (Qleast_positive_short_float, Qleast_positive_single_float);
  CP (Qleast_negative_short_float, Qleast_negative_single_float);
  CP (Qleast_positive_normalized_short_float,
      Qleast_positive_normalized_single_float);
  CP (Qleast_negative_normalized_short_float,
      Qleast_negative_normalized_single_float);
  CP (Qshort_float_epsilon, Qsingle_float_epsilon);
  CP (Qshort_float_negative_epsilon, Qsingle_float_negative_epsilon);

  xsymbol_value (Qmost_positive_double_float) = make_double_float (DBL_MAX);
  xsymbol_value (Qmost_negative_double_float) = make_double_float (-DBL_MAX);
  for (double dl = 1.0, de = 1.1; dl && de > dl; de = dl, dl /= 2.0)
    ;
  xsymbol_value (Qleast_positive_double_float) = make_double_float (de);
  xsymbol_value (Qleast_negative_double_float) = make_double_float (-de);
  xsymbol_value (Qleast_positive_normalized_double_float) =
    make_double_float (DBL_MIN);
  xsymbol_value (Qleast_negative_normalized_double_float) =
    make_double_float (-DBL_MIN);
  for (dl = 1.0, de = 1.1; 1.0 + dl != 1.0 && de > dl; de = dl, dl /= 2.0)
    ;
  xsymbol_value (Qdouble_float_epsilon) = make_double_float (de);
  for (dl = 1.0, de = 1.1; 1.0 - dl != 1.0 && de > dl; de = dl, dl /= 2.0)
    ;
  xsymbol_value (Qdouble_float_negative_epsilon) = make_double_float (de);

  CP (Qmost_positive_long_float, Qmost_positive_double_float);
  CP (Qleast_positive_long_float, Qleast_positive_double_float);
  CP (Qleast_negative_long_float, Qleast_negative_double_float);
  CP (Qmost_negative_long_float, Qmost_negative_double_float);
  CP (Qleast_positive_normalized_long_float,
      Qleast_positive_normalized_double_float);
  CP (Qleast_negative_normalized_long_float,
      Qleast_negative_normalized_double_float);
  CP (Qlong_float_epsilon, Qdouble_float_epsilon);
  CP (Qlong_float_negative_epsilon, Qdouble_float_negative_epsilon);

  xsymbol_value (Qmost_positive_fixnum) = make_fixnum (LONG_MAX);
  xsymbol_value (Qmost_negative_fixnum) = make_fixnum (LONG_MIN);

  xsymbol_value (Qpi) = make_double_float (M_PI);
  xsymbol_value (Qimag_two) = make_complex (make_fixnum (0), make_fixnum (2));

#undef CP
}
#pragma optimize ("", on)

static void
init_symbol_value_once ()
{
  xsymbol_value (Qt) = Qt;

  xsymbol_value (Vprint_readably) = Qnil;
  xsymbol_value (Vprint_escape) = Qt;
  xsymbol_value (Vprint_pretty) = Qt;
  xsymbol_value (Vprint_base) = make_fixnum (10);
  xsymbol_value (Vprint_radix) = Qnil;
  xsymbol_value (Vprint_circle) = Qnil;
  xsymbol_value (Vprint_length) = Qnil;
  xsymbol_value (Vprint_level) = Qnil;

  xsymbol_value (Vload_verbose) = Qt;
  xsymbol_value (Vload_print) = Qnil;
  xsymbol_value (Vload_pathname) = Qnil;

  xsymbol_value (Vrandom_state) = Fmake_random_state (Qt);
  xsymbol_value (Vdefault_random_state) = xsymbol_value (Vrandom_state);

  xsymbol_value (Qcall_arguments_limit) = make_fixnum (MAX_VECTOR_LENGTH);
  xsymbol_value (Qlambda_parameters_limit) = make_fixnum (MAX_VECTOR_LENGTH);
  xsymbol_value (Qmultiple_values_limit) = make_fixnum (MULTIPLE_VALUES_LIMIT);

  init_math_symbols ();
  init_readtable ();

  xsymbol_value (Qchar_code_limit) = make_fixnum (CHAR_LIMIT);

  xsymbol_value (Qarray_rank_limit) = make_fixnum (ARRAY_RANK_LIMIT);
  xsymbol_value (Qarray_dimension_limit) = make_fixnum (MAX_VECTOR_LENGTH);
  xsymbol_value (Qarray_total_size_limit) = make_fixnum (MAX_VECTOR_LENGTH);

  xsymbol_value (Qinternal_time_units_per_second) = make_fixnum (1000);

  xsymbol_value (Vcreate_buffer_hook) = Qnil;
  xsymbol_value (Vdefault_fileio_encoding) = xsymbol_value (Qencoding_sjis);
  xsymbol_value (Vexpected_fileio_encoding) = xsymbol_value (Qencoding_auto);
  xsymbol_value (Vdefault_eol_code) = make_fixnum (eol_crlf);
  xsymbol_value (Vexpected_eol_code) = make_fixnum (eol_guess);

  xsymbol_value (Qor_string_integer) =
    xcons (Qor, xcons (Qstring, xcons (Qinteger, Qnil)));
  xsymbol_value (Qor_symbol_integer) =
    xcons (Qor, xcons (Qsymbol, xcons (Qinteger, Qnil)));
  xsymbol_value (Qor_string_character) =
    xcons (Qor, xcons (Qstring, xcons (Qcharacter, Qnil)));
  xsymbol_value (Qor_integer_marker) =
    xcons (Qor, xcons (Qinteger, xcons (Qmarker, Qnil)));
  xsymbol_value (Qor_character_cons) =
    xcons (Qor, xcons (Qcharacter, xcons (Qcons, Qnil)));
  xsymbol_value (Qor_symbol_string) =
    xcons (Qor, xcons (Qsymbol, xcons (Qstring, Qnil)));
  xsymbol_value (Qor_string_stream) =
    xcons (Qor, xcons (Qstring, xcons (Qstream, Qnil)));

  xsymbol_value (Vread_default_float_format) = Qsingle_float;

  xsymbol_value (Vscroll_bar_step) = make_fixnum (2);

  xsymbol_value (Vglobal_keymap) = Fmake_keymap ();
  xsymbol_value (Vselection_keymap) = Qnil;
  xsymbol_value (Vkept_undo_information) = make_fixnum (1000);
  xsymbol_value (Vbuffer_read_only) = Qnil;
  xsymbol_value (Venable_meta_key) = Qt;
  xsymbol_value (Vlast_command_char) = Qnil;
  xsymbol_value (Vneed_not_save) = Qnil;
  xsymbol_value (Vauto_save) = Qt;
  xsymbol_value (Vbeep_on_error) = Qt;
  xsymbol_value (Vbeep_on_warn) = Qt;
  xsymbol_value (Vbeep_on_never) = Qnil;
  xsymbol_value (Vprefix_value) = Qnil;
  xsymbol_value (Vprefix_args) = Qnil;
  xsymbol_value (Vnext_prefix_value) = Qnil;
  xsymbol_value (Vnext_prefix_args) = Qnil;
  xsymbol_value (Vdefault_syntax_table) = Fmake_syntax_table ();
  xsymbol_value (Vauto_fill) = Qnil;
  xsymbol_value (Vthis_command) = Qnil;
  xsymbol_value (Vlast_command) = Qnil;

  xsymbol_value (Qsoftware_type) = make_string (ProgramName);
  xsymbol_value (Qsoftware_version) = make_string (VersionString);
  xsymbol_value (Qsoftware_version_display_string) =
    make_string (DisplayVersionString);

  xsymbol_value (Qtemporary_string) = make_string_simple ("", 0);

  xsymbol_value (Vversion_control) = Qt;
  xsymbol_value (Vkept_old_versions) = make_fixnum (2);
  xsymbol_value (Vkept_new_versions) = make_fixnum (2);
  xsymbol_value (Vmake_backup_files) = Qt;
  xsymbol_value (Vmake_backup_file_always) = Qnil;
  xsymbol_value (Vpack_backup_file_name) = Qt;
  xsymbol_value (Vauto_save_interval) = make_fixnum (256);
  xsymbol_value (Vauto_save_interval_timer) = make_fixnum (30);
  xsymbol_value (Vbackup_by_copying) = Qnil;

  xsymbol_value (Vinverse_mode_line) = Qt;
  xsymbol_value (Vbuffer_list_sort_ignore_case) = Qt;
  xsymbol_value (Veat_mouse_activate) = Qt;
  xsymbol_value (Vindent_tabs_mode) = Qt;

  xsymbol_value (Slock_file) = Qnil;
  xsymbol_value (Vexclusive_lock_file) = Qnil;

  xsymbol_value (Vcursor_shape) = Karrow;
  xsymbol_value (Vhide_restricted_region) = Qnil;

  xsymbol_value (Vfiler_last_command_char) = Qnil;
  xsymbol_value (Vfiler_dual_window) = Qnil;
  xsymbol_value (Vfiler_left_window_p) = Qt;
  xsymbol_value (Vfiler_secondary_directory) = Qnil;
  xsymbol_value (Vfiler_click_toggle_marks_always) = Qt;

  xsymbol_value (Vdll_module_list) = Qnil;

  xsymbol_value (Vfunction_bar_labels) =
    make_vector (MAX_FUNCTION_BAR_LABEL, Qnil);

  xsymbol_value (Vkeyword_hash_table) = Qnil;
  xsymbol_value (Vhighlight_keyword) = Qt;
  xsymbol_value (Vhtml_highlight_mode) = Qnil;

  xsymbol_value (Vblink_caret) = Qt;

  xsymbol_value (Vparentheses_hash_table) = Qnil;

  xsymbol_value (Vdefault_kinsoku_bol_chars) = Qnil;
  xsymbol_value (Vdefault_kinsoku_eol_chars) = Qnil;

  xsymbol_value (Vdde_timeout) = make_fixnum (30000);
  xsymbol_value (Vbrackets_is_wildcard_character) = Qt;

  init_char_encoding ();

  xsymbol_value (Vbypass_evalhook) = Qnil;
  xsymbol_value (Vbypass_applyhook) = Qnil;

  xsymbol_value (Vtitle_bar_format) = Qnil;
  xsymbol_value (Vstatus_bar_format) = Qnil;
  xsymbol_value (Vlast_status_bar_format) = Qnil;
  xsymbol_value (Vscroll_margin) = make_fixnum (0);
  xsymbol_value (Vjump_scroll_threshold) = make_fixnum (3);
  xsymbol_value (Vauto_update_per_device_directory) = Qt;
  xsymbol_value (Vmodal_filer_save_position) = Qt;
  xsymbol_value (Vmodal_filer_save_size) = Qt;
  xsymbol_value (Vfiler_echo_filename) = Qt;
  xsymbol_value (Vfiler_eat_esc) = Qt;
  xsymbol_value (Vsupport_mouse_wheel) = Qt;
  xsymbol_value (Vminibuffer_save_ime_status) = Qt;
  xsymbol_value (Vuse_shell_execute_ex) = Qt;
  xsymbol_value (Vshell_execute_disregards_shift_key) = Qt;
  xsymbol_value (Vregexp_keyword_list) = Qnil;
  xsymbol_value (Vunicode_to_half_width) = Qt;
  xsymbol_value (Vcolor_page_enable_dir_p) = Qnil;
  xsymbol_value (Vcolor_page_enable_subdir_p) = Qnil;
}

static void
init_symbol_value ()
{
  xsymbol_value (Vquit_flag) = Qnil;
  xsymbol_value (Vinhibit_quit) = Qnil;
  xsymbol_value (Voverwrite_mode) = Qnil;
  xsymbol_value (Vprocess_list) = Qnil;
  xsymbol_value (Vminibuffer_message) = Qnil;
  xsymbol_value (Vsi_find_motion) = Qt;
  xsymbol_value (Vdefault_menu) = Qnil;
  xsymbol_value (Vlast_active_menu) = Qnil;

  xsymbol_value (Vreader_in_backquote) = Qnil;
  xsymbol_value (Vreader_preserve_white) = Qnil;
  xsymbol_value (Vread_suppress) = Qnil;
  xsymbol_value (Vread_eval) = Qt;
  xsymbol_value (Vreader_label_alist) = Qnil;

  xsymbol_value (Vclipboard_newer_than_kill_ring_p) = Qnil;
  xsymbol_value (Vkill_ring_newer_than_clipboard_p) = Qnil;

  xsymbol_value (Vkbd_encoding) = xsymbol_value (Qencoding_sjis);
  xsymbol_value (Qperformance_counter_frequency) =
    (sysdep.perf_counter_present_p
     ? make_integer (*(large_int *)&sysdep.perf_freq)
     : make_fixnum (1000));

  xsymbol_value (Vsi_accept_kill_xyzzy) = Qt;
  xsymbol_value (Vlast_match_string) = Qnil;
}

static void
init_command_line (int ac)
{
  lisp p = Qnil;
  for (int i = __argc - 1; i >= ac; i--)
    p = xcons (make_string (__argv[i]), p);
  xsymbol_value (Vsi_command_line_args) = p;
}

void
report_out_of_memory ()
{
  MessageBox (0, "ÉÅÉÇÉäÇ™ïsë´ÇµÇƒÇ¢Ç‹Ç∑", TitleBarString, MB_OK | MB_ICONHAND);
}

static inline int
check_dump_key ()
{
  return (GetAsyncKeyState (VK_SHIFT) < 0
          && GetAsyncKeyState (VK_CONTROL) < 0);
}

static int
init_lisp_objects ()
{
  const char *config_path = 0, *ini_file = 0;
  *app.dump_image = 0;

  for (int ac = 1; ac < __argc - 1; ac += 2)
    if (!strcmp (__argv[ac], "-image"))
      {
        char *tem;
        int l = WINFS::GetFullPathName (__argv[ac + 1], sizeof app.dump_image,
                                        app.dump_image, &tem);
        if (!l || l >= sizeof app.dump_image)
          *app.dump_image = 0;
      }
    else if (!strcmp (__argv[ac], "-config"))
      config_path = __argv[ac + 1];
    else if (!strcmp (__argv[ac], "-ini"))
      ini_file = __argv[ac + 1];
    else
      break;

  try
    {
      init_dump_path ();
      if ((ac < __argc || !check_dump_key ())
          && rdump_xyzzy ())
        {
          combine_syms ();
          rehash_all_hash_tables ();
        }
      else
        {
          init_syms ();
          init_symbol_value_once ();
          init_condition ();
        }
      init_symbol_value ();
      init_syntax_spec ();
      init_env_symbols (config_path, ini_file);
      xsymbol_value (Vconvert_registry_to_file_p) = boole (reg2ini ());
      load_misc_colors ();
      init_command_line (ac);
      syntax_state::init_color_table ();
      create_std_streams ();
    }
  catch (nonlocal_jump &)
    {
      app.active_frame.selected = 0;
      report_out_of_memory ();
      return 0;
    }
  return 1;
}

static int
init_editor_objects ()
{
  try
    {
      Window::create_default_windows ();
      create_default_buffers ();
    }
  catch (nonlocal_jump &)
    {
      app.active_frame.selected = 0;
      report_out_of_memory ();
      return 0;
    }
  return 1;
}

lisp
Fsi_startup ()
{
  return Fsi_load_library (make_string ("startup"), Qnil);
}

static int
register_wndclasses (HINSTANCE hinst)
{
  WNDCLASS wc;

  wc.style = 0;
  wc.lpfnWndProc = toplevel_wndproc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = 0;
  wc.hInstance = hinst;
  wc.hIcon = LoadIcon (hinst, MAKEINTRESOURCE (IDI_XYZZY));
  wc.hCursor = sysdep.hcur_arrow;
  wc.hbrBackground = 0;
  wc.lpszMenuName = 0;
  wc.lpszClassName = Application::ToplevelClassName;
  app.atom_toplev = RegisterClass (&wc);
  if (!app.atom_toplev)
    return 0;

  wc.style = 0;
  wc.lpfnWndProc = frame_wndproc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = 0;
  wc.hInstance = hinst;
  wc.hIcon = 0;
  wc.hCursor = sysdep.hcur_arrow;
  wc.hbrBackground = 0;
  wc.lpszMenuName = 0;
  wc.lpszClassName = Application::FrameClassName;
  if (!RegisterClass (&wc))
    return 0;

  wc.style = 0;
  wc.lpfnWndProc = client_wndproc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = sizeof (Window *);
  wc.hInstance = hinst;
  wc.hIcon = 0;
  wc.hCursor = sysdep.hcur_arrow;
  wc.hbrBackground = 0;
  wc.lpszMenuName = 0;
  wc.lpszClassName = Application::ClientClassName;
  if (!RegisterClass (&wc))
    return 0;

  wc.style = 0;
  wc.lpfnWndProc = modeline_wndproc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = sizeof (Window *);
  wc.hInstance = hinst;
  wc.hIcon = 0;
  wc.hCursor = sysdep.hcur_sizens;
  wc.hbrBackground = HBRUSH (COLOR_BTNFACE + 1);
  wc.lpszMenuName = 0;
  wc.lpszClassName = Application::ModelineClassName;
  if (!RegisterClass (&wc))
    return 0;

  wc.style = CS_HREDRAW | CS_VREDRAW;
  wc.lpfnWndProc = fnkey_wndproc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = sizeof (void *);
  wc.hInstance = hinst;
  wc.hIcon = 0;
  wc.hCursor = sysdep.hcur_arrow;
  wc.hbrBackground = HBRUSH (COLOR_BTNFACE + 1);
  wc.lpszMenuName = 0;
  wc.lpszClassName = FunctionKeyClassName;
  if (!RegisterClass (&wc))
    return 0;

  stdctl_hook_init (hinst);

  return 1;
}

static int __cdecl
handle_new_failure (size_t)
{
  FEstorage_error ();
  return 0;
}

static void
copy_handle (DWORD f, int fd)
{
  HANDLE o = GetStdHandle (f), n;
  HANDLE hproc = GetCurrentProcess ();
  if (DuplicateHandle (hproc, o, hproc, &n,
                       0, 0, DUPLICATE_SAME_ACCESS))
    {
      _close (fd);
      _open_osfhandle (long (n), _O_TEXT | (fd ? 0 : _O_RDONLY));
      SetStdHandle (f, n);
    }
}

static void
pre_allocate_stack1 ()
{
  alloca (1024 * 1024);
}

static int
pre_allocate_stack ()
{
  int ok = 1;
  try
    {
      pre_allocate_stack1 ();
    }
  catch (Win32Exception &)
    {
      ok = 0;
    }
  return ok;
}

static int
sw_minimized_p (int sw)
{
  switch (sw)
    {
    case SW_SHOWMINIMIZED:
    case SW_MINIMIZE:
    case SW_SHOWMINNOACTIVE:
      return 1;

    default:
      return 0;
    }
}

static inline int
sw_maximized_p (int sw)
{
  return sw == SW_SHOWMAXIMIZED;
}

static int
init_app (HINSTANCE hinst, int passed_cmdshow, int &ole_initialized)
{
  SetErrorMode (SEM_FAILCRITICALERRORS | SEM_NOOPENFILEERRORBOX);
  app.toplev = 0;

  init_ucs2_table ();

  copy_handle (STD_INPUT_HANDLE, 0);
  copy_handle (STD_OUTPUT_HANDLE, 1);
  copy_handle (STD_ERROR_HANDLE, 2);

  Ctl3d::enable (hinst);

  _set_new_handler (handle_new_failure);
  _set_se_translator (se_handler);

  if (!pre_allocate_stack ())
    {
      report_out_of_memory ();
      return 0;
    }

  if (*sysdep.host_name)
    strcpy (stpcpy (TitleBarString + strlen (TitleBarString), "@"),
            sysdep.host_name);

  if (!init_lisp_objects ())
    return 0;

  app.hinst = hinst;
  if (!register_wndclasses (hinst))
    return 0;

  InitPrivateControls (hinst);

  POINT point;
  SIZE size;
  int cmdshow = environ::load_geometry (passed_cmdshow, &point, &size);
  int restore_maximized = 0;
  if (sw_minimized_p (passed_cmdshow))
    {
      restore_maximized = sw_maximized_p (cmdshow);
      cmdshow = passed_cmdshow;
    }
  else if (sw_maximized_p (passed_cmdshow))
    cmdshow = passed_cmdshow;

  int show_normal = !sw_minimized_p (cmdshow) && !sw_maximized_p (cmdshow);

  xsymbol_value (Vsave_window_size) = boole (environ::save_window_size);
  xsymbol_value (Vsave_window_position) = boole (environ::save_window_position);
  xsymbol_value (Vrestore_window_size) = boole (environ::restore_window_size);
  xsymbol_value (Vrestore_window_position) = boole (environ::restore_window_position);

  ole_initialized = SUCCEEDED (OleInitialize (0));

  app.toplev = CreateWindow (Application::ToplevelClassName, TitleBarString,
                             WS_OVERLAPPEDWINDOW,
                             point.x, point.y, size.cx, size.cy,
                             HWND_DESKTOP, 0, hinst, 0);
  if (!app.toplev)
    return 0;

  mouse_state::install_hook ();
  init_listen_server ();

  sock::init_winsock (hinst);

  WINDOWPLACEMENT wp;
  wp.length = sizeof wp;
  GetWindowPlacement (app.toplev, &wp);

  if (point.x != CW_USEDEFAULT)
    {
      wp.rcNormalPosition.right -= wp.rcNormalPosition.left;
      wp.rcNormalPosition.bottom -= wp.rcNormalPosition.top;
      wp.rcNormalPosition.left = point.x;
      wp.rcNormalPosition.top = point.y;
      wp.rcNormalPosition.right += point.x;
      wp.rcNormalPosition.bottom += point.y;
    }
  if (restore_maximized)
    {
      wp.flags = WPF_RESTORETOMAXIMIZED;
      wp.showCmd = SW_SHOWMINIMIZED;
    }
  else
    {
      wp.flags = 0;
      wp.showCmd = cmdshow;
    }
  SetWindowPlacement (app.toplev, &wp);

  if (point.x != CW_USEDEFAULT && show_normal)
    SetWindowPos (app.toplev, 0, 0, 0,
                  wp.rcNormalPosition.right - wp.rcNormalPosition.left,
                  wp.rcNormalPosition.bottom - wp.rcNormalPosition.top,
                  SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOZORDER);

#ifndef WINDOWBLINDS_FIXED
  if (size.cx != CW_USEDEFAULT && show_normal)
    {
      RECT r;
      GetClientRect (app.toplev, &r);
      AdjustWindowRect (&r, WS_OVERLAPPEDWINDOW, 0);
      int aw = r.right - r.left, ah = r.bottom - r.top;
      GetWindowRect (app.toplev, &r);
      int ww = r.right - r.left, wh = r.bottom - r.top;
      ww = min (ww, aw);
      wh = min (wh, ah);
      SetWindowPos (app.toplev, 0, 0, 0, ww, wh,
                    SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOZORDER);
    }
#endif /* WINDOWBLINDS_FIXED */

  if (!start_quit_thread ())
    return 0;

  Fbegin_wait_cursor ();

  ShowWindow (app.toplev, cmdshow);
  if (sysdep.Win5p ())
    UpdateWindow (app.toplev);

  app.modeline_param.init (HFONT (SendMessage (app.hwnd_sw, WM_GETFONT, 0, 0)));

  if (!init_editor_objects ())
    return 0;

  try {Dde::initialize ();} catch (Dde::Exception &) {}

  return 1;
}

int PASCAL
WinMain (HINSTANCE hinst, HINSTANCE, LPSTR, int cmdshow)
{
  int ole_initialized = 0;
  if (init_app (hinst, cmdshow, ole_initialized))
    {
      xyzzy_instance xi (app.toplev);

      MSG msg;
      while (PeekMessage (&msg, 0, 0, 0, PM_REMOVE))
        {
          XyzzyTranslateMessage (&msg);
          DispatchMessage (&msg);
        }

      int terminate_normally = 0;
      try
        {
          int init_ok = 0;
          try
            {
              Ffuncall (Ssi_startup, Qnil);
              init_ok = 1;
            }
          catch (nonlocal_jump &)
            {
              print_condition (nonlocal_jump::data ());
            }

          if (init_ok)
            {
              start_listen_server ();
              Fset_cursor (xsymbol_value (Vcursor_shape));
              end_wait_cursor (1);
              app.kbdq.init_kbd_encoding ();

              while (1)
                {
                  try
                    {
                      main_loop ();
                      break;
                    }
                  catch (nonlocal_jump &)
                    {
                    }
                }
            }
          app.kbdq.gime.disable ();
          cleanup_lisp_objects ();
          terminate_normally = 1;
        }
      catch (Win32Exception &)
        {
        }

      if (!terminate_normally)
        {
          _set_se_translator (0);
          cleanup_exception ();
        }

      sock::term_winsock ();

    }

  mouse_state::remove_hook ();

  if (app.toplev)
    {
      end_listen_server ();
      DestroyWindow (app.toplev);
    }

  if (ole_initialized)
    OleUninitialize ();

#ifdef DEBUG
  {
    _CrtSetReportMode (_CRT_WARN, _CRTDBG_MODE_FILE);
    _CrtSetReportFile (_CRT_WARN, _CRTDBG_FILE_STDERR);
    for (Buffer *bp = Buffer::b_blist, *b_next; bp; bp = b_next)
      {
        b_next = bp->b_next;
        delete bp;
      }
    for (Window *wp = app.active_frame.windows, *w_next; wp; wp = w_next)
      {
        w_next = wp->w_next;
        delete wp;
      }

    g_frame.cleanup ();

    fflush (stdout);
    fflush (stderr);
    _CrtSetDbgFlag (_CrtSetDbgFlag (_CRTDBG_REPORT_FLAG) | _CRTDBG_LEAK_CHECK_DF);
  }
#endif
  return 0;
}
