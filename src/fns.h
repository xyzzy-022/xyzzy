// -*-C++-*-
#ifndef _fns_h_
# define _fns_h_

# include "xcolor.h"

/* eval.cc */
lisp eval (lisp, lex_env &);
lisp funcall_1 (lisp, lisp);
lisp funcall_2 (lisp, lisp, lisp);
lisp funcall_3 (lisp, lisp, lisp, lisp);
lisp funcall_4 (lisp, lisp, lisp, lisp, lisp);
lfunction_proc fast_funcall_p (lisp, int);
struct Buffer;
lisp symbol_value (lisp, const Buffer *);
void set_globally (lisp, lisp, Buffer *);
int symbol_value_as_integer (lisp, const Buffer *);
void check_stack_overflow ();
lisp call_hook_nargs (lisp, lisp, lisp);

/* pathname.cc */
void file_error (message_code, lisp);
void file_error (message_code);
void file_error (int, lisp);
void file_error (int);
int parse_namestring (Char *, const Char *, int, const Char *, int);
char *pathname2cstr (lisp, char *);
int special_file_p (const char *);
int sub_directory_p (char *, const char *);
lisp make_path (const char *s, int append_slash = 1);
void map_backsl_to_sl (Char *, int);
int match_suffixes (const char *, lisp);
int set_device_dir (const char *, int);
const char *get_device_dir (int);
int strict_get_file_data (const char *, WIN32_FIND_DATA &);
lisp make_file_info (const WIN32_FIND_DATA &);
char *root_path_name (char *, const char *);

/* lprint.cc */
void print_stack_trace (lisp, lisp);
void print_condition (struct nonlocal_data *);
void write_object (lisp, lisp, lisp);
const char *get_message_string (int);
int msgbox (int, lisp, lisp = 0);
void message (lisp, lisp = 0);
int yes_or_no_p (lisp, lisp = 0);
int msgbox (int, message_code, lisp = 0);
void message (message_code, lisp = 0);
int yes_or_no_p (message_code, lisp = 0);
void warn_msgbox (lisp, lisp = 0);
void warn (lisp, lisp = 0);
void warn_msgbox (message_code, lisp = 0);
void warn (message_code, lisp = 0);
void format_message (message_code, ...);
int format_yes_or_no_p (message_code, ...);
char *print_key_sequence (char *, char *, Char);
void ding (int);
int get_glyph_width (Char, const struct glyph_width &);

/* environ.cc */
void init_environ ();

/* lread.cc */
void init_readtable ();

/* keymap.cc */
lisp lookup_keymap (Char, lisp *, int);
Char *lookup_command_keyseq (lisp, lisp, const lisp *, int, Char *, Char *, Char *);
int find_in_current_keymaps (Char);

/* data.cc */
lisp interactive_string (lisp);
void destruct_string (lisp);
void destruct_regexp (lisp);
#ifdef DEBUG_GC
void output_funcall_mark (FILE *);
#endif

/* toplev.cc */
LRESULT CALLBACK toplevel_wndproc (HWND, UINT, WPARAM, LPARAM);
LRESULT CALLBACK frame_wndproc (HWND, UINT, WPARAM, LPARAM);
LRESULT CALLBACK client_wndproc (HWND, UINT, WPARAM, LPARAM);
LRESULT CALLBACK modeline_wndproc (HWND, UINT, WPARAM, LPARAM);
void main_loop ();
int start_quit_thread ();
int wait_process_terminate (HANDLE);
lisp execute_string (lisp);
int end_wait_cursor (int);
void set_ime_caret ();
void recalc_toplevel ();
void set_caret_blink_time ();
void restore_caret_blink_time ();
void toplev_gc_mark (void (*)(lisp));
int toplev_accept_mouse_move_p ();

/* minibuf.cc */
lisp load_default (lisp, int);
lisp load_history (lisp, int);
lisp load_history (lisp, int, lisp);
lisp load_title (lisp, int);
lisp read_minibuffer (const Char *, long, lisp, lisp, lisp, lisp, int, int, int, lisp, int);
lisp complete_read (const Char *, long, lisp, lisp, lisp, lisp, int, int);
lisp read_filename (const Char *, long, lisp, lisp, lisp, lisp);
lisp minibuffer_read_integer (const Char *, long);

/* chname.cc */
Char standard_char_name2Char (const Char *, int);
Char function_char_name2Char (const Char *, int);
Char char_bit_name2Char (const Char *, int, int &);
const char *function_Char2name (Char);
const char *standard_Char2name (Char);

/* process.cc */
void read_process_output (WPARAM, LPARAM);
void wait_process_terminate (WPARAM, LPARAM);
int buffer_has_process (const Buffer *);
int query_kill_subprocesses ();
void process_gc_mark (void (*)(lisp));

/* menu.cc */
int init_menu_flags (lisp);
void init_menu_popup (WPARAM, LPARAM);
lisp lookup_menu_command (int);
lisp track_popup_menu (lisp, lisp, const POINT *);

/* dialogs.cc */
void center_window (HWND);
void set_window_icon (HWND);
void init_list_column (HWND, int, const int *, const int *, int, const char *, const char *);
void save_list_column_width (HWND, int, const char *, const char *);
int lv_find_selected_item (HWND);
int lv_find_focused_item (HWND);

/* fileio.cc */
int same_file_p (const char *, const char *);
int make_temp_file_name (char *, const char * = 0, const char * = 0, HANDLE = 0, int = 0);
void do_auto_save (int, int);

/* Buffer.cc */
void change_local_colors (const XCOLORREF *, int, int);
void update_buffer_bar ();

/* init.cc */
void report_out_of_memory ();

/* insdel.cc */
struct CLIPBOARDTEXT
{
  UINT fmt;
  HGLOBAL hgl;
};

int make_clipboard_text (CLIPBOARDTEXT &, lisp, int);
int make_string_from_clipboard_text (lisp, const void *, UINT, int);

/* popup.cc */
void erase_popup (int, int);

/* disp.cc */
void reload_caret_colors ();

/* listen.cc */
void start_listen_server ();
void init_listen_server ();
void end_listen_server ();
int read_listen_server (WPARAM, LPARAM);
extern UINT wm_private_xyzzysrv;

/* ces.cc */
void init_char_encoding ();
lisp find_char_encoding (lisp);
lisp make_char_encoding_constructor (lisp);
lisp symbol_value_char_encoding (lisp);
int to_vender_code (lisp);
int to_lang (lisp);
lisp from_lang (int);

/* Window.cc */
void ForceSetForegroundWindow (HWND);

/* usertool.cc */
lisp get_tooltip_text (lisp);

/* stdctl.cc */
void stdctl_hook_init (HINSTANCE);
int stdctl_operation (int);

#endif
