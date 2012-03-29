#ifndef _kbd_h_
# define _kbd_h_

#include "reconv.h"
#include "gime.h"

class kbd_macro_context;

class kbd_queue
{
  enum {QUEUE_MAX = 1024};
  lChar cc[QUEUE_MAX];
  int head;
  int tail;
  lChar pending;
  enum {KBDMACRO_MAX = 2048};
  Char saved[KBDMACRO_MAX];
  int nsaved;
  Char macro_char ();
  kbd_macro_context *kbd_macro;
  int last_ime_status;
  enum {ACTIVATE = 1, DEACTIVATE = 2, LAST_ACTIVE = 4};
  int delayed_activate;
  int in_hook;
  int putc_pending;
  typedef HKL (WINAPI *GETKEYBOARDLAYOUT)(DWORD);
  GETKEYBOARDLAYOUT GetKeyboardLayout;

  void run_hook (lisp, int);
  void run_activate_hook ();
  friend kbd_macro_context;
  int putraw (lChar);
public:
  enum input_mode
    {
      im_normal,
      im_save,
      im_disable = 0x80
    };

  GlobalIME gime;

private:
  input_mode current_mode;
  int in_main_loop;
  int last_command_key_index;
  int command_key_keeped;
  int st_start_time;
  int st_timeout;
  RECONVERTSTRING *reconv;
  LANGID input_langid;
  int unicode_kbd;
  int ime_prop;
  int gc_timer;

  class sleep_timer
    {
      kbd_queue &s_kbdq;
      int s_save_timeout;
      int s_save_start_time;
    public:
      sleep_timer (kbd_queue &);
      ~sleep_timer ();
      void wait (int, int);
    };
  friend sleep_timer;

public:
  class disable_kbd
    {
      kbd_queue &d_kbdq;
      int d_disable;
    public:
      disable_kbd (kbd_queue &q) : d_kbdq (q)
        {
          d_disable = q.current_mode & kbd_queue::im_disable;
          (int &)d_kbdq.current_mode |= kbd_queue::im_disable;
        }
      ~disable_kbd ()
        {
          if (!d_disable)
            (int &)d_kbdq.current_mode &= ~kbd_queue::im_disable;
        }
    };
  friend disable_kbd;

  input_mode mode () const;
  int save_p () const;
  int macro_is_running () const;
  int disablep () const;
  int idlep () const;
  void start_macro ();
  lisp end_macro ();
  void stop_macro ();
  void end_last_command_key ();
  void save_last_command_key (int &, int &) const;
  void restore_last_command_key (int, int);
  void set_next_command_key ();
  void keep_next_command_key ();
  kbd_queue ();
  ~kbd_queue ();
  int putc (lChar);
  int putw (int);
  int puts (const char *, int);
  lChar peek (int);
  lChar fetch (int, int);
  int listen ();
  void clear ();
  void sit_for (DWORD);
  void sleep_for (DWORD);
  void push_back (lChar);
  int wait_event (HANDLE, int = 0);
  void process_events ();
  int track_popup_menu (HMENU, int, const POINT &);
  void close_ime ();
  void restore_ime ();
  void activate (int);
  enum {IME_MODE_OFF = 1, IME_MODE_ON, IME_MODE_TOGGLE};
  int toggle_ime (int, int = 1);
  int lookup_kbd_macro (lisp) const;
  int copy_queue (Char *, int) const;
  void paste () const;
  int reconvert (RECONVERTSTRING *, int);
  int documentfeed (RECONVERTSTRING *, int);
  void set_kbd_langid (LANGID langid) {input_langid = langid;}
  LANGID kbd_langid () const {return input_langid;}
  void init_kbd_encoding () {init_kbd_encoding (get_kbd_langid ());}
  void init_kbd_encoding (LANGID);
  int kbd_mblead_p (int) const;
  static const class FontObject &kbd_encoding_font ();
  LANGID get_kbd_langid () const
    {return LANGID (get_kbd_layout ());}
  HKL get_kbd_layout () const;
  int ime_property () const {return ime_prop;}
  int unicode_kbd_p () const {return unicode_kbd;}
  void gc_timer_expired () {gc_timer = 1;}
};

class save_command_key_index
{
  int v1, v2;
  kbd_queue &q;
public:
  save_command_key_index (kbd_queue &);
  ~save_command_key_index ();
};

inline void
kbd_queue::push_back (lChar c)
{
  if (pending == lChar_EOF)
    pending = c;
}

inline kbd_queue::input_mode
kbd_queue::mode () const
{
  return current_mode;
}

inline int
kbd_queue::save_p () const
{
  return (current_mode & ~im_disable) == im_save;
}

inline int
kbd_queue::macro_is_running () const
{
  return int (kbd_macro);
}

inline int
kbd_queue::disablep () const
{
  return current_mode & im_disable;
}

inline int
kbd_queue::idlep () const
{
  return (mode () == kbd_queue::im_normal
          && !macro_is_running ()
          && in_main_loop);
}

inline void
kbd_queue::set_next_command_key ()
{
  command_key_keeped = 0;
}

inline void
kbd_queue::keep_next_command_key ()
{
  command_key_keeped = 1;
}

inline void
kbd_queue::end_last_command_key ()
{
  if (!command_key_keeped)
    last_command_key_index = nsaved;
}

inline void
kbd_queue::save_last_command_key (int &v1, int &v2) const
{
  v1 = last_command_key_index;
  v2 = command_key_keeped;
}

inline void
kbd_queue::restore_last_command_key (int v1, int v2)
{
  last_command_key_index = v1;
  command_key_keeped = v2;
}

inline
save_command_key_index::save_command_key_index (kbd_queue &q_)
     : q (q_)
{
  q.save_last_command_key (v1, v2);
}

inline
save_command_key_index::~save_command_key_index ()
{
  q.restore_last_command_key (v1, v2);
}

class kbd_macro_context
{
  kbd_macro_context *last;
  kbd_queue &kbdq;
  lisp string;
  int index;
public:
  kbd_macro_context (kbd_queue &, lisp);
  ~kbd_macro_context ();
  int running () const;
  friend kbd_queue;
};

inline
kbd_macro_context::kbd_macro_context (kbd_queue &q, lisp s)
     : kbdq (q), string (s), index (0)
{
  if (!xstring_length (string))
    return;
  last = kbdq.kbd_macro;
  kbdq.kbd_macro = this;
}

inline
kbd_macro_context::~kbd_macro_context ()
{
  if (kbdq.kbd_macro == this)
    kbdq.kbd_macro = last;
}

inline int
kbd_macro_context::running () const
{
  return kbdq.kbd_macro == this;
}

class key_sequence
{
  enum {MAX_KEYSEQ = 32};
  Char k_seq[MAX_KEYSEQ];
  int k_used;
  int k_last_used;

  void store (Char);
  void notice (int, int);
public:
  key_sequence ();
  void push (Char, int);
  void again (int);
  void done (Char, int);
};

class ime_comp_queue
{
public:
  enum {MAX_QUEUE = 16};
  struct pair
    {
      Char *comp;
      int compl;
      Char *read;
      int readl;
    };
private:
  pair qbuf[MAX_QUEUE];
  int qindex;
public:
  ime_comp_queue () : qindex (0)
    {
      bzero (qbuf, sizeof qbuf);
    }
  ~ime_comp_queue ()
    {
      for (int i = 0; i < MAX_QUEUE; i++)
        xfree (qbuf[i].comp);
    }
  void push (const char *, int, const char *, int);
  void push (const ucs2_t *, int, const ucs2_t *, int, const Char *);
  const pair *fetch () const;
  const pair *pop ();
};

void check_kbd_enable ();
BOOL XyzzyTranslateMessage (const MSG *);

lChar decode_syskeys (WPARAM, LPARAM);
lChar decode_keys (WPARAM, LPARAM);
lChar lookup_translate_table (lChar);
lChar decode_syschars (WPARAM);
lChar decode_chars (WPARAM);
int char_mouse_move_p (Char);

#endif
