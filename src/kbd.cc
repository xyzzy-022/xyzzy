#include "ed.h"
#include <imm.h>
#include "filer.h"
#include "safe_ptr.h"
#include "encoding.h"
#include "environ.h"

kbd_queue::kbd_queue ()
     : head (0), tail (0), pending (lChar_EOF), last_ime_status (-1),
       current_mode (im_normal), kbd_macro (0), delayed_activate (0), in_hook (0),
       last_command_key_index (0), command_key_keeped (0), st_timeout (-1),
       reconv (0), putc_pending (-1), ime_prop (0), unicode_kbd (0), gc_timer (0)
{
  GetKeyboardLayout =
    (GETKEYBOARDLAYOUT)GetProcAddress (GetModuleHandle ("USER32"),
                                       "GetKeyboardLayout");

  for (int i = 0; i < QUEUE_MAX; i++)
    cc[i] = lChar_EOF;
}

kbd_queue::~kbd_queue ()
{
  xfree (reconv);
}

void
kbd_queue::process_events ()
{
  disable_kbd dkbd (*this);
  MSG msg;
  while (PeekMessage (&msg, 0, 0, 0, PM_REMOVE))
    {
      if (msg.message == WM_QUIT)
        {
          PostQuitMessage (0);
          break;
        }
      XyzzyTranslateMessage (&msg);
      DispatchMessage (&msg);
    }
}

int
kbd_queue::putraw (lChar c)
{
  if ((tail + 1) % QUEUE_MAX == head)
    {
      DBG_PRINT (("KBD BUFFER OVER FLOW\n"));
      return 0;
    }
  cc[tail] = c;
  tail = (tail + 1) % QUEUE_MAX;
  return 1;
}

int
kbd_queue::putc (lChar c)
{
  if (c >= 256)
    return putraw (c);

  if (putc_pending >= 0)
    {
      char b[2];
      b[0] = char (putc_pending);
      b[1] = char (c);
      putc_pending = -1;
      return puts (b, 2);
    }

  if (c < 128)
    return putraw (c);

  if (kbd_mblead_p (c))
    {
      putc_pending = c;
      return 1;
    }

  char cc = char (c);
  return puts (&cc, 1);
}

int
kbd_queue::putw (int w)
{
  if (w < 256)
    return putc (w);
  char b[2];
  b[0] = w >> 8;
  b[1] = w;
  return puts (b, 2);
}

int
kbd_queue::puts (const char *string, int l)
{
  xinput_strstream in (string, l);
  encoding_input_stream_helper is (xsymbol_value (Vkbd_encoding), in);
  int c;
  while ((c = is->get ()) != xstream::eof)
    if (!putraw (c))
      return 0;
  return 1;
}

lChar
kbd_queue::peek (int req_mouse_move)
{
  lChar c;
  if (pending != lChar_EOF)
    {
      c = pending;
      pending = lChar_EOF;
    }
  else
    {
      if (kbd_macro)
        c = macro_char ();
      else
        {
          if (head == tail)
            return lChar_EOF;
          c = cc[head];
          head = (head + 1) % QUEUE_MAX;
          if (!req_mouse_move && char_mouse_move_p (Char (c)))
            return lChar_EOF;
          if (save_p () && !(c & LCHAR_MENU) && !char_mouse_move_p (Char (c)))
            {
              if (nsaved == KBDMACRO_MAX)
                stop_macro ();
              else
                saved[nsaved++] = Char (c);
            }
        }
    }
  return c;
}

void
kbd_queue::clear ()
{
  process_events ();
  head = tail;
  pending = lChar_EOF;
  stop_macro ();
}

lChar
kbd_queue::fetch (int in_main, int req_mouse_move)
{
  lChar c;
  in_main_loop = in_main;
  gc_timer = 0;

  if (delayed_activate && idlep ())
    run_activate_hook ();

  while (1)
    {
      c = peek (req_mouse_move);
      if (c != lChar_EOF)
        break;
      MSG msg;
      if (!GetMessage (&msg, 0, 0, 0))
        {
          in_main_loop = 0;
          PostQuitMessage (0);
          return lChar_EOF;
        }
      XyzzyTranslateMessage (&msg);
      DispatchMessage (&msg);
      in_main_loop = in_main;
      if (in_main && gc_timer)
        {
          gc_timer = 0;
          if (ldataP::ld_nwasted)
            gc (-1);
        }
    }
  in_main_loop = 0;
  if (!(c & (LCHAR_MOUSE | LCHAR_MENU)))
    process_events ();
  xsymbol_value (Vquit_flag) = Qnil;
  return c;
}

kbd_queue::sleep_timer::sleep_timer (kbd_queue &q)
     : s_kbdq (q)
{
  s_save_start_time = s_kbdq.st_start_time;
  s_save_timeout = s_kbdq.st_timeout;
}

kbd_queue::sleep_timer::~sleep_timer ()
{
  KillTimer (app.toplev, TID_SLEEP);
  app.sleep_timer_exhausted = 0;
  s_kbdq.st_start_time = s_save_start_time;
  s_kbdq.st_timeout = s_save_timeout;
  if (s_kbdq.st_timeout >= 0)
    {
      DWORD req = s_kbdq.st_start_time + s_kbdq.st_timeout;
      DWORD curtime = GetTickCount ();
      if (req >= curtime)
        app.sleep_timer_exhausted = 1;
      else
        SetTimer (app.toplev, TID_SLEEP, req - curtime, 0);
    }
}

void
kbd_queue::sleep_timer::wait (int timeout, int kbdp)
{
  disable_kbd dkbd (s_kbdq);
  s_kbdq.st_start_time = GetTickCount ();
  s_kbdq.st_timeout = timeout;
  app.sleep_timer_exhausted = 0;
  SetTimer (app.toplev, TID_SLEEP, timeout, 0);
  while ((!kbdp || s_kbdq.head == s_kbdq.tail) && !app.sleep_timer_exhausted)
    {
      QUIT;
      MSG msg;
      if (!GetMessage (&msg, 0, 0, 0))
        {
          PostQuitMessage (0);
          break;
        }
      XyzzyTranslateMessage (&msg);
      DispatchMessage (&msg);
    }
}

void
kbd_queue::sit_for (DWORD timeout)
{
  if (kbd_macro || pending != lChar_EOF || head != tail)
    return;
  sleep_timer st (*this);
  st.wait (timeout, 1);
}

void
kbd_queue::sleep_for (DWORD timeout)
{
  sleep_timer st (*this);
  st.wait (timeout, 0);
}

int
kbd_queue::listen ()
{
  if (kbd_macro || pending != lChar_EOF || head != tail)
    return 1;
  process_events ();
  xsymbol_value (Vquit_flag) = Qnil;
  return head != tail;
}

int
kbd_queue::wait_event (HANDLE h, int enable_quit)
{
  disable_kbd dkbd (*this);
  while (1)
    {
      if (MsgWaitForMultipleObjects (1, &h, 0, INFINITE, QS_ALLINPUT) == WAIT_OBJECT_0)
        return 1;

      MSG msg;
      while (PeekMessage (&msg, 0, 0, 0, PM_REMOVE))
        {
          if (msg.message == WM_QUIT)
            {
              PostQuitMessage (0);
              return 0;
            }
          XyzzyTranslateMessage (&msg);
          DispatchMessage (&msg);
          if (enable_quit && xsymbol_value (Vquit_flag) != Qnil)
            return 0;
        }
    }
}

void
kbd_queue::start_macro ()
{
  assert (!save_p ());
  current_mode = input_mode (disablep () | im_save);
  Window::modify_all_mode_line ();
  nsaved = 0;
  last_command_key_index = 0;
  command_key_keeped = 0;
}

lisp
kbd_queue::end_macro ()
{
  assert (save_p ());
  stop_macro ();
  return make_string (saved, min (nsaved, last_command_key_index));
}

void
kbd_queue::stop_macro ()
{
  if (save_p ())
    Window::modify_all_mode_line ();
  current_mode = input_mode (disablep () | im_normal);
}

Char
kbd_queue::macro_char ()
{
  assert (kbd_macro);
  assert (kbd_macro->index < xstring_length (kbd_macro->string));
  Char c = xstring_contents (kbd_macro->string) [kbd_macro->index++];
  if (kbd_macro->index >= xstring_length (kbd_macro->string))
    kbd_macro = kbd_macro->last;
  return c;
}

int
kbd_queue::track_popup_menu (HMENU hmenu, int button, const POINT &p)
{
  int result = 0;
  in_main_loop = 1;
  if (idlep ())
    {
      result = TrackPopupMenu (hmenu, TPM_RETURNCMD | TPM_LEFTALIGN | button,
                               p.x, p.y, 0, app.toplev, 0);
      if (result == -1)
        result = 0;
    }
  in_main_loop = 0;
  return result;
}

void
kbd_queue::close_ime ()
{
  if (pending == lChar_EOF && head == tail
      && !macro_is_running ())
    {
      if (xsymbol_value (Vime_control) == Qnil)
        last_ime_status = -1;
      else if (last_ime_status == -1)
        {
          HIMC imc = gime.ImmGetContext (app.toplev);
          if (imc)
            {
              last_ime_status = gime.ImmGetOpenStatus (imc);
              if (last_ime_status)
                gime.ImmSetOpenStatus (imc, 0);
              gime.ImmReleaseContext (app.toplev, imc);
            }
        }
    }
}

void
kbd_queue::restore_ime ()
{
  if (pending == lChar_EOF && head == tail
      && !macro_is_running ()
      && xsymbol_value (Vime_control) != Qnil
      && last_ime_status != -1)
    {
      HIMC imc = gime.ImmGetContext (app.toplev);
      if (imc)
        {
          gime.ImmSetOpenStatus (imc, last_ime_status);
          gime.ImmReleaseContext (app.toplev, imc);
          last_ime_status = -1;
        }
    }
}

int
kbd_queue::toggle_ime (int new_stat, int update_last_ime_status)
{
  HIMC imc = gime.ImmGetContext (app.toplev);
  if (!imc)
    return 0;
  int old_stat = gime.ImmGetOpenStatus (imc) ? IME_MODE_ON : IME_MODE_OFF;
  if (new_stat == IME_MODE_TOGGLE || new_stat != old_stat)
    gime.ImmSetOpenStatus (imc, old_stat != IME_MODE_ON);
  gime.ImmReleaseContext (app.toplev, imc);
  if (update_last_ime_status)
    last_ime_status = -1;
  return old_stat;
}

void
kbd_queue::run_hook (lisp hook, int f)
{
  if ((delayed_activate & f) == f)
    {
      delayed_activate &= ~f;
      in_hook = 1;
      selected_buffer ()->safe_run_hook (hook, 1);
      in_hook = 0;
    }
}

void
kbd_queue::run_activate_hook ()
{
  if (in_hook)
    return;
  run_hook (Vactivate_hook, ACTIVATE | LAST_ACTIVE);
  run_hook (Vdeactivate_hook, DEACTIVATE | LAST_ACTIVE);
  run_hook (Vactivate_hook, ACTIVATE);
  run_hook (Vdeactivate_hook, DEACTIVATE);
  refresh_screen (0);
}

void
kbd_queue::activate (int active)
{
  if (active)
    delayed_activate |= ACTIVATE | LAST_ACTIVE;
  else
    {
      delayed_activate |= DEACTIVATE;
      delayed_activate &= ~LAST_ACTIVE;
    }
  if (idlep ())
    run_activate_hook ();
}

int
kbd_queue::lookup_kbd_macro (lisp string) const
{
  for (kbd_macro_context *p = kbd_macro; p; p = p->last)
    if (p->string == string)
      return 1;
  return 0;
}

void
check_kbd_enable ()
{
  if (IsWindowEnabled (app.toplev))
    return;
  FEprogram_error (Ekbd_input_is_disabled);
}

#define VK_HANKAKU 0xf3
#define VK_ZENKAKU 0xf4

static int
exkey_index (UINT c, int syskey)
{
  static const UINT exkey[] =
    {
      VK_BACK,
      VK_TAB,
      VK_RETURN,
      VK_ESCAPE,
      VK_ZENKAKU,
      VK_SPACE,
    };
  if (syskey && c == VK_SPACE && xsymbol_value (Venable_meta_key) == Qnil)
    return -1;

  // for DEC LK411-AJ
  if ((c == VK_ZENKAKU || c == VK_HANKAKU)
      && LOWORD (MapVirtualKey (c, 2)))
    return -1;

  if (c == VK_HANKAKU)
    c = VK_ZENKAKU;

  for (int i = 0; i < numberof (exkey); i++)
    if (c == exkey[i])
      return i;
  return -1;
}

static int
to_ascii_char (int vk, int sc)
{
  if (vk < VK_SPACE)
    return -1;
  if (GetKeyState (VK_CONTROL) >= 0)
    return -1;
  static BYTE state[256];
  state[VK_SHIFT] = GetKeyState (VK_SHIFT) < 0 ? 0x80 : 0;
  state[VK_CONTROL] = 0;
  WORD key[2] = {0};
  if (ToAscii (vk, sc, state, key, 0) != 1)
    return -1;
  if (*key >= 128)
    return -1;
  return *key;
}

static int
maybe_ctrl_char_p (int c)
{
  return c == 0x3f || c == 0x40 || (c >= 0x5b && c <= 0x5f);
}

static int
translate_unicode (HWND hwnd, WPARAM wparam, LPARAM lparam)
{
  BYTE b[256];
  GetKeyboardState (b);
  wchar_t w[2];
  switch (ToUnicode (wparam, lparam, b, w, 2, 0))
    {
    default:
      return 0;

    case 1:
    case -1:
      PostMessage (hwnd, WM_PRIVATE_WCHAR, w[0], lparam);
      return 1;

    case 2:
      PostMessage (hwnd, WM_PRIVATE_WCHAR, w[0], lparam);
      PostMessage (hwnd, WM_PRIVATE_WCHAR, w[1], lparam);
      return 1;
    }
}

BOOL
XyzzyTranslateMessage (const MSG *msg)
{
  if (msg->hwnd == app.toplev || Filer::filer_ancestor_p (msg->hwnd))
    {
      switch (msg->message)
        {
        case WM_SYSKEYDOWN:
          if (msg->wParam >= VK_NUMPAD0 && msg->wParam <= VK_NUMPAD9)
            {
              PostMessage (msg->hwnd, WM_SYSCHAR,
                           msg->wParam - VK_NUMPAD0 + '0', msg->lParam);
              return 1;
            }
          /* fall thru... */
        case WM_KEYDOWN:
          if (msg->wParam == VK_PROCESSKEY
              && xsymbol_value (Vime_does_not_process_control_backslach) != Qnil)
            {
              int key = to_ascii_char (app.kbdq.gime.ImmGetVirtualKey (msg->hwnd), msg->lParam);
              if (key == '\\' || key == '_')
                {
                  PostMessage (msg->hwnd, msg->message == WM_KEYDOWN ? WM_CHAR : WM_SYSCHAR,
                               key - '@', msg->lParam);
                  return 1;
                }
            }
          else
            {
              if (app.kbdq.unicode_kbd_p () && msg->message == WM_KEYDOWN
                  && exkey_index (msg->wParam, 0) < 0
                  && translate_unicode (msg->hwnd, msg->wParam, msg->lParam))
                return 1;

              int key = to_ascii_char (msg->wParam, msg->lParam);
              if (maybe_ctrl_char_p (key))
                {
                  PostMessage (msg->hwnd, msg->message == WM_KEYDOWN ? WM_CHAR : WM_SYSCHAR,
                               key >= 0x40 ? key - 0x40 : key + 0x40, msg->lParam);
                  return 1;
                }
              else if (key != -1 && !alpha_char_p (key))
                return 0;
            }
          /* fall thru... */
        case WM_KEYUP:
        case WM_SYSKEYUP:
          if (exkey_index (msg->wParam, (msg->message == WM_SYSKEYDOWN
                                         || msg->message == WM_SYSKEYUP)) >= 0)
            return 0;
          break;
        }
    }
  return app.kbdq.gime.TranslateMessage (msg);
}

static lChar
pre_decode_vkeys (UINT vkey, int syskey)
{
  int i = exkey_index (vkey, syskey);
  if (i < 0)
    return lChar_EOF;

  lisp vec = xsymbol_value (Vextended_key_translate_table);
  if (!general_vector_p (vec))
    return lChar_EOF;

  i <<= 3;
  if (GetKeyState (VK_SHIFT) < 0)
    i |= 1;
  if (GetKeyState (VK_CONTROL) < 0)
    i |= 2;
  if (GetKeyState (VK_MENU) < 0)
    i |= 4;

  if (i >= xvector_length (vec))
    return lChar_EOF;

  lisp cc = xvector_contents (vec)[i];
  if (!charp (cc))
    return lChar_EOF;

  return xchar_code (cc);
}

lChar
lookup_translate_table (lChar cc)
{
  if (cc >= 128)
    return cc;
  lisp vec = xsymbol_value (Vkbd_translate_table);
  if (!general_vector_p (vec))
    return cc;
  if (xvector_length (vec) != 128)
    return cc;
  lisp lcc = xvector_contents (vec)[cc];
  return charp (lcc) ? xchar_code (lcc) : cc;
}

static lChar
decode_vkeys (UINT vkey)
{
  if (vkey == VK_APPS)
    return CCF_APPS;
  if (vkey == VK_PAUSE)
    return CCF_PAUSE;
  if (vkey == VK_SCROLL)
    return CCF_SCROLL;
  if (vkey >= VK_SELECT && vkey <= VK_SNAPSHOT)
    return lChar_EOF;
  if (vkey >= VK_PRIOR && vkey <= VK_HELP)
    return CCF_PRIOR + vkey - VK_PRIOR;
  if (vkey >= VK_F1 && vkey <= VK_F1 + CCF_Fn_MAX - CCF_F1)
    return CCF_F1 + vkey - VK_F1;
  return lChar_EOF;
}

static lChar
decode_ctlchars (UINT vk, UINT sc)
{
  int key = to_ascii_char (vk, sc);
  if (key < 0 || maybe_ctrl_char_p (vk))
    return lChar_EOF;
  if (!pseudo_char2ctl_table[key])
    return lChar_EOF;
  return CCF_CHAR_MIN + pseudo_char2ctl_table[key];
}

lChar
decode_syschars (WPARAM wparam)
{
  lChar cc = lookup_translate_table (wparam);
  if (GetKeyState (VK_MENU) < 0)
    {
      if (ascii_char_p (cc))
        cc |= CC_META_BIT;
      else if (function_char_p (Char (cc)))
        cc = function_to_meta_function (Char (cc));
    }
  return cc;
}

lChar
decode_chars (WPARAM wparam)
{
  return decode_syschars (wparam);
}

lChar
decode_syskeys (WPARAM wparam, LPARAM lparam)
{
  if (wparam == VK_TAB)
    return lChar_EOF;

  lChar c = pre_decode_vkeys (wparam, 1);
  if (c != lChar_EOF)
    return c;

  c = decode_vkeys (wparam);
  if (c != lChar_EOF)
    {
      if (GetKeyState (VK_SHIFT) < 0)
        c |= CCF_SHIFT_BIT;
      if (GetKeyState (VK_CONTROL) < 0)
        c |= CCF_CTRL_BIT;
      if (GetKeyState (VK_MENU) < 0)
        c = function_to_meta_function (Char (c));
    }
  else
    {
      c = decode_ctlchars (wparam, lparam);
      if (c != lChar_EOF)
        {
          c = lookup_translate_table (c);
          if (GetKeyState (VK_MENU) < 0 && function_char_p (Char (c)))
            c = function_to_meta_function (Char (c));
        }
    }
  return c;
}

lChar
decode_keys (WPARAM wparam, LPARAM lparam)
{
  lChar c = pre_decode_vkeys (wparam, 0);
  if (c != lChar_EOF)
    return c;

  c = decode_vkeys (wparam);
  if (c != lChar_EOF)
    {
      if (GetKeyState (VK_SHIFT) < 0)
        c |= CCF_SHIFT_BIT;
      if (GetKeyState (VK_CONTROL) < 0)
        c |= CCF_CTRL_BIT;
      if (GetKeyState (VK_MENU) < 0)  // ???
        c = function_to_meta_function (Char (c));
    }
  else
    {
      c = decode_ctlchars (wparam, lparam);
      if (c != lChar_EOF)
        {
          c = lookup_translate_table (c);
          if (GetKeyState (VK_MENU) < 0 && function_char_p (Char (c)))
            c = function_to_meta_function (Char (c));
        }
      else
        {
          // handle Meta-Ctrl-XXX
          if (!(lparam & (1L << 29)))
            return lChar_EOF;

          BYTE state[256];
          WORD key[2];
          GetKeyboardState (state);
          if (ToAscii (wparam, lparam, state, key, 0) > 0)
            return lChar_EOF;

          if (wparam >= '@' && wparam < '@' + 0x20)
            c = wparam - '@';
          else
            return lChar_EOF;
          c = lookup_translate_table (c);
          if (GetKeyState (VK_MENU) < 0)
            {
              if (ascii_char_p (c))
                c |= CC_META_BIT;
              else if (function_char_p (Char (c)))
                c = function_to_meta_function (Char (c));
            }
        }
    }
  return c;
}

int
kbd_queue::copy_queue (Char *b0, int size) const
{
  Char *b = b0 + size;
  for (int i = (head + QUEUE_MAX - 1) % QUEUE_MAX; i != tail && b > b0;
       i = (i + QUEUE_MAX - 1) % QUEUE_MAX)
    if (cc[i] != lChar_EOF && !(cc[i] & LCHAR_MENU))
      *--b = Char (cc[i]);
  int l = b0 + size - b;
  if (pending != lChar_EOF && l)
    l--;
  bcopy (b, b0, l);
  return l;
}

void
kbd_queue::paste () const
{
  if (idlep ())
    selected_buffer ()->safe_run_hook (Vsi_paste_hook, 1);
}

int
kbd_queue::reconvert (RECONVERTSTRING *rsbuf, int unicode_p)
{
  if (rsbuf)
    {
      if (!reconv || rsbuf->dwSize < reconv->dwSize)
        {
          xfree (reconv);
          reconv = 0;
          return 0;
        }
      memcpy (rsbuf, reconv, reconv->dwSize);
      xfree (reconv);
      reconv = 0;
      return rsbuf->dwSize;
    }
  else
    {
      if (reconv)
        {
          xfree (reconv);
          reconv = 0;
        }

      if (!idlep ())
        return 0;

      lisp hook = xsymbol_value (Vime_reconvert_helper);
      if (hook == Qunbound || hook == Qnil)
        return 0;

      try
        {
          suppress_gc sgc;
          lisp r = Ffuncall (hook, Qnil);
          int n = multiple_value::count ();
          multiple_value::clear ();
          if (n != 4)
            return 0;

          Buffer *bp = selected_buffer ();
          point_t p1 = bp->coerce_to_restricted_point (r);
          point_t p2 = bp->coerce_to_restricted_point (multiple_value::value (1));
          point_t r1 = bp->coerce_to_restricted_point (multiple_value::value (2));
          point_t r2 = bp->coerce_to_restricted_point (multiple_value::value (3));
          if (p1 == p2 || r1 == r2)
            return 0;
          if (p1 > p2)
            swap (p1, p2);
          if (r1 > r2)
            swap (r1, r2);
          if (r1 < p1 || r2 > p2)
            return 0;

          int size = p2 - p1;
          safe_ptr <Char> data0 (new Char [size + 2]);
          Char *const data = data0 + 1;
          bp->substring (p1, size, data);
          char *b0 = (char *)(Char *)data0;
          char *b1 = w2s (b0, data, r1 - p1);
          char *b2 = w2s (b1, data + (r1 - p1), r2 - r1);
          char *b3 = w2s (b2, data + (r2 - p1), p2 - r2);
          if (!unicode_p)
            {
              reconv = (RECONVERTSTRING *)xmalloc (sizeof *reconv + (b3 - b0) + 1);
              reconv->dwSize = sizeof *reconv + (b3 - b0) + 1;
              reconv->dwVersion = 0;
              reconv->dwStrLen = b3 - b0;
              reconv->dwStrOffset = sizeof *reconv;
              reconv->dwCompStrLen = b2 - b1;
              reconv->dwCompStrOffset = b1 - b0;
              reconv->dwTargetStrLen = b2 - b1;
              reconv->dwTargetStrOffset = b1 - b0;
              strcpy ((char *)(reconv + 1), b0);
            }
          else
            {
              int l1 = MultiByteToWideChar (CP_ACP, 0, b0, b1 - b0, 0, 0);
              int l2 = MultiByteToWideChar (CP_ACP, 0, b1, b2 - b1, 0, 0);
              int l3 = MultiByteToWideChar (CP_ACP, 0, b2, b3 - b2, 0, 0);
              int l = l1 + l2 + l3;
              size = sizeof *reconv + (l + 1) * sizeof (wchar_t);
              reconv = (RECONVERTSTRING *)xmalloc (size);
              reconv->dwSize = size;
              reconv->dwVersion = 0;
              reconv->dwStrLen = l;
              reconv->dwStrOffset = sizeof *reconv;
              reconv->dwCompStrLen = l2;
              reconv->dwCompStrOffset = l1 * sizeof (wchar_t);
              reconv->dwTargetStrLen = l2;
              reconv->dwTargetStrOffset = l1 * sizeof (wchar_t);
              MultiByteToWideChar (CP_ACP, 0, b0, -1, (wchar_t *)(reconv + 1), l + 1);
            }
          return reconv->dwSize;
        }
      catch (nonlocal_jump &)
        {
          print_condition (nonlocal_jump::data ());
          return 0;
        }
    }
  return 0;
}

int
kbd_queue::documentfeed (RECONVERTSTRING *rsbuf, int unicode_p)
{
  if (!idlep ())
    return 0;

  lisp hook = xsymbol_value (Vime_documentfeed_helper);
  if (hook == Qunbound || hook == Qnil)
    return 0;

  try
    {
      suppress_gc sgc;
      lisp r = Ffuncall (hook, Qnil);
      int n = multiple_value::count ();
      multiple_value::clear ();
      if (n != 2)
        return 0;

      lisp b = multiple_value::value (0);
      lisp c = multiple_value::value (1);

      char *content = w2s (c);
      char *before = w2s (b);
      long len = strlen (content);
      long offset = strlen (before);
      if (unicode_p)
        {
          int numc = MultiByteToWideChar (CP_ACP, 0, content, len, 0, 0);
          int numo = MultiByteToWideChar (CP_ACP, 0, before, offset, 0, 0);
          len = (numc + 1) * sizeof (wchar_t);
          offset = numo * sizeof (wchar_t);
        }
      long size = sizeof *rsbuf + len;

      if (!rsbuf)
        return size;

      rsbuf->dwSize = size;
      rsbuf->dwVersion = 0;
      rsbuf->dwStrLen = len;
      rsbuf->dwStrOffset = sizeof *rsbuf;
      rsbuf->dwCompStrLen = 0;
      rsbuf->dwCompStrOffset = 0;
      rsbuf->dwTargetStrLen = 0;
      rsbuf->dwTargetStrOffset = offset;

      if (!unicode_p)
        strncpy ((char *)(rsbuf + 1), content, len);
      else
        MultiByteToWideChar (CP_ACP, 0, content, -1, (wchar_t *)(rsbuf + 1), strlen (content));

      return size;
    }
  catch (nonlocal_jump &)
    {
      print_condition (nonlocal_jump::data ());
      return 0;
    }
  return 0;
}

int
kbd_queue::kbd_mblead_p (int c) const
{
  switch (PRIMARYLANGID (kbd_langid ()))
    {
    case LANG_JAPANESE:
      return SJISP (c);

    case LANG_KOREAN:
    case LANG_CHINESE:
      return c >= 0x80;

    default:
      return 0;
    }
}

HKL
kbd_queue::get_kbd_layout () const
{
  if (GetKeyboardLayout)
    return GetKeyboardLayout (0);

  char b[KL_NAMELENGTH];
  if (GetKeyboardLayoutName (b))
    return HKL (strtol (b, 0, 16));
  return HKL (MAKELANGID (LANG_JAPANESE, SUBLANG_DEFAULT));
}

void
kbd_queue::init_kbd_encoding (LANGID langid)
{
  set_kbd_langid (langid);
  ime_prop = app.kbdq.gime.ImmGetProperty (get_kbd_layout (), IGP_PROPERTY);

  unicode_kbd = 0;
  for (lisp p = xsymbol_value (Vkeyboard_layout_list); consp (p); p = xcdr (p))
    {
      long n;
      lisp x = xcar (p);
      if (consp (x)
          && (xcar (x) == Smultiply
              || (safe_fixnum_value (xcar (x), &n)
                  && n == PRIMARYLANGID (langid)))
          && (x = xcdr (x), consp (x))
          && (xcar (x) == Smultiply
              || (safe_fixnum_value (xcar (x), &n)
                  && n == SUBLANGID (langid)))
          && (x = xcdr (x), consp (x)))
        {
          x = xcar (x);
          if (symbolp (x))
            x = xsymbol_value (x);
          if (char_encoding_p (x)
              && xchar_encoding_type (x) != encoding_auto_detect)
            {
              xsymbol_value (Vkbd_encoding) = x;
              return;
            }
          if (x == Qnil)
            {
              unicode_kbd = 1;
              break;
            }
        }
    }
  xsymbol_value (Vkbd_encoding) =
    symbol_value_char_encoding (Vencoding_windows_latin1);
}

const FontObject &
kbd_queue::kbd_encoding_font ()
{
  lisp encoding = xsymbol_value (Vkbd_encoding);
  switch (xchar_encoding_type (encoding))
    {
    case encoding_sjis:
      return app.text_font.font (FONT_JP);

    case encoding_big5:
      return app.text_font.font (FONT_CN_TRADITIONAL);

    case encoding_windows_codepage:
      switch (xchar_encoding_windows_codepage (encoding))
        {
        case CP_JAPANESE:
          return app.text_font.font (FONT_JP);

        case CP_KOREAN:
          return app.text_font.font (FONT_HANGUL);

        case CP_CN_TRADITIONAL:
          return app.text_font.font (FONT_CN_TRADITIONAL);

        case CP_CN_SIMPLIFIED:
          return app.text_font.font (FONT_CN_SIMPLIFIED);

        case CP_CYRILLIC:
          return app.text_font.font (FONT_CYRILLIC);

        case CP_GREEK:
          return app.text_font.font (FONT_GREEK);
        }
      break;

    case encoding_iso2022_noesc:
      switch (xchar_encoding_iso_cjk (encoding))
        {
        case ENCODING_LANG_JP:
        case ENCODING_LANG_JP2:
          return app.text_font.font (FONT_JP);

        case ENCODING_LANG_KR:
          return app.text_font.font (FONT_HANGUL);

        case ENCODING_LANG_CN_GB:
          return app.text_font.font (FONT_CN_SIMPLIFIED);

        case ENCODING_LANG_CN_BIG5:
        case ENCODING_LANG_CN:
          return app.text_font.font (FONT_CN_TRADITIONAL);
        }
    }
  return app.text_font.font (FONT_LATIN);
}


key_sequence::key_sequence ()
     : k_used (0), k_last_used (0)
{
}

inline void
key_sequence::store (Char c)
{
  if (k_used < MAX_KEYSEQ)
    k_seq[k_used] = c;
  k_used++;
}

void
key_sequence::notice (int n, int cont)
{
  char buf[256];
  char *b = buf, *be = b + sizeof buf - 32;
  n--;
  for (int i = 0; b < be; i++)
    {
      b = print_key_sequence (b, be, k_seq[i]);
      if (i >= n)
        {
          if (cont)
            *b++ = '-';
          break;
        }
      *b++ = ' ';
    }
  *b = 0;
  app.status_window.puts (buf, 1);
}

void
key_sequence::push (Char c, int f)
{
  store (c);
  if (f)
    notice (k_used, 1);
}

void
key_sequence::done (Char c, int f)
{
  store (c);
  k_last_used = k_used;
  k_used = 0;
  if (k_last_used == 1)
    return;
  if (f)
    notice (k_last_used, 0);
}

void
key_sequence::again (int f)
{
  k_used = k_last_used;
  if (f)
    notice (k_used, 1);
}

static int
count_mblen (const char *string, int l)
{
  xinput_strstream in (string, l);
  encoding_input_stream_helper is (xsymbol_value (Vkbd_encoding), in);
  for (int len = 0; is->get () != xstream::eof; len++)
    ;
  return len;
}

static void
store_mbs (Char *b, const char *string, int l)
{
  xinput_strstream in (string, l);
  encoding_input_stream_helper is (xsymbol_value (Vkbd_encoding), in);
  int c;
  while ((c = is->get ()) != xstream::eof)
    *b++ = c;
}

void
ime_comp_queue::push (const char *comp, int compl, const char *read, int readl)
{
  int cl = count_mblen (comp, compl);
  int rl = count_mblen (read, readl);
  Char *b = (Char *)malloc ((cl + rl) * sizeof (Char));
  if (!b)
    return;
  qindex = (qindex + 1) % MAX_QUEUE;
  xfree (qbuf[qindex].comp);
  qbuf[qindex].comp = b;
  qbuf[qindex].compl = cl;
  qbuf[qindex].read = b + cl;
  qbuf[qindex].readl = rl;
  store_mbs (qbuf[qindex].comp, comp, compl);
  store_mbs (qbuf[qindex].read, read, readl);
}

static int
store_wcs (Char *b0, const ucs2_t *w, int l, const Char *tab)
{
  Char *b = b0;
  for (const ucs2_t *we = w + l; w < we; w++)
    {
      Char cc;
      if ((!tab || (cc = tab[*w]) == Char (-1))
          && (cc = w2i (*w)) == Char (-1))
        {
          *b++ = utf16_ucs2_to_undef_pair_high (*w);
          cc = utf16_ucs2_to_undef_pair_low (*w);
        }
      *b++ = cc;
    }
  return b - b0;
}

void
ime_comp_queue::push (const ucs2_t *comp, int compl, const ucs2_t *read, int readl,
                      const Char *tab)
{
  Char *b = (Char *)malloc ((compl + readl) * (sizeof (Char) * 2));
  if (!b)
    return;
  qindex = (qindex + 1) % MAX_QUEUE;
  xfree (qbuf[qindex].comp);
  qbuf[qindex].comp = b;
  qbuf[qindex].compl = store_wcs (qbuf[qindex].comp, comp, compl, tab);
  qbuf[qindex].read = b + compl * 2;
  qbuf[qindex].readl = store_wcs (qbuf[qindex].read, read, readl, tab);
}

const ime_comp_queue::pair *
ime_comp_queue::fetch () const
{
  return qbuf[qindex].comp ? &qbuf[qindex] : 0;
}

const ime_comp_queue::pair *
ime_comp_queue::pop ()
{
  const ime_comp_queue::pair *p = fetch ();
  if (p)
    qindex = (qindex - 1 + MAX_QUEUE) % MAX_QUEUE;
  return p;
}

lisp
Fsit_for (lisp timeout, lisp nodisp)
{
  if (!app.kbdq.macro_is_running ())
    {
      if (!nodisp || nodisp == Qnil)
        refresh_screen (0);
      app.kbdq.sit_for (DWORD (coerce_to_double_float (timeout) * 1000));
    }
  return Qnil;
}

lisp
Fsleep_for (lisp timeout)
{
  app.kbdq.sleep_for (DWORD (coerce_to_double_float (timeout) * 1000));
  return Qnil;
}

lisp
Fdo_events ()
{
  app.kbdq.process_events ();
  return Qnil;
}

lisp
Freset_prefix_args (lisp arg, lisp value)
{
  xsymbol_value (Vnext_prefix_args) = arg;
  xsymbol_value (Vnext_prefix_value) = value;
  app.keyseq.again (!app.kbdq.macro_is_running ());
  app.kbdq.close_ime ();
  app.kbdq.keep_next_command_key ();
  Fcontinue_pre_selection ();
  return Qt;
}

lisp
Fset_next_prefix_args (lisp arg, lisp value, lisp c)
{
  xsymbol_value (Vnext_prefix_args) = arg;
  xsymbol_value (Vnext_prefix_value) = value;
  if (c && c != Qnil)
    {
      check_char (c);
      app.keyseq.push (xchar_code (c), !app.kbdq.macro_is_running ());
      Fcontinue_pre_selection ();
      app.kbdq.close_ime ();
      app.kbdq.keep_next_command_key ();
    }
  return Qt;
}

lisp
Fstart_save_kbd_macro ()
{
  if (app.kbdq.save_p ())
    return Qnil;
  app.kbdq.start_macro ();
  return Qt;
}

lisp
Fstop_save_kbd_macro ()
{
  if (!app.kbdq.save_p ())
    return Qnil;
  return app.kbdq.end_macro ();
}

lisp
Fkbd_macro_saving_p ()
{
  return boole (app.kbdq.save_p ());
}

lisp
Ftoggle_ime (lisp arg)
{
  return boole (app.kbdq.toggle_ime (arg
                                     ? (arg == Qnil
                                        ? kbd_queue::IME_MODE_OFF
                                        : kbd_queue::IME_MODE_ON)
                                     : kbd_queue::IME_MODE_TOGGLE));
}

lisp
Fget_recent_keys ()
{
  Char b[128];
  int n = app.kbdq.copy_queue (b, numberof (b));
  return make_string (b, n);
}

lisp
Fget_ime_mode ()
{
  return boole (app.ime_open_mode == kbd_queue::IME_MODE_ON);
}

lisp
Fget_ime_composition_string ()
{
  const ime_comp_queue::pair *p = app.ime_compq.fetch ();
  if (!p)
    return Qnil;
  return xcons (make_string (p->comp, p->compl),
                make_string (p->read, p->readl));
}

lisp
Fpop_ime_composition_string ()
{
  const ime_comp_queue::pair *p = app.ime_compq.pop ();
  if (!p)
    return Qnil;
  return xcons (make_string (p->comp, p->compl),
                make_string (p->read, p->readl));
}

lisp
Fset_ime_read_string (lisp string)
{
  char *read;
  if (!string || string == Qnil)
    {
      const ime_comp_queue::pair *p = app.ime_compq.fetch ();
      if (!p)
        return Qnil;
      read = (char *)alloca (w2sl (p->read, p->readl) + 1);
      w2s (read, p->read, p->readl);
    }
  else
    {
      check_string (string);
      read = (char *)alloca (w2sl (string) + 1);
      w2s (read, string);
    }
  HIMC hIMC = app.kbdq.gime.ImmGetContext (app.toplev);
  if (!hIMC)
    return Qnil;
  int f = app.kbdq.gime.ImmSetCompositionString (hIMC, SCS_SETSTR, 0, 0,
                                                 read, strlen (read));
  app.kbdq.gime.ImmReleaseContext (app.toplev, hIMC);
  return boole (f);
}

lisp
Fime_register_word_dialog (lisp lcomp, lisp lread)
{
  REGISTERWORD rw;
  rw.lpWord = rw.lpReading = 0;
  if (lcomp && lcomp != Qnil)
    {
      check_string (lcomp);
      rw.lpWord = (char *)alloca (w2sl (lcomp) + 1);
      w2s (rw.lpWord, lcomp);
    }
  if (lread && lread != Qnil)
    {
      check_string (lread);
      rw.lpReading = (char *)alloca (w2sl (lread) + 2);
      char *e = w2s (rw.lpReading, lread);
      if (sysdep.Win95p ())
        {
          *e++ = ' ';
          *e = 0;
        }
    }
  return boole (app.kbdq.gime.ImmConfigureIME (GetKeyboardLayout (0), app.toplev,
                                               IME_CONFIG_REGISTERWORD, &rw));
}

lisp
Fenable_global_ime (lisp f)
{
  if (f == Qnil)
    app.kbdq.gime.disable ();
  else if (!app.kbdq.gime.enable (&app.atom_toplev, 1))
    FEsimple_win32_error (GetLastError ());
  return Qt;
}

static int
get_kbd_layout_name (HKL hkl, char *buf, int size)
{
  char k[256];
  sprintf (k, "SYSTEM\\CurrentControlSet\\Control\\Keyboard Layouts\\%08x", hkl);
  ReadRegistry r (HKEY_LOCAL_MACHINE, k);
  return ((!r.fail () && r.get ("Layout Text", buf, size) > 0)
          || app.kbdq.gime.ImmGetDescription (hkl, buf, size) > 0);
}

typedef UINT (WINAPI *GETKEYBOARDLAYOUTLIST)(int, HKL *);
static const GETKEYBOARDLAYOUTLIST pGetKeyboardLayoutList =
  (GETKEYBOARDLAYOUTLIST)GetProcAddress (GetModuleHandle ("user32"),
                                         "GetKeyboardLayoutList");

lisp
Flist_kbd_layout ()
{
  if (!pGetKeyboardLayoutList)
    return Qnil;
  int n = (*pGetKeyboardLayoutList)(0, 0);
  if (!n)
    return Qnil;
  HKL *h = (HKL *)alloca (sizeof *h * n);
  n = GetKeyboardLayoutList (n, h);
  if (!n)
    FEsimple_win32_error (GetLastError ());

  lisp r = Qnil;
  for (int i = 0; i < n; i++)
    {
      char buf[256];
      if (get_kbd_layout_name (h[i], buf, sizeof buf)
          || get_kbd_layout_name (HKL (HIWORD (h[i])), buf, sizeof buf))
        r = xcons (xcons (make_fixnum (int (h[i])),
                          make_string (buf)),
                   r);
    }
  return r;
}

lisp
Fselect_kbd_layout (lisp layout)
{
  HKL hkl;
  if (stringp (layout))
    {
      if (!pGetKeyboardLayoutList)
        return Qnil;
      int n = (*pGetKeyboardLayoutList)(0, 0);
      if (!n)
        return Qnil;
      HKL *h = (HKL *)alloca (sizeof *h * n);
      n = GetKeyboardLayoutList (n, h);
      if (!n)
        FEsimple_win32_error (GetLastError ());

      char name[256];
      w2s (name, name + sizeof name,
           xstring_contents (layout), xstring_length (layout));

      for (int i = 0; i < n; i++)
        {
          char buf[256];
          if ((get_kbd_layout_name (h[i], buf, sizeof buf)
               || get_kbd_layout_name (HKL (LOWORD (h[i])), buf, sizeof buf))
              && !strcmp (buf, name))
            {
              hkl = h[i];
              break;
            }
        }
      if (i == n)
        return Qnil;
    }
  else if (layout == Qnil)
    hkl = HKL (HKL_NEXT);
  else if (layout == Qt)
    hkl = HKL (HKL_PREV);
  else
    {
      if (!fixnump (layout))
        FEtype_error (layout, xsymbol_value (Qor_string_integer));
      hkl = HKL (fixnum_value (layout));
    }
  if (!ActivateKeyboardLayout (hkl, 0))
    FEsimple_win32_error (GetLastError ());
  return Qt;
}

lisp
Fcurrent_kbd_layout ()
{
  HKL hkl = app.kbdq.get_kbd_layout ();
  char buf[256];
  if (get_kbd_layout_name (hkl, buf, sizeof buf)
      || get_kbd_layout_name (HKL (HIWORD (hkl)), buf, sizeof buf))
    return xcons (make_fixnum (int (hkl)), make_string (buf));
  return xcons (make_fixnum (int (hkl)), Qnil);
}

int
char_mouse_move_p (Char cc)
{
  if (function_char_p (cc))
    cc = meta_function_to_function (cc & ~(CCF_SHIFT_BIT | CCF_CTRL_BIT));
  return cc == CCF_MOUSEMOVE;
}
