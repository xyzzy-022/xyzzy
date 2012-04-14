#include "stdafx.h"
#include "chtype.h"

struct print_char_name
{
  const char *name;
  int l;
  Char code;
};

#define X(A, B) {A, sizeof A - 1, B}

static const print_char_name standard_char_names[] =
{
  X ("TAB", CC_TAB),
  X ("SPC", CC_SPC),
  X ("LFD", CC_NL),
  X ("RET", CC_CR),
  X ("ESC", CC_ESC),
  X ("DEL", CC_DEL),
  X ("NUL", 0),

  // Common Lisp compat char name
  X ("Backspace", CC_BS),
  X ("Newline", CC_NL),
  X ("Linefeed", CC_NL),
  X ("Page", CC_FF),
  X ("Return", CC_CR),
  X ("Space", CC_SPC),
  X ("Rubout", CC_DEL),
};

static const print_char_name function_char_names[] =
{
  X ("PageUp", CCF_PRIOR),
  X ("PageDown", CCF_NEXT),
  X ("End", CCF_END),
  X ("Home", CCF_HOME),
  X ("Left", CCF_LEFT),
  X ("Up", CCF_UP),
  X ("Right", CCF_RIGHT),
  X ("Down", CCF_DOWN),
  X ("Pause", CCF_PAUSE),
  X ("MouseMove", CCF_MOUSEMOVE),
  X ("Scroll", CCF_SCROLL),
  X ("Apps", CCF_APPS),
  X ("Insert", CCF_INSERT),
  X ("Delete", CCF_DELETE),
  X ("Help", CCF_HELP),
  X ("F24", CCF_F24),
  X ("F23", CCF_F23),
  X ("F22", CCF_F22),
  X ("F21", CCF_F21),
  X ("F20", CCF_F20),
  X ("F19", CCF_F19),
  X ("F18", CCF_F18),
  X ("F17", CCF_F17),
  X ("F16", CCF_F16),
  X ("F15", CCF_F15),
  X ("F14", CCF_F14),
  X ("F13", CCF_F13),
  X ("F12", CCF_F12),
  X ("F11", CCF_F11),
  X ("F10", CCF_F10),
  X ("F9", CCF_F9),
  X ("F8", CCF_F8),
  X ("F7", CCF_F7),
  X ("F6", CCF_F6),
  X ("F5", CCF_F5),
  X ("F4", CCF_F4),
  X ("F3", CCF_F3),
  X ("F2", CCF_F2),
  X ("F1", CCF_F1),
  X ("LBtnDown", CCF_LBTNDOWN),
  X ("LBtnUp", CCF_LBTNUP),
  X ("LBtnMove", CCF_LBTNMOVE),
  X ("RBtnDown", CCF_RBTNDOWN),
  X ("RBtnUp", CCF_RBTNUP),
  X ("RBtnMove", CCF_RBTNMOVE),
  X ("MBtnDown", CCF_MBTNDOWN),
  X ("MBtnUp", CCF_MBTNUP),
  X ("MBtnMove", CCF_MBTNMOVE),
  X ("XBtn1Down", CCF_XBTN1DOWN),
  X ("XBtn1Up", CCF_XBTN1UP),
  X ("XBtn1Move", CCF_XBTN1MOVE),
  X ("XBtn2Down", CCF_XBTN2DOWN),
  X ("XBtn2Up", CCF_XBTN2UP),
  X ("XBtn2Move", CCF_XBTN2MOVE),
};

static const print_char_name char_bit_names[] =
{
  X ("C-", CCF_CTRL_BIT),
  X ("M-", CCF_META),
  X ("S-", CCF_SHIFT_BIT),
};

static inline int
seql (const Char *p1, const u_char *p2)
{
  for (; *p2; p1++, p2++)
    if (char_upcase (*p1) != _char_upcase (*p2))
      return 0;
  return 1;
}

static inline int
sequal (const Char *p1, const u_char *p2)
{
  for (; *p2; p1++, p2++)
    if (*p1 != *p2)
      return 0;
  return 1;
}

Char
standard_char_name2Char (const Char *name, int l)
{
  for (const print_char_name *p = standard_char_names,
       *pe = p + numberof (standard_char_names);
       p < pe; p++)
    if (l == p->l && seql (name, (const u_char *)p->name))
      return p->code;
  return Char (-1);
}

Char
function_char_name2Char (const Char *name, int l)
{
  for (const print_char_name *p = function_char_names,
       *pe = p + numberof (function_char_names);
       p < pe; p++)
    if (l == p->l && sequal (name, (const u_char *)p->name))
      return p->code;
  return Char (-1);
}

Char
char_bit_name2Char (const Char *name, int l, int &xl)
{
  for (const print_char_name *p = char_bit_names,
       *pe = p + numberof (char_bit_names);
       p < pe; p++)
    if (l >= p->l && seql (name, (const u_char *)p->name))
      {
        xl = p->l;
        return p->code;
      }
  return 0;
}

const char *
function_Char2name (Char c)
{
  for (const print_char_name *p = function_char_names,
       *pe = p + numberof (function_char_names);
       p < pe; p++)
    if (p->code == c)
      return p->name;
  return 0;
}

const char *
standard_Char2name (Char c)
{
  for (const print_char_name *p = standard_char_names,
       *pe = p + numberof (standard_char_names);
       p < pe; p++)
    if (p->code == c)
      return p->name;
  return 0;
}
