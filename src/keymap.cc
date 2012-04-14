#include "stdafx.h"
#include "ed.h"

#define KEYMAP_LENGTH (128 + 4 * NFUNCTION_KEYS)

static inline u_int
keymap_length (lisp map)
{
  return u_int (xvector_length (map));
}

lisp
Fkeymapp (lisp map)
{
  if (map == Qnil)
    return Qnil;
  if (symbolp (map))
    map = xsymbol_function (map);
  if (general_vector_p (map))
    return xvector_length (map) == KEYMAP_LENGTH ? map : Qnil;
  return consp (map) && xcar (map) == Qkeymap ? map : Qnil;
}

lisp
Fmake_keymap ()
{
  lisp map = make_vector (KEYMAP_LENGTH, Qnil);
  return map;
}

lisp
Fmake_sparse_keymap ()
{
  return xcons (Qkeymap, Qnil);
}

static u_int
full_keymap_index (Char c)
{
  if (function_char_p (c))
    {
      int x = 128 + (c & CCF_CHAR_MASK);
      if (c & CCF_SHIFT_BIT)
        x += NFUNCTION_KEYS;
      if (c & CCF_CTRL_BIT)
        x += 2 * NFUNCTION_KEYS;
      return x;
    }
  return c < 128 ? c : -1;
}

static lisp
parse_keymap (Char c, lisp map)
{
  if (general_vector_p (map))
    {
      u_int i = full_keymap_index (c);
      if (i < keymap_length (map))
        return xvector_contents (map) [i];
      return Qnil;
    }
  if (consp (map) && xcar (map) == Qkeymap)
    {
      lisp cc = make_char (c);
      for (map = xcdr (map); consp (map); map = xcdr (map))
        {
          lisp p = xcar (map);
          if (consp (p) && xcar (p) == cc)
            return xcdr (p);
        }
    }
  return Qnil;
}

lisp
Fcurrent_selection_keymap ()
{
  Window *wp = selected_window ();
  if (wp->w_selection_type != Buffer::SELECTION_VOID)
    switch (wp->w_selection_type & Buffer::SELECTION_TYPE_MASK)
      {
      case Buffer::SELECTION_LINEAR:
      case Buffer::SELECTION_REGION:
      case Buffer::SELECTION_RECTANGLE:
        return Fkeymapp (symbol_value (Vselection_keymap, wp->w_bufp));
      }
  return Qnil;
}

lisp
lookup_keymap (Char c, lisp *map, int n)
{
  int i;
  if (meta_function_char_p (c))
    {
      for (i = 0; i < n; i++)
        map[i] = parse_keymap (CC_ESC, Fkeymapp (map[i]));
      c = meta_function_to_function (c);
    }
  else if (meta_char_p (c))
    {
      for (i = 0; i < n; i++)
        map[i] = parse_keymap (CC_ESC, Fkeymapp (map[i]));
      c = meta_char_to_char (c);
    }

  lisp *save = 0;
  if (alpha_char_p (c))
    {
      save = (lisp *)alloca (sizeof *save * n);
      memcpy (save, map, sizeof *save * n);
    }

  for (i = 0; i < n; i++)
    {
      map[i] = parse_keymap (c, Fkeymapp (map[i]));
      if (map[i] != Qnil)
        save = 0;
    }

  if (save)
    {
      c = _char_transpose_case (c);
      for (i = 0; i < n; i++)
        map[i] = parse_keymap (c, Fkeymapp (save[i]));
    }

  int f_contunue = 0;
  for (i = 0; i < n; i++)
    if (map[i] != Qnil)
      {
        if (!f_contunue && Fkeymapp (map[i]) == Qnil)
          return map[i];
        f_contunue = 1;
      }
  return f_contunue ? 0 : Qnil;
}

lisp
Fuse_keymap (lisp keymap, lisp buffer)
{
  if (Fkeymapp (keymap) == Qnil)
    FEtype_error (keymap, Qkeymap);
  Buffer::coerce_to_buffer (buffer)->lmap = keymap;
  return Qt;
}

lisp
Fset_minor_mode_map (lisp keymap, lisp buffer)
{
  if (Fkeymapp (keymap) == Qnil)
    FEtype_error (keymap, Qkeymap);
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  if (!memq (keymap, bp->lminor_map))
    bp->lminor_map = xcons (keymap, bp->lminor_map);
  return Qt;
}

lisp
Funset_minor_mode_map (lisp keymap, lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  return boole (delq (keymap, &bp->lminor_map));
}

lisp
Fminor_mode_map (lisp buffer)
{
  return Buffer::coerce_to_buffer (buffer)->lminor_map;
}

static lisp *
scan_key_slot (lisp keymap, Char c, int igcase)
{
  if (general_vector_p (keymap))
    {
      u_int i = full_keymap_index (c);
      if (i >= keymap_length (keymap))
        return 0;
      lisp *v = &xvector_contents (keymap) [i];
      if (!igcase || *v != Qnil || !alpha_char_p (c))
        return v;
      i = full_keymap_index (_char_transpose_case (c));
      if (i >= keymap_length (keymap))
        return 0;
      return &xvector_contents (keymap) [i];
    }

  lisp cc = make_char (c);
  for (lisp p = keymap; consp (p); p = xcdr (p))
    {
      lisp x = xcar (p);
      if (consp (x) && xcar (x) == cc)
        return &xcdr (x);
    }

  if (igcase && alpha_char_p (c))
    {
      cc = make_char (_char_transpose_case (c));
      for (lisp p = keymap; consp (p); p = xcdr (p))
        {
          lisp x = xcar (p);
          if (consp (x) && xcar (x) == cc)
            return &xcdr (x);
        }
    }

  return 0;
}

static lisp *
make_key_slot (lisp keymap, Char c)
{
  if (general_vector_p (keymap))
    {
      u_int i = full_keymap_index (c);
      if (i >= keymap_length (keymap))
        return 0;
      return &xvector_contents (keymap) [i];
    }

  if (consp (keymap))
    {
      lisp x = xcons (make_char (c), Qnil);
      xcdr (keymap) = xcons (x, xcdr (keymap));
      return &xcdr (x);
    }
  return 0;
}

static lisp *
search_key_slot (lisp keymap, lisp key, int bindp, int igcase)
{
  lisp *v;
  lisp map = Fkeymapp (keymap);
  if (map == Qnil)
    FEtype_error (keymap, Qkeymap);

  if (charp (key))
    {
      Char c = xchar_code (key);
      if (meta_char_p (c))
        c = meta_char_to_char (c);
      else if (meta_function_char_p (c))
        c = meta_function_to_function (c);
      else
        {
          v = scan_key_slot (map, c, igcase);
          if (v || !bindp)
            return v;
          return make_key_slot (map, c);
        }

      v = scan_key_slot (map, CC_ESC, igcase);
      if (!v)
        {
          if (!bindp)
            return 0;
          v = make_key_slot (map, CC_ESC);
          if (!v)
            return 0;
          map = *v = Fmake_sparse_keymap ();
        }
      else
        {
          map = Fkeymapp (*v);
          if (map == Qnil)
            {
              if (!bindp)
                return 0;
              map = *v = Fmake_sparse_keymap ();
            }
        }

      v = scan_key_slot (map, c, igcase || !bindp);
      if (v || !bindp)
        return v;
      return make_key_slot (map, c);
    }

  if (!consp (key))
    FEtype_error (key, xsymbol_value (Qor_character_cons));

  while (1)
    {
      v = search_key_slot (map, xcar (key), bindp, igcase);
      if (!v)
        return 0;
      key = xcdr (key);
      if (!consp (key))
        return v;
      map = Fkeymapp (*v);
      if (map == Qnil)
        {
          if (!bindp)
            return 0;
          map = *v = Fmake_sparse_keymap ();
        }
      if (!bindp)
        igcase = 1;
    }
}

lisp
Fdefine_key (lisp keymap, lisp key, lisp fn)
{
  lisp *x = search_key_slot (keymap, key, 1, 0);
  if (!x)
    return Qnil;
  *x = fn;
  return Qt;
}

lisp PASCAL
Flookup_keymap (lisp keymap, lisp key, lisp ignore_case, lisp symbol_only)
{
  lisp *x = search_key_slot (keymap, key, 0, ignore_case && ignore_case != Qnil);
  if (!x)
    return Qnil;
  if (symbol_only && symbol_only != Qnil)
    return symbolp (*x) ? *x : Qnil;
  return *x;
}

lisp
Flocal_keymap (lisp buffer)
{
  return Buffer::coerce_to_buffer (buffer)->lmap;
}

lisp
Fkeymap_index_char (lisp code)
{
  int n = fixnum_value (code);
  if (n < 0 || n >= KEYMAP_LENGTH)
    return Qnil;

  if (n < 128)
    return make_char (n);

  n -= 128;
  Char c = CCF_CHAR_MIN + n % NFUNCTION_KEYS;
  n /= NFUNCTION_KEYS;
  if (n & 1)
    c |= CCF_SHIFT_BIT;
  if (n & 2)
    c |= CCF_CTRL_BIT;
  return make_char (c);
}

lisp
Fkeymap_char_index (lisp c)
{
  check_char (c);
  u_int i = full_keymap_index (xchar_code (c));
  return i < KEYMAP_LENGTH ? make_fixnum (i) : Qnil;
}

static lisp *
expand_keymap (lisp keymap, lisp *buf)
{
  if (general_vector_p (keymap))
    return xvector_contents (keymap);

  for (int i = 0; i < KEYMAP_LENGTH; i++)
    buf[i] = Qnil;

  if (consp (keymap) && xcar (keymap) == Qkeymap)
    for (lisp p = xcdr (keymap); consp (p); p = xcdr (p))
      {
        lisp x = xcar (p);
        if (consp (x) && charp (xcar (x)))
          {
            u_int idx = full_keymap_index (xchar_code (xcar (x)));
            if (idx < KEYMAP_LENGTH)
              buf[idx] = xcdr (x);
          }
      }
  return buf;
}

struct keyseq_list
{
  keyseq_list *prev;
  keyseq_list *next;
  Char c;
};

static int
command_shadow_p (const keyseq_list *p, lisp keymap)
{
  for (; p; p = p->next)
    {
      keymap = parse_keymap (p->c, Fkeymapp (keymap));
      if (keymap == Qnil)
        return 0;
    }
  return 1;
}

static int
command_shadow_p (keyseq_list *tail, const lisp *shadow, int nshadow)
{
  keyseq_list *head, *last;
  for (head = tail, last = 0;
       (head->next = last), head->prev;
       last = head, head = head->prev)
    ;

  for (int i = 0; i < nshadow; i++)
    if (command_shadow_p (head, shadow[i]))
      return 1;
  return 0;
}

static lisp
command_keys (lisp command, lisp keymap, keyseq_list *prev,
              const lisp *shadow, int nshadow)
{
  keyseq_list keyseq;
  keyseq.prev = prev;
  int i;
  lisp result = Qnil;
  lisp b[KEYMAP_LENGTH];
  lisp *map = expand_keymap (Fkeymapp (keymap), b);
  lisp *p;
  for (i = 0, p = map; i < KEYMAP_LENGTH; i++, p++)
    if (*p == command)
      {
        lisp lc = Fkeymap_index_char (make_fixnum (i));
        keyseq.c = xchar_code (lc);
        if (!command_shadow_p (&keyseq, shadow, nshadow))
          result = Fcons (lc, result);
      }

  for (i = 0, p = map; i < KEYMAP_LENGTH; i++, p++)
    if (Fkeymapp (*p) != Qnil)
      {
        lisp lc = Fkeymap_index_char (make_fixnum (i));
        keyseq.c = xchar_code (lc);
        lisp sub = command_keys (command, *p, &keyseq, shadow, nshadow);
        if (sub != Qnil)
          result = Fcons (Fcons (lc, sub), result);
      }

  return Fnreverse (result);
}

lisp
Fcommand_keys (lisp command, lisp gmap, lisp lmap, lisp minor_map)
{
  if (!minor_map)
    minor_map = Qnil;
  int nmaps = xlist_length (minor_map) + 2;
  lisp *map = (lisp *)alloca (nmaps * sizeof *map);
  lisp *p;
  for (p = map; consp (minor_map); minor_map = xcdr (minor_map))
    *p++ = xcar (minor_map);
  *p++ = lmap;
  *p++ = gmap;

  lisp result = Qnil;
  for (int i = 0; i < nmaps; i++)
    {
      lisp r = command_keys (command, map[i], 0, map, i);
      if (consp (r))
        {
          lisp x;
          for (x = r; consp (xcdr (x)); x = xcdr (x))
            ;
          xcdr (x) = result;
          result = r;
        }
    }
  return result;
}

static int
command_shadow_p (const Char *b, const Char *be, lisp keymap)
{
  for (; b < be; b++)
    {
      keymap = parse_keymap (*b, Fkeymapp (keymap));
      if (keymap == Qnil)
        return 0;
    }
  return 1;
}

static int
command_shadow_p (const Char *b, const Char *be, const lisp *shadow, int nshadow)
{
  for (int i = 0; i < nshadow; i++)
    if (command_shadow_p (b, be, shadow[i]))
      return 1;
  return 0;
}

Char *
lookup_command_keyseq (lisp command, lisp keymap, const lisp *shadow, int nshadow,
                       Char *keyb, Char *keyp, Char *keye)
{
  if (keyp == keye)
    return 0;

  int i;
  lisp *p;
  lisp b[KEYMAP_LENGTH];
  lisp *map = expand_keymap (Fkeymapp (keymap), b);

  for (i = 0, p = map; i < KEYMAP_LENGTH; i++, p++)
    if (*p == command)
      {
        *keyp++ = xchar_code (Fkeymap_index_char (make_fixnum (i)));
        if (!command_shadow_p (keyb, keyp, shadow, nshadow))
          return keyp;
      }

  for (i = 0, p = map; i < KEYMAP_LENGTH; i++, p++)
    if (Fkeymapp (*p) != Qnil)
      {
        *keyp = xchar_code (Fkeymap_index_char (make_fixnum (i)));
        Char *e = lookup_command_keyseq (command, *p, shadow, nshadow, keyb, keyp + 1, keye);
        if (e)
          return e;
      }
  return 0;
}

static int
find_in_current_keymaps (Char c, lisp map)
{
  lisp command = parse_keymap (c, map);
  return command != Qnil && Fkeymapp (command) == Qnil;
}

int
find_in_current_keymaps (Char c)
{
  if (find_in_current_keymaps (c, xsymbol_value (Vglobal_keymap)))
    return 1;

  Buffer *bp = selected_buffer ();
  if (find_in_current_keymaps (c, bp->lmap))
    return 1;

  if (Flist_length (bp->lminor_map) != Qnil)
    for (lisp p = bp->lminor_map; consp (p); p = xcdr (p))
      if (find_in_current_keymaps (c, xcar (p)))
        return 1;

  if (find_in_current_keymaps (c, Fcurrent_selection_keymap ()))
    return 1;

  return 0;
}
