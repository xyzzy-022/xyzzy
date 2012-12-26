#include "stdafx.h"
#include "ed.h"

static int minibuffer_recursive_level;

static Buffer *
create_minibuffer ()
{
  char b[32];
  sprintf (b, " *Minibuf%d*", minibuffer_recursive_level);
  return Buffer::make_internal_buffer (b);
}

static lisp
load_default (const char *fmt, lisp keys, int number)
{
  if (keys == Qnil)
    return Qnil;

  char b[32];
  sprintf (b, fmt, number);
  int l = strlen (b);
  Char w[32];
  a2w (w, b, l);
  temporary_string t (w, l);
  lisp var = Ffind_symbol (t.string (), xsymbol_value (Vkeyword_package));
  return var != Qnil ? find_keyword (var, keys) : Qnil;
}

lisp
load_default (lisp keys, int number)
{
  return load_default ("default%d", keys, number);
}

lisp
load_history (lisp keys, int number)
{
  return load_default ("history%d", keys, number);
}

lisp
load_history (lisp keys, int number, lisp def)
{
  lisp x = load_history (keys, number);
  return x != Qnil ? x : def;
}

lisp
load_title (lisp keys, int number)
{
  return load_default ("title%d", keys, number);
}

static int
insert_default (Window *wp, lisp def, int noselect)
{
  point_t opoint = wp->w_point.p_point;
  if (stringp (def))
    {
      if (!wp->w_bufp->insert_chars_internal (wp->w_point,
                                              xstring_contents (def),
                                              xstring_length (def), 1))
        return 0;
      if (noselect)
        return 1;
      if (wp->w_point.p_point != opoint)
        Fstart_selection (make_fixnum (Buffer::SELECTION_REGION), Qt, 0);
    }
  wp->w_bufp->goto_char (wp->w_point, opoint);
  return 1;
}

static int
count_prompt_columns (const Char *s, int l)
{
  int n = 0;
  for (const Char *se = s + l; s < se; s++)
    n += char_width (*s);
  return n;
}

lisp
read_minibuffer (const Char *prompt, long prompt_length, lisp def,
                 lisp type, lisp compl, lisp history,
                 int noselect, int completion, int must_match,
                 lisp title, int opt_arg)
{
  static int last_ime_mode = kbd_queue::IME_MODE_OFF;

  check_kbd_enable ();
  Window *wp = selected_window ();
  Buffer *curbp = selected_buffer ();
  if (wp->minibuffer_window_p ()
      && symbol_value (Venable_recursive_minibuffers, curbp) == Qnil)
    FEsimple_error (Eattempt_to_use_minibuffer_recursively);

  Buffer *bp = create_minibuffer ();
  bp->ldirectory = curbp->ldirectory;
  bp->lsyntax_table = curbp->lsyntax_table;
  bp->lminibuffer_buffer = curbp->lbp;
  bp->ldialog_title = title;
  bp->lminibuffer_default = stringp (def) ? def : Qnil;
  bp->lmap = xsymbol_value (type == Kcommand_line
                            ? Vminibuffer_local_command_line_map
                            : (completion
                               ? (must_match
                                  ? Vminibuffer_local_must_match_map
                                  : Vminibuffer_local_completion_map)
                               : Vminibuffer_local_map));
  if (bp->lmap == Qunbound)
    bp->lmap = Qnil;

  WindowConfiguration wc;

  protect_gc gcpro4 (type);
  protect_gc gcpro5 (compl);

  bp->run_hook (Venter_minibuffer_hook, bp->lbp, history);

  bp->b_prompt = prompt;
  bp->b_prompt_length = prompt_length;
  bp->b_prompt_columns = count_prompt_columns (prompt, prompt_length);
  *bp->b_prompt_arg = 0;
  if (!opt_arg)
    {
      long n;
      if (xsymbol_value (Vprefix_args) == Vuniversal_argument
          && safe_fixnum_value (xsymbol_value (Vprefix_value), &n)
          && n == 4)
        strcpy (bp->b_prompt_arg, "C-u ");
      else if (safe_fixnum_value (xsymbol_value (Vprefix_value), &n))
        sprintf (bp->b_prompt_arg, "%d ", n);
    }
  bp->b_prompt_columns += strlen (bp->b_prompt_arg);

  bp->b_minibufferp = 1;
  bp->b_fold_mode = bp->b_fold_columns = Buffer::FOLD_NONE;
  bp->fold_width_modified ();
  bp->lcomplete_type = type;
  bp->lcomplete_list = compl;
  bp->b_ime_mode = last_ime_mode;
  last_ime_mode = kbd_queue::IME_MODE_OFF;

  Window *mini = Window::minibuffer_window ();
  mini->set_buffer_params (bp);

  mini->set_window ();
  mini->w_flags = 0;
  minibuffer_recursive_level++;

  lisp result = Qnil;
  lisp nld_type = 0, nld_id = 0;
  int abnormal_exit = 0;
  try
    {
      if (insert_default (mini, def, noselect))
        main_loop ();
      abnormal_exit = 1;
    }
  catch (nonlocal_jump &)
    {
      nonlocal_data *nld = nonlocal_jump::data ();
      result = nld->value;
      nld_type = nld->type;
      nld_id = nld->id;
    }

  protect_gc gcpro (result);
  protect_gc gcpro2 (nld_id);  // nld_type is a symbol.

  bp->lcomplete_type = Qnil;
  bp->lcomplete_list = Qnil;

  bp->b_prompt = 0;
  bp->b_prompt_length = 0;
  bp->b_prompt_columns = 0;
  *bp->b_prompt_arg = 0;
  if (xsymbol_value (Vminibuffer_save_ime_status) != Qnil)
    last_ime_mode = bp->b_ime_mode;

  if (--minibuffer_recursive_level)
    bp->b_minibufferp = 0;

  lisp contents = Qnil;
  protect_gc gcpro3 (contents);

  if (!abnormal_exit)
    {
      if (nld_type == Qexit_this_level && nld_id == Qnil)
        {
          try
            {
              contents = bp->substring (0, bp->b_nchars);
            }
          catch (nonlocal_jump &)
            {
            }
        }
      bp->run_hook (Vexit_minibuffer_hook, bp->lbp, contents);
    }

  bp->lminibuffer_buffer = Qnil;
  bp->lvar = Qnil;
  bp->ldialog_title = Qnil;
  bp->lminibuffer_default = Qnil;

  if (minibuffer_recursive_level)
    Fdelete_buffer (bp->lbp);
  else
    Ferase_buffer (bp->lbp);

  if (abnormal_exit)
    Fexit_recursive_edit (Qnil);

  if (contents == Qnil)
    {
      if (nld_type == Qexit_this_level)
        Fsi_throw_error (nld_id);
      throw nonlocal_jump ();
    }

  return result != Qnil ? result : contents;
}

lisp
complete_read (const Char *prompt, long prompt_length, lisp def,
               lisp type, lisp compl, lisp history,
               int must_match, int opt_arg)
{
  lisp string = read_minibuffer (prompt, prompt_length, def, type, compl,
                                 history, 0, 1, must_match, Qnil, opt_arg);

  if (!symbolp (type))
    return string;

  if (type == Kexist_buffer_name)
    return Ffind_buffer (string);

  if (type == Kbuffer_name)
    {
      if (stringp (string) && !xstring_length (string))
        return def;
      lisp x = Ffind_buffer (string);
      return x == Qnil ? string : x;
    }

  if (type == Ksymbol_name || type == Kfunction_name
      || type == Kcommand_name || type == Kvariable_name
      || type == Knon_trivial_symbol_name)
    {
      lisp package = coerce_to_package (0);
      lisp lpkg = symbol_value (Vbuffer_package, selected_buffer ());
      if (stringp (lpkg))
        {
          lpkg = Ffind_package (lpkg);
          if (lpkg != Qnil)
            package = lpkg;
        }

      Char *b = xstring_contents (string);
      int l = xstring_length (string);

      maybe_symbol_string mss (package);
      mss.parse (b, l);
      package = mss.current_package ();

      if (!mss.pkg_end ())
        return Fintern (string, 0);

      return Fintern (make_string (b, (xstring_contents (string)
                                       + xstring_length (string) - b)),
                      package);
    }

  if (type == Kchar_encoding || type == Kexact_char_encoding)
    return find_char_encoding (string);

  return string;
}

lisp
read_filename (const Char *prompt, long prompt_length, lisp type,
               lisp title, lisp defalt, lisp history)
{
  Buffer *bp = selected_buffer ();
  return read_minibuffer (prompt, prompt_length,
                          (defalt != Qnil
                           ? defalt
                           : (symbol_value (Vinsert_default_directory, bp) != Qnil
                              ? bp->ldirectory : Qnil)),
                          type, Qnil,
                          (history != Qnil
                           ? history
                           : type == Kdirectory_name ? Kdirectory_name : Kfile_name),
                          1, 1,
                          type == Kexist_file_name || type == Kdirectory_name,
                          title, -1);
}

lisp
minibuffer_read_integer (const Char *prompt, long prompt_length)
{
  lisp string = read_minibuffer (prompt, prompt_length, Qnil, Kinteger, Qnil, Kinteger,
                                 0, 0, 0, Qnil, -1);
  assert (stringp (string));
  int l = xstring_length (string);
  return parse_integer (string, 0, l, 10, 1);
}

lisp
Fquit_recursive_edit (lisp silent)
{
  nonlocal_data *nld = nonlocal_jump::data ();
  nld->type = Qexit_this_level;
  nld->value = Qnil;
  nld->tag = Qnil;
  nld->id = xsymbol_value (silent && silent != Qnil
                           ? Vierror_silent_quit
                           : Vierror_quit);
  throw nonlocal_jump ();
  /*NOTREACHED*/
  return Qnil; /* avoid warning */
}

lisp
Fexit_recursive_edit (lisp value)
{
  nonlocal_data *nld = nonlocal_jump::data ();
  nld->type = Qexit_this_level;
  nld->value = value ? value : Qnil;
  nld->tag = Qnil;
  nld->id = Qnil;
  throw nonlocal_jump ();
  /*NOTREACHED*/
  return Qnil; /* avoid warning */
}

class completion
{
  lisp c_type;
  lisp c_string;
  lisp c_target;
  int c_target_len;
  int c_match_len;
  lisp c_result;
  lisp c_item;
  lisp c_matches_list;
  int c_strict_match;
  int c_nmatches;
  int c_no_completions;
  int c_word;
  lisp c_prefix;
  int c_force_no_match;

  int do_completion (lisp, int);
  void complete_with_slash (lisp, int);
  void fix_match_len ();
  void complete_symbol (lisp);
  void set_target (lisp);
  void set_prefix (lisp);
  void adjust_prefix (lisp);
  int complete_filename (const char *, lisp, lisp);
  lisp split_pathname ();
  int complete_UNC (lisp &);
public:
  completion (lisp, lisp, int);
  void complete_symbol ();
  void complete_buffer_name ();
  void complete_filename ();
  void complete_char_encoding ();
  void complete_list (lisp, int);
  lisp result () const;
};

inline void
completion::set_target (lisp string)
{
  assert (stringp (string));
  c_target = string;
  c_target_len = xstring_length (string);
}

inline void
completion::set_prefix (lisp prefix)
{
  assert (stringp (prefix));
  c_prefix = prefix;
}

completion::completion (lisp type, lisp string, int word)
{
  c_type = type;
  c_string = string;
  set_target (string);
  c_match_len = 0;
  c_result = 0;
  c_item = Qnil;
  c_matches_list = Qnil;
  c_strict_match = 0;
  c_nmatches = 0;
  c_no_completions = 1;
  c_word = word;
  c_prefix = Qnil;
  c_force_no_match = 0;
}

int
completion::do_completion (lisp candidate, int igcase)
{
  c_no_completions = 0;

  lisp item = c_item == Qnil ? c_target : c_item;
  lisp eq = (igcase
             ? Fstring_not_equalp (item, candidate, Qnil)
             : Fstring_not_equal (item, candidate, Qnil));
  int l = eq == Qnil ? xstring_length (item) : fixnum_value (eq);

  if (l < c_target_len)
    return 0;

  if (memq (candidate, c_matches_list))
    return 1;
  c_matches_list = Fcons (candidate, c_matches_list);
  c_nmatches++;

  if (l == c_target_len && l == xstring_length (candidate))
    c_strict_match = 1;
  if (c_item == Qnil)
    {
      c_item = candidate;
      c_match_len = xstring_length (candidate);
    }
  else
    c_match_len = min (c_match_len, l);

  return 1;
}

void
completion::complete_with_slash (lisp s, int igcase)
{
  if (stringp (s))
    {
      lisp d = make_string (xstring_length (s) + 1);
      bcopy (xstring_contents (s), xstring_contents (d),
             xstring_length (s));
      xstring_contents (d)[xstring_length (s)] = '/';
      if (!do_completion (d, igcase))
        destruct_string (d);
    }
}

void
completion::fix_match_len ()
{
  if (c_item == Qnil || !c_word || c_match_len <= c_target_len)
    return;

  const Char *p = xstring_contents (c_item) + c_target_len;
  const Char *pe = xstring_contents (c_item) + c_match_len;

  if (p < pe)
    {
      word_state ws (xsyntax_table (selected_buffer ()->lsyntax_table), *p);
      for (; p < pe && ws.forward (*p) != word_state::not_inword; p++)
        ;
    }

  c_match_len = min (c_match_len, p - xstring_contents (c_item));
}

void
completion::adjust_prefix (lisp prefix)
{
  int l = xstring_length (prefix) + c_match_len;
  Char *b = (Char *)alloca (sizeof (Char) * l);
  bcopy (xstring_contents (prefix), b, xstring_length (prefix));
  if (stringp (c_item))
    bcopy (xstring_contents (c_item), b + xstring_length (prefix), c_match_len);
  if (l == xstring_length (c_string) && !bcmp (b, xstring_contents (c_string), l))
    c_result = c_string;
  else
    c_result = make_string (b, l);
}

void
completion::complete_symbol (lisp vec)
{
  for (lisp *v = xvector_contents (vec), *ve = v + xvector_length (vec); v < ve; v++)
    for (lisp p = *v; consp (p); p = xcdr (p))
      {
        lisp symbol = xcar (p);
        if (c_type == Kfunction_name)
          {
            if (void_function_p (symbol))
              continue;
          }
        else if (c_type == Kcommand_name)
          {
            if (Fcommandp (symbol) == Qnil)
              continue;
          }
        else if (c_type == Kvariable_name)
          {
            if (xsymbol_value (symbol) == Qunbound)
              continue;
          }
        else if (c_type == Knon_trivial_symbol_name)
          {
            if (void_function_p (symbol)
                && xsymbol_value (symbol) == Qunbound
                && xsymbol_plist (symbol) == Qnil)
              continue;
          }
        do_completion (xsymbol_name (symbol), 0);
      }
}

void
completion::complete_symbol ()
{
  lisp package = coerce_to_package (0);

  lisp lpkg = symbol_value (Vbuffer_package, selected_buffer ());
  if (stringp (lpkg))
    {
      lpkg = Ffind_package (lpkg);
      if (lpkg != Qnil)
        package = lpkg;
    }

  Char *b = xstring_contents (c_target);
  int l = xstring_length (c_target);

  maybe_symbol_string mss (package);
  mss.parse (b, l);
  package = mss.current_package ();

  if (mss.pkg_end ())
    {
      set_prefix (make_string (xstring_contents (c_target),
                               b - xstring_contents (c_target)));
      set_target (make_string (b, (xstring_contents (c_target)
                                   + xstring_length (c_target) - b)));
    }

  if (!mss.pkg_end () || b - mss.pkg_end () == 2)
    complete_symbol (xpackage_internal (package));
  complete_symbol (xpackage_external (package));

  if (!mss.pkg_end ())
    for (lisp p = xpackage_use_list (package); consp (p); p = xcdr (p))
      {
        package = xcar (p);
        if (packagep (package))
          complete_symbol (xpackage_external (package));
      }

  // パッケージ名の補完
  if (!mss.pkg_end ())
    for (lisp p = xsymbol_value (Vpackage_list); consp (p); p = xcdr (p))
      {
        lisp x = xcar (p);
        // なにも export していないパッケージは補完候補に出さない
        if (count_symbols (xpackage_external (x)) <= 0)
          continue;
        do_completion (xpackage_name (x), 0);
        for (lisp q = xpackage_nicknames (x); consp (q); q = xcdr (q))
          do_completion (xcar (q), 0);
      }

  fix_match_len ();
  if (mss.pkg_end ())
    adjust_prefix (c_prefix);
}

void
completion::complete_buffer_name ()
{
  int int_ok = c_target_len >= 1 && *xstring_contents (c_target) == ' ';
  for (Buffer *bp = Buffer::b_blist; bp; bp = bp->b_next)
    if (int_ok || !bp->internal_buffer_p ())
      {
        lisp name = Fbuffer_name (bp->lbp);
        if (!do_completion (name, 0) && name != bp->lbuffer_name)
          destruct_string (name);
      }
  fix_match_len ();
  c_force_no_match = int_ok;
}

int
completion::complete_filename (const char *path, lisp show_dots, lisp ignores)
{
  int ignored = 0;

  WIN32_FIND_DATA *fd = (WIN32_FIND_DATA *)alloca (sizeof *fd + 2);
  HANDLE h = WINFS::FindFirstFile (path, fd);
  if (h == INVALID_HANDLE_VALUE)
    {
      int e = GetLastError ();
      if (e != ERROR_FILE_NOT_FOUND)
        file_error (e, c_string);
      return 0;
    }

  find_handle fh (h);
  do
    {
#ifndef PATHNAME_ESCAPE_TILDE
      if (*fd->cFileName == '~' && !fd->cFileName[1])
        continue;
#endif
      if (show_dots == Qnil && *fd->cFileName == '.')
        continue;
      if (fd->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
        strcat (fd->cFileName, "/");
      else if (c_type == Kdirectory_name)
        continue;

      lisp name = make_string (fd->cFileName);
      for (lisp p = ignores; consp (p); p = xcdr (p))
        {
          lisp ext = xcar (p);
          if (stringp (ext)
              && xstring_length (name) > xstring_length (ext)
              && string_equalp (name, xstring_length (name) - xstring_length (ext),
                                ext, 0, xstring_length (ext)))
            {
              destruct_string (name);
              ignored = 1;
              goto ignore;
            }
        }
      if (!do_completion (name, 1))
        destruct_string (name);
    ignore:
      ;
    }
  while (WINFS::FindNextFile (h, fd));
  return ignored;
}

lisp
completion::split_pathname ()
{
  const Char *p0 = xstring_contents (c_target);
  const Char *pe = p0 + xstring_length (c_target);
  const Char *p;
  for (p = pe;
       p > p0 && p[-1] != ':' && p[-1] != '/' && p[-1] != '\\';
       p--)
    ;
  set_target (make_string (p, pe - p));

  pe = p;
  if (pe - p0 >= 2)
    {
      p = p0;
      if ((*p == '/' || *p == '\\')
          && (p[1] == '/' || p[1] == '\\'))
        {
          int n = 0;
          for (p += 2; p < pe; p++)
            if ((*p == '/' || *p == '\\') && ++n == 2)
              break;
          if (n < 2)
            return make_string (p0, pe - p0);
        }
    }

  if (!c_target_len)
    {
      lisp x = Fnamestring (make_string (p0, pe - p0));
      if (xstring_length (x)
          && xstring_contents (x)[xstring_length (x) - 1] != '/')
        {
          Char *b = (Char *)xmalloc ((xstring_length (x) + 1) * sizeof (Char));
          bcopy (xstring_contents (x), b, xstring_length (x));
          b[xstring_length (x)++] = '/';
          xfree (xstring_contents (x));
          xstring_contents (x) = b;
        }
      return x;
    }

  return Fdirectory_namestring (make_string (p0, pe - p0));
}

int
completion::complete_UNC (lisp &directory)
{
  const Char *p0 = xstring_contents (directory);
  const Char *pe = p0 + xstring_length (directory);
  int l = pe - p0;
  if (l < 2 || *p0 != '/' || p0[1] != '/')
    return 0;
  const Char *p;
  for (p = p0 + 2; p < pe && *p != '/'; p++)
    ;
  for (; pe > p && pe[-1] == '/'; pe--)
    ;
  if (p != pe)
    return 0;
  suppress_gc sgc;
  if (p == p0 + 2)
    {
      directory = make_string (p0, 2);
      for (lisp r = Flist_servers (Qnil); consp (r); r = xcdr (r))
        complete_with_slash (xcar (r), 1);
    }
  else
    {
      directory = make_string (p0, pe - p0 + 1);
      p0 += 2;
      for (lisp r = Flist_server_resources (make_string (p0, pe - p0), Qnil);
           consp (r); r = xcdr (r))
        complete_with_slash (xcar (r), 1);
    }
  return 1;
}

void
completion::complete_filename ()
{
  Buffer *bp = selected_buffer ();

  lisp show_dots = symbol_value (Vshow_dots, bp);
  if (show_dots == Qunbound)
    show_dots = Qnil;

  lisp ignores = symbol_value (Vignored_extensions, bp);
  if (ignores == Qunbound)
    ignores = Qnil;

  lisp directory = split_pathname ();
  if (!xstring_length (directory))
    directory = bp->ldirectory;
  set_prefix (directory);
  if (xstring_length (c_target))
    show_dots = Qt;

  if (!complete_UNC (directory))
    {
      char *path = (char *)alloca (2 * xstring_length (directory) + 10);
      w2s (path, directory);
      map_sl_to_backsl (path);
      strcat (path, "*");

      if (complete_filename (path, show_dots, ignores) && c_item == Qnil)
        complete_filename (path, show_dots, Qnil);
    }

  fix_match_len ();
  adjust_prefix (directory);
}

void
completion::complete_char_encoding ()
{
  for (lisp p = xsymbol_value (Vchar_encoding_list); consp (p); p = xcdr (p))
    {
      lisp encoding = xcar (p);
      if (char_encoding_p (encoding)
          && (c_type == Kchar_encoding
              || xchar_encoding_type (encoding) != encoding_auto_detect))
        do_completion (xchar_encoding_name (encoding), 0);
    }
  fix_match_len ();
}

void
completion::complete_list (lisp list, int igcase)
{
  for (; consp (list); list = xcdr (list))
    {
      lisp x = xcar (list);
      if (consp (x))
        x = xcar (x);
      if (stringp (x))
        do_completion (x, igcase);
      else if (symbolp (x))
        do_completion (xsymbol_name (x), igcase);
    }
  fix_match_len ();
}

lisp
completion::result () const
{
  multiple_value::count () = 2;
  multiple_value::value (1) = Qnil;

  if (c_no_completions)
    return Kno_completions;

  if (c_item == Qnil)
    return Kno_match;

  multiple_value::count () = 3;
  multiple_value::value (1) = c_matches_list;
  multiple_value::value (2) = c_prefix;
  if (c_target_len && c_match_len == c_target_len && c_strict_match)
    {
      if (xlist_length (c_matches_list) == 1)
        return Ksolo_match;
      return Knot_unique;
    }

  if (c_force_no_match)
    return Kno_match;

  if (c_result)
    return c_result;

  if (c_match_len == c_target_len)
    return c_string;

  return make_string (xstring_contents (c_item), c_match_len);
}

lisp
Fdo_completion (lisp string, lisp type, lisp word, lisp list)
{
  check_string (string);

  completion cmplt (type, string, word && word != Qnil);

  if (type == Ksymbol_name || type == Kfunction_name
      || type == Kcommand_name || type == Kvariable_name
      || type == Knon_trivial_symbol_name)
    cmplt.complete_symbol ();
  else if (type == Kexist_file_name || type == Kfile_name
           || type == Kfile_name_list || type == Kdirectory_name)
    cmplt.complete_filename ();
  else if (type == Kbuffer_name || type == Kexist_buffer_name)
    cmplt.complete_buffer_name ();
  else if (type == Kchar_encoding || type == Kexact_char_encoding)
    cmplt.complete_char_encoding ();
  else if (type == Klist)
    cmplt.complete_list (list ? list : Qnil, 0);
  else if (type == Klist_ignore_case)
    cmplt.complete_list (list ? list : Qnil, 1);
  else
    {
      multiple_value::count () = 2;
      multiple_value::value (1) = Qnil;
      return Qnil;
    }

  return cmplt.result ();
}

lisp
Fminibuffer_completion_type (lisp buffer)
{
  return Buffer::coerce_to_buffer (buffer)->lcomplete_type;
}

lisp
Fminibuffer_completion_list (lisp buffer)
{
  return Buffer::coerce_to_buffer (buffer)->lcomplete_list;
}

lisp
Fminibuffer_buffer (lisp buffer)
{
  return Buffer::coerce_to_buffer (buffer)->lminibuffer_buffer;
}

lisp
Fminibuffer_dialog_title (lisp buffer)
{
  return Buffer::coerce_to_buffer (buffer)->ldialog_title;
}

lisp
Fminibuffer_default (lisp buffer)
{
  return Buffer::coerce_to_buffer (buffer)->lminibuffer_default;
}

static lisp
complete_read (lisp prompt, lisp def, lisp type, lisp compl,
               lisp history, int must_match, lisp keys)
{
  check_string (prompt);
  lisp x = find_keyword (Khistory, keys);
  if (x != Qnil)
    history = x;
  return complete_read (xstring_contents (prompt), xstring_length (prompt),
                        def, type, compl, history, must_match, -1);
}

lisp
Fread_string (lisp prompt, lisp keys)
{
  check_string (prompt);
  return read_minibuffer (xstring_contents (prompt), xstring_length (prompt),
                          find_keyword (Kdefault, keys), Qnil, Qnil,
                          find_keyword (Khistory, keys), 0, 0, 0, Qnil, -1);
}

lisp
Fread_function_name (lisp prompt, lisp keys)
{
  return complete_read (prompt, find_keyword (Kdefault, keys),
                        Kfunction_name, Qnil, Ksymbol_name, 1, keys);
}

lisp
Fread_command_name (lisp prompt, lisp keys)
{
  return complete_read (prompt, find_keyword (Kdefault, keys),
                        Kcommand_name, Qnil, Ksymbol_name, 1, keys);
}

lisp
Fread_symbol_name (lisp prompt, lisp keys)
{
  return complete_read (prompt, find_keyword (Kdefault, keys),
                        Ksymbol_name, Qnil, Ksymbol_name, 1, keys);
}

lisp
Fread_variable_name (lisp prompt, lisp keys)
{
  return complete_read (prompt, find_keyword (Kdefault, keys),
                        Kvariable_name, Qnil, Ksymbol_name, 1, keys);
}

static lisp
read_filename (lisp prompt, lisp keys, lisp type)
{
  check_string (prompt);
  return read_filename (xstring_contents (prompt), xstring_length (prompt),
                        type, find_keyword (Ktitle, keys),
                        find_keyword (Kdefault, keys),
                        find_keyword (Khistory, keys));
}

lisp
Fread_file_name (lisp prompt, lisp keys)
{
  return read_filename (prompt, keys, Kfile_name);
}

lisp
Fread_file_name_list (lisp prompt, lisp keys)
{
  return read_filename (prompt, keys, Kfile_name_list);
}

lisp
Fread_exist_file_name (lisp prompt, lisp keys)
{
  return read_filename (prompt, keys, Kexist_file_name);
}

lisp
Fread_directory_name (lisp prompt, lisp keys)
{
  return read_filename (prompt, keys, Kdirectory_name);
}

lisp
Fread_buffer_name (lisp prompt, lisp keys)
{
  lisp def = find_keyword (Kdefault, keys);
  if (def == Qnil)
    def = Fother_buffer (0);
  if (bufferp (def))
    def = Fbuffer_name (def);
  return complete_read (prompt, def, Kbuffer_name, Qnil, Kbuffer_name, 0, keys);
}

lisp
Fread_exist_buffer_name (lisp prompt, lisp keys)
{
  lisp def = find_keyword (Kdefault, keys);
  if (def == Qnil)
    def = Fselected_buffer ();
  if (bufferp (def))
    def = Fbuffer_name (def);
  return complete_read (prompt, def, Kexist_buffer_name, Qnil, Kbuffer_name, 1, keys);
}

lisp
Fread_integer (lisp prompt, lisp)
{
  check_string (prompt);
  return minibuffer_read_integer (xstring_contents (prompt), xstring_length (prompt));
}

lisp
Fread_sexp (lisp prompt, lisp)
{
  check_string (prompt);
  return funcall_1 (Vread_from_string,
                    read_minibuffer (xstring_contents (prompt), xstring_length (prompt),
                                     Qnil, Klisp_sexp, Qnil, Klisp_sexp, 0, 0, 0, Qnil, -1));
}

lisp
Fread_char_encoding (lisp prompt, lisp keys)
{
  check_string (prompt);
  return complete_read (prompt, Qnil, Kchar_encoding,
                        Qnil, Kchar_encoding, 1, keys);
}

lisp
Fread_exact_char_encoding (lisp prompt, lisp keys)
{
  check_string (prompt);
  return complete_read (prompt, Qnil, Kexact_char_encoding,
                        Qnil, Kchar_encoding, 1, keys);
}

lisp
Fcompleting_read (lisp prompt, lisp compl, lisp keys)
{
  return complete_read (prompt,
                        find_keyword (Kdefault, keys),
                        find_keyword_bool (Kcase_fold, keys) ? Klist_ignore_case : Klist,
                        compl, Qnil,
                        find_keyword_bool (Kmust_match, keys),
                        keys);
}
