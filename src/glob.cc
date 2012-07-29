#include "stdafx.h"
#include "ed.h"
#include "glob.h"

int
file_masks::match (const char *name) const
{
  if (empty_p ())
    return 0;
  int not = 0, match = 0;
  for (char **p = fm_masks; *p; p++)
    if (**p == GLOB_NOT)
      {
        match |= pathname_match_p (*p + 1, name);
        not = 1;
      }
    else if (pathname_match_p (*p, name))
      return 1;
  return not ? !match : 0;
}

char **
file_masks::build_masks (lisp lmasks)
{
  int nfiles = 0;
  int nbytes = 0;
  for (lisp p = lmasks; consp (p); p = xcdr (p))
    {
      lisp x = xcar (p);
      check_string (x);
      if (xstring_length (x))
        {
          nbytes += w2sl (x) + 1;
          nfiles++;
        }
    }

  if (!nfiles)
    return 0;

  nfiles++;
  char **b0 = (char **)xmalloc (sizeof (char *) * nfiles + nbytes);
  char **b = b0;
  char *s = (char *)b0 + sizeof (char *) * nfiles;
  for (lisp p = lmasks; consp (p); p = xcdr (p))
    {
      lisp x = xcar (p);
      if (xstring_length (x))
        {
          *b++ = s;
          s = w2s (s, x) + 1;
        }
    }
  *b = 0;
  return b0;
}

void
file_masks::set_text (HWND hwnd) const
{
  if (empty_p ())
    SetWindowText (hwnd, "");
  else
    {
      int nbytes = 16;
      for (char **p = fm_masks; *p; p++)
        nbytes += strlen (*p) + 1;

      char *b0 = (char *)alloca (nbytes);
      char *b = stpcpy (b0, "Mask:");
      for (char **p = fm_masks; *p; p++)
        {
          *b++ = ' ';
          b = stpcpy (b, *p);
        }
      SetWindowText (hwnd, b0);
    }
}

static const u_char *
find_matched_bracket (const u_char *s)
{
  if (*s == '^')
    s++;
  if (*s == ']')
    s++;
  while (1)
    {
      int c = *s++;
      switch (c)
        {
        case 0:
          return 0;

        case ']':
          return s - 1;

        default:
          if (SJISP (c) && *s)
            s++;
          break;
        }
    }
}

int
wild_pathname_p (const char *filename)
{
  const u_char *p = (const u_char *)filename;
  int unmatched_bracket = xsymbol_value (Vbrackets_is_wildcard_character) == Qnil;

  if (*p == GLOB_NOT)
    return 1;

  while (1)
    {
      int c = *p++;
      switch (c)
        {
        case 0:
          return 0;

        case '[':
          if (!unmatched_bracket && find_matched_bracket (p))
            return 1;
          unmatched_bracket = 1;
          break;

        case '*':
        case '?':
          return 1;

        default:
          if (SJISP (c) && *p)
            p++;
          break;
        }
    }
}

int
pathname_match_p1 (const char *pat, const char *str, int nodot)
{
  const u_char *p = (const u_char *)pat;
  const u_char *s = (const u_char *)str;
  int unmatched_bracket = xsymbol_value (Vbrackets_is_wildcard_character) == Qnil;

  while (1)
    {
      int c = *p++;
      switch (c)
        {
        case 0:
          return !*s;

        case '[':
          {
            if (unmatched_bracket)
              goto normal;
            const u_char *pe = find_matched_bracket (p);
            if (!pe)
              {
                unmatched_bracket = 1;
                goto normal;
              }
            if (!*s)
              return 0;
            int not = 0;
            if (*p == '^')
              {
                not = 1;
                p++;
              }

            while (p < pe)
              {
                c = *p++;
                if (SJISP (c) && *p)
                  {
                    if (p[1] == '-' && p + 3 < pe && SJISP (p[2]))
                      {
                        u_int x = (*s << 8) + s[1];
                        if (x >= u_int ((c << 8) + *p)
                            && x <= u_int ((p[2] << 8) + p[3]))
                          {
                            not ^= 1;
                            break;
                          }
                        p += 4;
                      }
                    else
                      {
                        if (u_int (c) == *s && *p == s[1])
                          {
                            not ^= 1;
                            break;
                          }
                        p++;
                      }
                  }
                else
                  {
                    if (*p == '-' && p + 1 < pe && !SJISP (p[1]))
                      {
                        int x = char_upcase (*s);
                        if (x >= char_upcase (c) && x <= char_upcase (p[1]))
                          {
                            not ^= 1;
                            break;
                          }
                        p += 2;
                      }
                    else if (char_upcase (c) == char_upcase (*s))
                      {
                        not ^= 1;
                        break;
                      }
                  }
              }
            if (!not)
              return 0;
            p = pe + 1;
            s += (SJISP (*s) && s[1]) ? 2 : 1;
            break;
          }

        case '?':
          if (!*s)
            return 0;
          if (nodot && *s == '.')
            return 0;
          if (SJISP (*s) && s[1])
            s++;
          s++;
          break;

        case '*':
          while (*p == '*')
            p++;
          if (!*p)
            return 1;
          while (1)
            {
              if (pathname_match_p1 ((const char *)p, (const char *)s, nodot))
                return 1;
              if (!*s)
                return 0;
              if (nodot && *s == '.')
                return 0;
              if (SJISP (*s) && s[1])
                s++;
              s++;
            }
          /* NOTREACHED */

        case '.':
          if (*s == '.')
            s++;
          else
            return !*p && !*s;
          break;

        default:
        normal:
          if (SJISP (c) && *p)
            {
              if (u_int (c) != *s++)
                return 0;
              if (*p++ != *s++)
                return 0;
            }
          else
            {
              if (char_upcase (c) != char_upcase (*s))
                return 0;
              s++;
            }
        }
    }
}

int
pathname_match_p (const char *pat, const char *str)
{
  int nodot = 0;
  int l = strlen (pat);
  if (l > 1 && pat[l - 1] == '.' && !check_kanji2 (pat, l - 1))
    nodot = 1;
  return pathname_match_p1 (pat, str, nodot);
}

#define DF_ABSOLUTE 1
#define DF_RECURSIVE 2
#define DF_FILE_ONLY 4
#define DF_SHOW_DOTS 8
#define DF_COUNT 16
#define DF_DIR_ONLY 32
#define DF_FILE_INFO 64

static lisp
directory (char *path, const char *pat, char *name, file_masks &masks, int flags,
           int depth, int max_depth, long &count, lisp callback, lisp test, lisp result)
{
  QUIT;

  if (max_depth && depth >= max_depth)
    return result;

  int l = strlen (path);
  if (l >= PATH_MAX)
    return result;

  char *pe = path + l;
  *pe = '*';
  pe[1] = 0;

  char *ne = name + strlen (name);

  WIN32_FIND_DATA fd;

  HANDLE h = WINFS::FindFirstFile (path, &fd);
  if (h != INVALID_HANDLE_VALUE)
    {
      find_handle fh (h);
      do
        {
#ifndef PATHNAME_ESCAPE_TILDE
          if (*fd.cFileName == '~' && !fd.cFileName[1])
            continue;
#endif
          bool test_called = false;
          if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
            {
              if (*fd.cFileName == '.'
                  && (!fd.cFileName[1]
                      || (fd.cFileName[1] == '.' && !fd.cFileName[2])))
                {
                  if (!(flags & DF_SHOW_DOTS))
                    continue;
                }
              else if (*pat == GLOB_NOT
                       ? pathname_match_p (pat + 1, fd.cFileName)
                       : (*pat && !pathname_match_p (pat, fd.cFileName)))
                continue;
              else if (flags & DF_RECURSIVE)
                {
                  if (!(flags & DF_ABSOLUTE))
                    strcpy (stpcpy (ne, fd.cFileName), "/");
                  strcpy (stpcpy (pe, fd.cFileName), "/");
                  if (test != Qnil)
                    {
                      lisp lpath = make_string ((flags & DF_ABSOLUTE) ? path : name);
                      if (flags & DF_FILE_INFO)
                        lpath = xcons (lpath, make_file_info (fd));
                      test_called = true;
                      if (funcall_1 (test, lpath) == Qnil)
                        continue;
                    }
                  result = directory (path, "", name, masks, flags,
                                      depth + 1, max_depth, count, callback, test, result);
                  if (flags & DF_COUNT && count <= 0)
                    break;
                }
              if (flags & DF_FILE_ONLY)
                continue;
              if (!masks.empty_p () && !masks.match (fd.cFileName))
                continue;
              if (flags & DF_ABSOLUTE)
                strcpy (stpcpy (pe, fd.cFileName), "/");
              else
                strcpy (stpcpy (ne, fd.cFileName), "/");
            }
          else
            {
              if (flags & DF_DIR_ONLY)
                continue;
              if (*pat)
                {
                  if (*pat == GLOB_NOT
                      ? pathname_match_p (pat + 1, fd.cFileName)
                      : !pathname_match_p (pat, fd.cFileName))
                    continue;
                }
              else
                {
                  if (!masks.empty_p () && !masks.match (fd.cFileName))
                    continue;
                }
              if (flags & DF_ABSOLUTE)
                strcpy (pe, fd.cFileName);
              else
                strcpy (ne, fd.cFileName);
            }
          lisp lpath = make_string ((flags & DF_ABSOLUTE) ? path : name);
          if (flags & DF_FILE_INFO)
            lpath = xcons (lpath, make_file_info (fd));
          if (test != Qnil && !test_called)
            {
              if (funcall_1 (test, lpath) == Qnil)
                continue;
            }

          if (callback != Qnil)
            {
              lisp arg = xcons (lpath, Qnil);
              protect_gc gcpro (arg);
              Ffuncall (callback, arg);
            }
          else
            result = xcons (lpath, result);
          if (flags & DF_COUNT && --count <= 0)
            break;
        }
      while (WINFS::FindNextFile (h, &fd));
    }
  return result;
}

lisp
Fdirectory (lisp dirname, lisp keys)
{
  char path[PATH_MAX * 2];
  char pat[PATH_MAX + 1];
  pathname2cstr (dirname, path);
  char *p = jrindex (path, '/');
  int f = WINFS::GetFileAttributes (path);
  if (f != -1 && f & FILE_ATTRIBUTE_DIRECTORY)
    {
      if (p && p[1])
        strcat (path, "/");
      *pat = 0;
    }
  else
    {
      if (p)
        {
          strcpy (pat, p + 1);
          p[1] = 0;
        }
      else
        {
          strcpy (pat, path);
          *path = 0;
        }
    }

  lisp wild = find_keyword (Kwild, keys, Qnil);
  file_masks masks (stringp (wild) ? xcons (wild, Qnil) : wild);

  char name[PATH_MAX * 2];
  *name = 0;

  int flags = 0;
  int max_depth = 0;
  long count = 0;
  if (find_keyword (Kabsolute, keys, Qnil) != Qnil)
    flags |= DF_ABSOLUTE;
  if (find_keyword (Krecursive, keys, Qnil) != Qnil)
    {
      flags |= DF_RECURSIVE;
      lisp x = find_keyword (Kdepth, keys, Qnil);
      if (x != Qnil)
        max_depth = fixnum_value (x);
    }
  if (find_keyword (Kfile_only, keys, Qnil) != Qnil)
    flags |= DF_FILE_ONLY;
  if (find_keyword (Kshow_dots, keys, Qnil) != Qnil)
    flags |= DF_SHOW_DOTS;
  lisp lcount = find_keyword (Kcount, keys, Qnil);
  if (lcount != Qnil)
    {
      flags |= DF_COUNT;
      count = fixnum_value (lcount);
      if (count <= 0)
        return Qnil;
    }
  else if (find_keyword (Kany_one, keys, Qnil) != Qnil) // for compatibility
    {
      flags |= DF_COUNT;
      count = 1;
    }
  if (find_keyword (Kdirectory_only, keys, Qnil) != Qnil)
    flags |= DF_DIR_ONLY;
  if (find_keyword (Kfile_info, keys, Qnil) != Qnil)
    flags |= DF_FILE_INFO;
  lisp callback = find_keyword (Kcallback, keys, Qnil);
  lisp test = find_keyword (Ktest, keys, Qnil);
  return Fnreverse (directory (path, pat, name, masks, flags,
                               0, max_depth, count, callback, test, Qnil));
}

lisp
Fpathname_match_p (lisp pathname, lisp wildname)
{
  check_string (pathname);
  check_string (wildname);
  char *path = (char *)alloca (xstring_length (pathname) * 2 + 1);
  w2s (path, pathname);
  char *wild = (char *)alloca (xstring_length (wildname) * 2 + 1);
  w2s (wild, wildname);
  return boole (*wild == GLOB_NOT
                ? !pathname_match_p (wild + 1, path)
                : pathname_match_p (wild, path));
}

lisp
Fwild_pathname_p (lisp pathname)
{
  char path[PATH_MAX + 1];
  pathname2cstr (pathname, path);
  return boole (wild_pathname_p (path));
}
