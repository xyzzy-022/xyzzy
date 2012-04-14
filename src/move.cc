#include "stdafx.h"
#include "ed.h"

point_t
Buffer::coerce_to_point (lisp x) const
{
  if (short_int_p (x))
    return xshort_int_value (x);
  if (long_int_p (x))
    return xlong_int_value (x);
  if (markerp (x))
    {
      valid_marker_p (x);
      if (xmarker_point (x) == NO_MARK_SET)
        FEsimple_error (Emarker_is_not_set);
      return xmarker_point (x);
    }
  FEtype_error (x, xsymbol_value (Qor_integer_marker));
  return 0;
}

point_t
Buffer::coerce_to_restricted_point (lisp x) const
{
  point_t p = coerce_to_point (x);
  return min (max (p, b_contents.p1), b_contents.p2);
}

void
Buffer::check_range (Point &point) const
{
  if (point.p_point < b_contents.p1)
    goto_char (point, b_contents.p1);
  else if (point.p_point > b_contents.p2)
    goto_char (point, b_contents.p2);
}

int
Buffer::forward_char (Point &point, long nchars) const
{
  long d = min (max (point.p_point + nchars, b_contents.p1),
                b_contents.p2) - point.p_point;
  int f = d == nchars;
  const Chunk *cp = point.p_chunk;

  if (d > 0)
    {
      while (1)
        {
          int size = cp->c_used - point.p_offset;
          if (d <= size)
            {
              point.p_point += d;
              if (d == size && cp->c_next)
                {
                  cp = cp->c_next;
                  point.p_offset = 0;
                }
              else
                point.p_offset += d;
              point.p_chunk = (Chunk *)cp;
              break;
            }
          d -= size;
          point.p_point += size;
          point.p_offset = 0;
          cp = cp->c_next;
          assert (cp);
        }
    }
  else if (d < 0)
    {
      while (1)
        {
          if (point.p_offset + d >= 0)
            {
              point.p_offset += d;
              point.p_point += d;
              point.p_chunk = (Chunk *)cp;
              break;
            }
          d += point.p_offset + 1;
          point.p_point -= point.p_offset + 1;
          cp = cp->c_prev;
          assert (cp);
          point.p_offset = cp->c_used - 1;
        }
    }
  return f;
}

void
Buffer::goto_char (Point &point, point_t goal) const
{
  goal = min (max (goal, b_contents.p1), b_contents.p2);
  if (goal < point.p_point / 2)
    {
      point.p_point = 0;
      point.p_chunk = b_chunkb;
      point.p_offset = 0;
    }
  else if (goal > (point.p_point + b_nchars) / 2)
    {
      point.p_point = b_nchars;
      point.p_chunk = b_chunke;
      point.p_offset = b_chunke->c_used;
    }
  forward_char (point, goal - point.p_point);
}

int
Chunk::count_lines () const
{
  int nlines = 0;
  for (const Char *p = c_text, *pe = p + c_used; p < pe; p++)
    if (*p == '\n')
      nlines++;
  return nlines;
}

long
Buffer::count_lines ()
{
  if (b_nlines == -1)
    {
      long nlines = 1;
      for (Chunk *cp = b_chunkb; cp; cp = cp->c_next)
        {
          if (cp->c_nlines == -1)
            cp->c_nlines = cp->count_lines ();
          nlines += cp->c_nlines;
        }
      b_nlines = nlines;
    }
  return b_nlines;
}

long
Buffer::point_linenum (point_t goal) const
{
  long linenum = 1;
  point_t point = 0;
  const Chunk *cp;
  for (cp = b_chunkb; point + cp->c_used < goal; cp = cp->c_next)
    {
      if (cp->c_nlines == -1)
        ((Chunk *)cp)->c_nlines = cp->count_lines ();
      linenum += cp->c_nlines;
      point += cp->c_used;
    }
  for (const Char *p = cp->c_text, *pe = p + goal - point; p < pe; p++)
    if (*p == '\n')
      linenum++;
  return linenum;
}

long
Buffer::linenum_point (Point &pbuf, long goal)
{
  assert (goal >= 1);
  if (goal == 1)
    {
      pbuf.p_point = 0;
      pbuf.p_chunk = b_chunkb;
      pbuf.p_offset = 0;
      return 1;
    }

  Chunk *cp = b_chunkb;
  point_t point = 0;
  long linenum = 1;

  while (1)
    {
      if (cp->c_nlines == -1)
        {
          long olinenum = linenum;
          for (const Char *p = cp->c_text, *pe = p + cp->c_used; p < pe;)
            if (*p++ == '\n' && ++linenum == goal)
              {
                pbuf.p_offset = p - cp->c_text;
                pbuf.p_point = point + pbuf.p_offset;
                if (pbuf.p_offset == cp->c_used && cp->c_next)
                  {
                    pbuf.p_offset = 0;
                    cp = cp->c_next;
                  }
                pbuf.p_chunk = cp;
                return linenum;
              }
          cp->c_nlines = short (linenum - olinenum);
          if (!cp->c_next)
            {
              linenum = olinenum;
              continue;
            }
        }
      else
        {
          long ln = linenum + cp->c_nlines;
          if (ln >= goal || !cp->c_next)
            {
              if (!cp->c_nlines)
                {
                  do
                    {
                      cp = cp->c_prev;
                      if (!cp)
                        {
                          pbuf.p_point = 0;
                          pbuf.p_chunk = b_chunkb;
                          pbuf.p_offset = 0;
                          return 1;
                        }
                      point -= cp->c_used;
                    }
                  while (!cp->c_nlines);
                  linenum -= cp->c_nlines;
                }

              int d = min (goal - linenum, long (cp->c_nlines));
              linenum += d;
              const Char *p = cp->c_text;
              if (d < cp->c_nlines / 2)
                {
                  while (*p++ != '\n' || --d)
                    ;
                }
              else
                {
                  const Char *p0 = p;
                  for (p += cp->c_used;
                       p > p0 && (p[-1] != '\n' || d++ != cp->c_nlines);
                       p--)
                    ;
                }
              pbuf.p_offset = p - cp->c_text;
              pbuf.p_point = point + pbuf.p_offset;
              if (pbuf.p_offset == cp->c_used && cp->c_next)
                {
                  pbuf.p_offset = 0;
                  cp = cp->c_next;
                }
              pbuf.p_chunk = cp;
              return linenum;
            }
          linenum = ln;
        }
      point += cp->c_used;
      cp = cp->c_next;
    }
}

void
Buffer::go_bol (Point &point) const
{
  Chunk *cp = point.p_chunk;
  while (1)
    {
      if (cp->c_nlines)
        {
          for (const Char *p0 = cp->c_text, *p = p0 + point.p_offset - 1;
               p >= p0; p--)
            if (*p == '\n')
              {
                int n = p - p0 + 1;
                point.p_point -= point.p_offset - n;
                if (n == cp->c_used && cp->c_next)
                  {
                    point.p_offset = 0;
                    point.p_chunk = cp->c_next;
                  }
                else
                  {
                    point.p_offset = n;
                    point.p_chunk = cp;
                  }
                return;
              }
          if (point.p_offset == cp->c_used)
            {
              assert (cp->c_nlines == -1 || !cp->c_nlines);
              cp->c_nlines = 0;
            }
        }
      if (!cp->c_prev)
        break;
      cp = cp->c_prev;
      point.p_point -= point.p_offset;
      point.p_offset = cp->c_used;
    }
  point.p_offset = 0;
  point.p_point = 0;
  point.p_chunk = cp;
}

void
Buffer::go_eol (Point &point) const
{
  Chunk *cp = point.p_chunk;
  while (1)
    {
      if (cp->c_nlines)
        {
          for (const Char *p = cp->c_text + point.p_offset, *pe = cp->c_text + cp->c_used;
               p < pe; p++)
            if (*p == '\n')
              {
                int n = p - cp->c_text;
                point.p_point += n - point.p_offset;
                point.p_offset = n;
                point.p_chunk = cp;
                return;
              }
          if (!point.p_offset)
            {
              assert (cp->c_nlines == -1 || !cp->c_nlines);
              cp->c_nlines = 0;
            }
        }
      if (!cp->c_next)
        break;
      point.p_point += cp->c_used - point.p_offset;
      point.p_offset = 0;
      cp = cp->c_next;
    }
  point.p_point += cp->c_used - point.p_offset;
  assert (point.p_point == b_nchars);
  point.p_offset = cp->c_used;
  point.p_chunk = cp;
}

int
Buffer::line_backward (Point &point, long req) const
{
  assert (req > 0);
  req++;
  long nlines = req;
  Point opoint (point);
  Chunk *cp = point.p_chunk;
  while (1)
    {
      if (cp->c_nlines)
        {
          if (point.p_offset == cp->c_used && cp->c_nlines > 0 && nlines > cp->c_nlines)
            nlines -= cp->c_nlines;
          else
            {
              int l = 0;
              int n = min (long (point.p_offset), point.p_point - b_contents.p1);
              const Char *p = cp->c_text + point.p_offset;
              const Char *p0 = p - n;
              while (p > p0)
                if (*--p == '\n')
                  {
                    l++;
                    if (!--nlines)
                      {
                        n = p - cp->c_text;
                        point.p_point -= point.p_offset - n;
                        point.p_offset = n;
                        point.p_chunk = cp;
                        forward_char (point, 1);
                        return 1 - req;
                      }
                  }
              if (point.p_offset == cp->c_used && cp->c_used == n)
                {
                  assert (cp->c_nlines == -1 || cp->c_nlines == l);
                  cp->c_nlines = l;
                }
            }
        }
      if (point.p_point - point.p_offset <= b_contents.p1)
        break;
      point.p_point -= point.p_offset;
      cp = cp->c_prev;
      assert (cp);
      point.p_offset = cp->c_used;
    }

  if (nlines == req)
    {
      point = opoint;
      return 0;
    }
  else
    {
      if (point.p_offset == cp->c_used && cp->c_next)
        {
          point.p_chunk = cp->c_next;
          point.p_offset = 0;
        }
      else
        point.p_chunk = cp;
      goto_char (point, b_contents.p1);
      return nlines - req;
    }
}

int
Buffer::line_forward (Point &point, long req) const
{
  assert (req > 0);
  Point opoint (point);
  long nlines = req;
  Chunk *cp = point.p_chunk;
  while (1)
    {
      if (cp->c_nlines)
        {
          if (!point.p_offset && cp->c_nlines > 0 && nlines > cp->c_nlines
              && (nlines != req || point.p_point + cp->c_used <= b_contents.p2))
            nlines -= cp->c_nlines;
          else
            {
              int n = min (long (cp->c_used - point.p_offset),
                           b_contents.p2 - point.p_point);
              int l = 0;
              const Char *p = cp->c_text + point.p_offset, *pe = p + n;
              while (p < pe)
                if (*p++ == '\n')
                  {
                    l++;
                    if (!--nlines)
                      {
                        n = p - cp->c_text;
                        point.p_point += n - point.p_offset;
                        if (n == cp->c_used && cp->c_next)
                          {
                            point.p_offset = 0;
                            point.p_chunk = cp->c_next;
                          }
                        else
                          {
                            point.p_offset = n;
                            point.p_chunk = cp;
                          }
                        return req;
                      }
                  }
              if (n == cp->c_used)
                {
                  assert (cp->c_nlines < 0 || cp->c_nlines == l);
                  cp->c_nlines = l;
                }
            }
        }
      if (point.p_point + cp->c_used - point.p_offset >= b_contents.p2)
        break;
      point.p_point += cp->c_used - point.p_offset;
      point.p_offset = 0;
      cp = cp->c_next;
      assert (cp);
    }

  if (nlines == req)
    {
      point = opoint;
      return 0;
    }
  else
    {
      point.p_chunk = cp;
      goto_char (point, b_contents.p2);
      goto_bol (point);
      assert (point.p_point > opoint.p_point);
      return req - nlines;
    }
}

void
Buffer::goto_bol (Point &point) const
{
  go_bol (point);
  if (point.p_point < b_contents.p1)
    goto_char (point, b_contents.p1);
}

void
Buffer::goto_eol (Point &point) const
{
  go_eol (point);
  if (point.p_point > b_contents.p2)
    goto_char (point, b_contents.p2);
}

int
Buffer::forward_line (Point &point, long nlines) const
{
  if (nlines > 0)
    return line_forward (point, nlines);
  if (nlines < 0)
    return line_backward (point, -nlines);
  return 0;
}

long
Buffer::point_column (const Point &point) const
{
  Point p (point);

  go_bol (p);

  long column = 0;
  while (p.p_point < point.p_point)
    {
      column += char_columns (p.p_chunk->c_text[p.p_offset++], column);
      p.p_point++;
      if (p.p_offset == p.p_chunk->c_used)
        {
          p.p_chunk = p.p_chunk->c_next;
          p.p_offset = 0;
        }
    }
  return column;
}

long
Buffer::forward_column (Point &point, long ncolumns, long curcol,
                        int can_exceed, int restrict) const
{
  Chunk *cp = point.p_chunk;
  point_t limit = restrict ? b_contents.p2 : b_nchars;

  do
    {
      if (point.p_point == limit)
        break;
      Char c = cp->c_text[point.p_offset];
      if (c == '\n')
        break;
      int ncol = char_columns (c, curcol);
      if (!can_exceed && curcol + ncol > ncolumns)
        break;
      curcol += ncol;
      point.p_point++;
      point.p_offset++;
      if (point.p_offset == cp->c_used)
        {
          if (!cp->c_next)
            break;
          cp = cp->c_next;
          point.p_offset = 0;
        }
    }
  while (curcol < ncolumns);
  point.p_chunk = cp;
  return curcol;
}

long
Buffer::goto_column (Point &point, long column, int exceed) const
{
  go_bol (point);
  long curcol = forward_column (point, column, 0, exceed, 1);
  if (point.p_point < b_contents.p1)
    {
      goto_char (point, b_contents.p1);
      curcol = point_column (point);
    }
  return curcol;
}

int
Window::scroll_window (long nlines, int abs)
{
  if (w_bufp->b_fold_columns == Buffer::FOLD_NONE)
    {
      long ol = w_bufp->point_linenum (w_disp);
      long goal = max (1L, abs ? nlines : ol + nlines);
      if (goal == ol)
        return 0;

      Point point;
      if (w_bufp->linenum_point (point, goal) != goal)
        return 0;

      w_disp = point.p_point;
      if (w_disp_flags & WDF_GOAL_COLUMN)
        w_goal_column = w_bufp->point_column (w_point);
      w_bufp->forward_line (w_point, goal - ol);
      w_bufp->forward_column (w_point, w_goal_column, 0, 0, 1);
      if (w_point.p_point < w_bufp->b_contents.p1)
        w_bufp->goto_char (w_point, w_bufp->b_contents.p1);
      w_disp_flags &= ~WDF_GOAL_COLUMN;
    }
  else
    {
      long ol = w_bufp->folded_point_linenum (w_disp);
      long goal = max (1L, abs ? nlines : ol + nlines);
      if (goal == ol)
        return 0;

      Point point;
      if (w_bufp->folded_linenum_point (point, goal) != goal)
        return 0;

      w_disp = point.p_point;
      if (w_disp_flags & WDF_GOAL_COLUMN)
        w_goal_column = w_bufp->folded_point_column (w_point);
      w_bufp->folded_forward_line (w_point, goal - ol);
      w_bufp->folded_forward_column (w_point, w_goal_column, 0, 0, 0);
      w_bufp->check_range (w_point);
      w_disp_flags &= ~WDF_GOAL_COLUMN;
    }
  return 1;
}

int
Window::scroll_window_horizontally (long ncolumns, int abs)
{
  long oc = w_top_column;
  long goal = max (0L, abs ? ncolumns : oc + ncolumns);
  if (goal == oc)
    return 0;
  long curcol;
  if (w_bufp->b_fold_columns == Buffer::FOLD_NONE)
    {
      w_top_column = goal;
      curcol = w_bufp->point_column (w_point);
    }
  else
    {
      w_top_column = min (int (goal), w_bufp->b_fold_columns - 1);
      curcol = w_bufp->folded_point_column (w_point);
    }

  if (curcol < w_top_column)
    {
      if (w_bufp->b_fold_columns == Buffer::FOLD_NONE)
        curcol = w_bufp->forward_column (w_point, w_top_column,
                                         curcol, 1, 1);
      else
        curcol = w_bufp->folded_forward_column (w_point, w_top_column,
                                                curcol, 1, 1);
      if (curcol < w_top_column)
        w_top_column = curcol;
    }
  else
    {
#if 0 /* とりあえずちゃんと計算できているようにしたつもりだが、どーだろ? */
      /* こんなところでこんなことをやるのはめちゃめちゃみっともないが、
         ウィンドウサイズが更新されないタイミングがあるみたいなのでしょうがない。
         つーか、このあたりの作りがたぶん間違っているのだが。*/
      RECT r;
      GetClientRect (w_hwnd, &r);
      int cx = max (0L, ((r.right - sysdep.edge.cx - app.text_font.cell ().cx / 2)
                         / app.text_font.cell ().cx));
#else
      int cx = w_ech.cx;
#endif
      if (flags () & WF_LINE_NUMBER)
        cx -= LINENUM_COLUMNS + 1;
      if (w_point.p_offset != w_point.p_chunk->c_used)
        {
          Char c = w_point.ch ();
          if (c != CC_LFD && c != CC_TAB && char_width (c) == 2)
            cx--;
        }
      if (cx <= 0)
        cx = 1;
      int w = w_top_column + cx;
      if (curcol >= w)
        {
          if (w_bufp->b_fold_columns == Buffer::FOLD_NONE)
            {
              w_bufp->go_bol (w_point);
              w_bufp->forward_column (w_point, w - 1, 0, 0, 1);
            }
          else
            {
              w_bufp->folded_go_bol (w_point);
              w_bufp->folded_forward_column (w_point, w - 1, 0, 0, 1);
            }
        }
    }
  if (w_point.p_point < w_bufp->b_contents.p1)
    w_bufp->goto_char (w_point, w_bufp->b_contents.p1);
  w_disp_flags |= WDF_GOAL_COLUMN;
  return 1;
}

int
Buffer::forward_word (Point &point, long n) const
{
  if (n > 0)
    {
      if (eobp (point))
        return 0;
      word_state ws (xsyntax_table (lsyntax_table), point.ch ());
      for (long i = 0; i < n; i++)
        {
          while (ws.forward (point.ch ()) == word_state::punct)
            if (!forward_char (point, 1) || eobp (point))
              return i;
          while (ws.forward (point.ch ()) == word_state::inword)
            if (!forward_char (point, 1) || eobp (point))
              return i;
        }
    }
  else if (n < 0)
    {
      if (bobp (point) || (eobp (point) && !forward_char (point, -1)))
        return 0;
      int back = 0;
      word_state ws (xsyntax_table (lsyntax_table), point.ch ());
      if (ws.backward (point.ch ()) == word_state::inword)
        {
          if (!forward_char (point, -1))
            return 0;
          while (ws.backward (point.ch ()) == word_state::inword)
            {
              if (!forward_char (point, -1))
                return 1;
              back = -1;
            }
        }

      for (long i = back; i > n; i--)
        {
          while (ws.backward (point.ch ()) == word_state::punct)
            if (!forward_char (point, -1))
              return i;
          while (ws.backward (point.ch ()) == word_state::inword)
            if (!forward_char (point, -1))
              return i;
        }
      forward_char (point, 1);
    }
  return n;
}

struct fold_info
{
  const Char *p0;
  const Char *p;
  const Char *pe;
  Chunk *cp;
  long column;
};

static int
hang_char1 (fold_info &f, Char cc)
{
  if (f.p == f.pe)
    {
      Chunk *cp = f.cp->c_next;
      if (cp && *cp->c_text == cc)
        {
          f.cp = cp;
          f.p = cp->c_text + 1;
          f.pe = cp->c_text + cp->c_used;
          return 1;
        }
    }
  else if (*f.p == cc)
    {
      f.p++;
      return 1;
    }
  return 0;
}

static int
hang_char (fold_info &f, int mode)
{
  int r = 0;
  if (mode & Buffer::KINSOKU_SPC && hang_char1 (f, ' '))
    r = 1;
  if (mode & Buffer::KINSOKU_EOL && hang_char1 (f, '\n'))
    r = 1;
  return r;
}

static int
iso8859_word_char_p (Char cc)
{
  ucs2_t wc = i2w (cc);
  return (wc >= 0xc0
          && (wc <= 0x1ff
              || (wc >= 0x250
                  && (wc <= 0x2af
                      || (wc >= 0x386
                          && (wc <= 0x3ce
                              || (wc >= 0x401
                                  && wc <= 0x4ff)))))));
}

#if 0
static inline int
word_char_fwd_p (Char cc, const syntax_table *tab)
{
  if (DBCP (cc))
    return iso8859_word_char_p (cc);
  syntax_code sc = syntax_code (xchar_syntax (tab, cc));
  return sc == SCword || sc == SCsymbol || sc == SCtag_end;
}

static inline int
word_char_bwd_p (Char cc, const syntax_table *tab, syntax_code &last)
{
  if (last == SCtag_start)
    return 0;
  if (DBCP (cc))
    {
      if (!iso8859_word_char_p (cc))
        return 0;
      last = SCword;
      return 1;
    }
  syntax_code sc = syntax_code (xchar_syntax (tab, cc));
  switch (sc)
    {
    case SCword:
    case SCsymbol:
      last = SCword;
      return 1;

    case SCtag_start:
    case SCquote:
    case SCsymbol_prefix:
      last = SCtag_start;
      return 1;

    default:
      return 0;
    }
}

static void
word_wrap (const Buffer *bp, fold_info &f)
{
  const syntax_table *tab = xsyntax_table (bp->lsyntax_table);
  Char cc;
  if (f.p == f.pe)
    {
      Chunk *cp = f.cp->c_next;
      if (!cp)
        return;
      cc = *cp->c_text;
    }
  else
    cc = *f.p;
  if (!word_char_fwd_p (cc, tab))
    return;

  syntax_code last = SCword;
  const Char *p = f.p;
  Chunk *cp = f.cp;
  while (1)
    {
      if (p == cp->c_text)
        {
          cp = cp->c_prev;
          if (!cp)
            break;
          p = cp->c_text + cp->c_used - 1;
        }
      else
        p--;
      if (p == f.p0)
        break;
      if (!word_char_bwd_p (*p, tab, last))
        {
          f.cp = cp;
          f.p = p + 1;
          f.pe = cp->c_text + cp->c_used;
          return;
        }
    }
}
#else

static inline int
word_char_p (Char cc)
{
  return cc > ' ' && char_width (cc) == 1 && !kana_char_p (cc);
}

static void
word_wrap (const Buffer *bp, fold_info &f)
{
  Char cc;
  if (f.p == f.pe)
    {
      Chunk *cp = f.cp->c_next;
      if (!cp)
        return;
      cc = *cp->c_text;
    }
  else
    cc = *f.p;
  if (!word_char_p (cc))
    return;

  const Char *p = f.p;
  Chunk *cp = f.cp;
  while (1)
    {
      if (p == cp->c_text)
        {
          cp = cp->c_prev;
          if (!cp)
            break;
          p = cp->c_text + cp->c_used - 1;
        }
      else
        p--;
      if (p == f.p0)
        break;
      if (!word_char_p (*p))
        {
          f.cp = cp;
          f.p = p + 1;
          f.pe = cp->c_text + cp->c_used;
          return;
        }
    }
}
#endif

static inline int
in_chars (Char c, const Char *p, const Char *const pe)
{
  if (p == pe || c > pe[-1])
    return 0;
  for (; p < pe && *p <= c; p++)
    if (c == *p)
      return 1;
  return 0;
}

static int
kinsoku_bwd (fold_info &f, Char nextch, int limit,
             const Char *bchars, const Char *bcharsl,
             const Char *echars, const Char *echarsl)
{
  Chunk *cp = f.cp;
  const Char *p = f.p;
  const Char *pe = f.pe;
  int skip = 1;
  while (1)
    {
      if (--limit < 0)
        return 0;
      if (p == cp->c_text)
        {
          cp = cp->c_prev;
          if (!cp)
            return 0;
          p = cp->c_text + cp->c_used - 1;
        }
      else
        p--;
      if (p == f.p0)
        return 0;
      int in;
      if (skip)
        {
          in = 0;
          skip = 0;
        }
      else if (!(in = in_chars (*p, echars, echarsl))
               && (nextch == '\n' || !in_chars (nextch, bchars, bcharsl)))
        {
          f.cp = cp;
          f.p = p + 1;
          f.pe = cp->c_text + cp->c_used;
          return 1;
        }
      nextch = in ? '\n' : *p;
    }
}

static int
kinsoku_fwd (fold_info &f, Char prevch, int limit,
             const Char *bchars, const Char *bcharsl,
             const Char *echars, const Char *echarsl)
{
  Chunk *cp = f.cp;
  const Char *p = f.p;
  const Char *pe = f.pe;
  int skip = 1;
  while (1)
    {
      if (--limit < 0)
        return 0;
      if (p == pe)
        {
          if (!cp->c_next)
            {
              if (prevch != '\n' && in_chars (prevch, echars, echarsl))
                return 0;
              break;
            }
          cp = cp->c_next;
          p = cp->c_text;
          pe = p + cp->c_used;
        }
      int in;
      if (skip)
        {
          in = 0;
          skip = 0;
        }
      else if (!(in = in_chars (*p, bchars, bcharsl))
               && (prevch == '\n' || !in_chars (prevch, echars, echarsl)))
        break;
      prevch = in ? '\n' : *p;
      p++;
    }
  f.cp = cp;
  f.p = p;
  f.pe = pe;
  return 1;
}

static void
kinsoku (const Buffer *bp, fold_info &f, const fold_parameter &param)
{
  const Char *bchars, *bcharsl, *echars, *echarsl;
  lisp x = bp->kinsoku_bol_chars ();
  if (stringp (x))
    {
      bchars = xstring_contents (x);
      bcharsl = bchars + xstring_length (x);
    }
  else
    bchars = bcharsl = 0;

  x = bp->kinsoku_eol_chars ();
  if (stringp (x))
    {
      echars = xstring_contents (x);
      echarsl = echars + xstring_length (x);
    }
  else
    echars = echarsl = 0;

  if (!echars && !bchars)
    return;

  Chunk *cp = f.cp;
  const Char *p = f.p;
  const Char *pe = f.pe;

  Char prevch, nextch;
  if (p == cp->c_text)
    {
      Chunk *pcp = cp->c_prev;
      prevch = pcp ? pcp->c_text[pcp->c_used - 1] : '\n';
    }
  else
    prevch = p[-1];

  if (p == pe)
    {
      Chunk *ncp = cp->c_next;
      nextch = ncp ? *ncp->c_text : '\n';
    }
  else
    nextch = *p;

  if (prevch != '\n' && in_chars (prevch, echars, echarsl))
    {
      if (!kinsoku_bwd (f, prevch, param.shorten_limit,
                        bchars, bcharsl, echars, echarsl))
        kinsoku_fwd (f, nextch, param.extend_limit,
                     bchars, bcharsl, echars, echarsl);
    }
  else if (nextch != '\n' && in_chars (nextch, bchars, bcharsl))
    {
      if (!kinsoku_fwd (f, nextch, param.extend_limit,
                        bchars, bcharsl, echars, echarsl))
        kinsoku_bwd (f, prevch, param.shorten_limit,
                     bchars, bcharsl, echars, echarsl);
    }
}

int
Buffer::parse_fold_line (Point &point, long fold_columns,
                         const fold_parameter &param) const
{
  if (point.p_offset == point.p_chunk->c_used && point.p_chunk->c_next)
    {
      point.p_chunk = point.p_chunk->c_next;
      point.p_offset = 0;
    }

  fold_info f;
  f.cp = point.p_chunk;
  f.p0 = f.p = f.cp->c_text + point.p_offset;
  f.pe = f.cp->c_text + f.cp->c_used;
  f.column = 0;

  while (1)
    {
      if (f.p >= f.pe)
        {
          f.cp = f.cp->c_next;
          if (!f.cp)
            {
              point.p_chunk = 0;
              return 0;
            }
          f.p = f.cp->c_text;
          f.pe = f.p + f.cp->c_used;
        }

      Char c = *f.p++;
      if (c == '\n')
        break;

      int cl = char_columns (c, f.column);
      f.column += cl;
      if (f.column >= fold_columns)
        {
          if (f.column > fold_columns && cl == 2 && c != '\t')
            f.p--;
          if (param.mode & (KINSOKU_EOL | KINSOKU_SPC) && hang_char (f, param.mode))
            break;
          if (param.mode & KINSOKU_CHARS)
            kinsoku (this, f, param);
          if (param.mode & KINSOKU_WORDWRAP)
            word_wrap (this, f);
          if (param.mode & (KINSOKU_EOL | KINSOKU_SPC) && hang_char (f, param.mode))
            break;
          if (f.p == f.pe && !f.cp->c_next)
            {
              point.p_chunk = 0;
              return 0;
            }
          break;
        }
    }

  if (f.p == f.cp->c_text)
    {
      point.p_chunk = f.cp->c_prev;
      point.p_offset = point.p_chunk->c_used - 1;
    }
  else
    {
      point.p_offset = f.p - f.cp->c_text - 1;
      point.p_chunk = f.cp;
    }
  return 1;
}

int
Buffer::parse_fold_line (Point &point, long max_width, const glyph_width &gw,
                         const fold_parameter &param) const
{
  if (point.p_offset == point.p_chunk->c_used && point.p_chunk->c_next)
    {
      point.p_chunk = point.p_chunk->c_next;
      point.p_offset = 0;
    }

  fold_info f;
  f.cp = point.p_chunk;
  f.p0 = f.p = f.cp->c_text + point.p_offset;
  f.pe = f.cp->c_text + f.cp->c_used;
  f.column = 0;
  int pixel = 0;

  while (1)
    {
      if (f.p >= f.pe)
        {
          f.cp = f.cp->c_next;
          if (!f.cp)
            {
              point.p_chunk = 0;
              return 0;
            }
          f.p = f.cp->c_text;
          f.pe = f.p + f.cp->c_used;
        }

      Char c = *f.p++;
      if (c == '\n')
        break;

      if (c == '\t')
        pixel += (get_glyph_width (' ', gw)
                  * (b_tab_columns - f.column % b_tab_columns));
      else
        pixel += get_glyph_width (c, gw);
      int cl = char_columns (c, f.column);
      f.column += cl;
      if (pixel >= max_width)
        {
          if (pixel > max_width && cl == 2 && c != '\t')
            f.p--;
          if (param.mode & (KINSOKU_EOL | KINSOKU_SPC) && hang_char (f, param.mode))
            break;
          if (param.mode & KINSOKU_WORDWRAP)
            word_wrap (this, f);
          if (param.mode & KINSOKU_CHARS)
            kinsoku (this, f, param);
          if (param.mode & (KINSOKU_EOL | KINSOKU_SPC) && hang_char (f, param.mode))
            break;
          if (f.p == f.pe && !f.cp->c_next)
            {
              point.p_chunk = 0;
              return 0;
            }
          break;
        }
    }

  if (f.p == f.cp->c_text)
    {
      point.p_chunk = f.cp->c_prev;
      point.p_offset = point.p_chunk->c_used - 1;
    }
  else
    {
      point.p_offset = f.p - f.cp->c_text - 1;
      point.p_chunk = f.cp;
    }
  return 1;
}

static int
trail_chunk_modified_p (const Chunk *cp)
{
  while ((cp = cp->c_next))
    if (cp->c_nbreaks)
      return cp->c_nbreaks < 0;
  return 0;
}

static void
add_break (Chunk *cp, u_int n)
{
  if (cp->c_first_eol < 0)
    cp->c_first_eol = n;
  cp->c_last_eol = n;
  cp->break_on (n);
}

void
Buffer::parse_fold_chunk (Chunk *cp) const
{
  if (b_nfolded >= 0)
    return;

  Point point;
  if (cp->c_nbreaks < 0)
    {
      cp->clear_breaks ();
      if (cp->c_prev)
        {
          add_break (cp, cp->c_first_eol);
          point.p_offset = cp->c_first_eol + 1;
        }
      else
        {
          cp->c_first_eol = -1;
          point.p_offset = 0;
        }
    }
  else if (trail_chunk_modified_p (cp))
    {
      if (cp->c_nbreaks)
        point.p_offset = cp->c_last_eol + 1;
      else if (!cp->c_prev)
        point.p_offset = 0;
      else
        return;
    }
  else
    return;

  fold_parameter param;
  param.mode = kinsoku_mode ();
  param.extend_limit = kinsoku_extend_limit ();
  param.shorten_limit = kinsoku_shorten_limit ();

  point.p_chunk = cp;
  while (1)
    {
      parse_fold_line (point, param);
      if (point.p_chunk != cp)
        break;
      add_break (cp, point.p_offset);
      point.p_offset++;
    }

  for (cp = cp->c_next; cp != point.p_chunk; cp = cp->c_next)
    {
      cp->clear_breaks ();
      cp->c_first_eol = -1;
      cp->c_last_eol = -1;
    }

  if (cp && cp->c_first_eol != point.p_offset)
    {
      cp->c_nbreaks = -1;
      cp->c_first_eol = point.p_offset;
    }
}

long
Buffer::folded_count_lines ()
{
  if (b_nfolded == -1)
    {
      long nlines = 1;
      for (Chunk *cp = b_chunkb; cp; cp = cp->c_next)
        {
          parse_fold_chunk (cp);
          nlines += cp->c_nbreaks;
        }
      b_nfolded = nlines;
    }
  return b_nfolded;
}

static void
adjust_offset (Point &point)
{
  if (point.p_offset == point.p_chunk->c_used && point.p_chunk->c_next)
    {
      point.p_offset = 0;
      point.p_chunk = point.p_chunk->c_next;
    }
}

void
Buffer::update_fold_chunk (point_t goal, update_fold_info &info) const
{
  info.linenum = 1;
  info.point = 0;
  Chunk *cp;
  for (cp = b_chunkb; info.point + cp->c_used < goal; cp = cp->c_next)
    {
      parse_fold_chunk (cp);
      info.linenum += cp->c_nbreaks;
      info.point += cp->c_used;
    }
  parse_fold_chunk (cp);
  info.cp = cp;
}

long
Buffer::folded_point_linenum_column (point_t goal, long *columnp) const
{
  update_fold_info info;
  update_fold_chunk (goal, info);
  if (info.cp->c_nbreaks)
    {
      for (int i = 0, e = goal - info.point; i < e; i++)
        if (info.cp->break_p (i))
          info.linenum++;
    }
  if (columnp)
    *columnp = folded_point_column_1 (goal, info);

  return info.linenum;
}

long
Buffer::folded_point_column_1 (point_t goal, Point &point) const
{
  int rest = goal - point.p_point;
  long column = 0;

  while (1)
    {
      int l = point.p_chunk->c_used - point.p_offset;
      const Char *p = point.p_chunk->c_text + point.p_offset;
      const Char *pe = p + min (l, rest);
      while (p < pe)
        column += char_columns (*p++, column);
      rest -= l;
      if (rest <= 0)
        break;
      point.p_chunk = point.p_chunk->c_next;
      point.p_offset = 0;
    }
  return column;
}

long
Buffer::folded_point_column_1 (point_t goal, const update_fold_info &info) const
{
  Point point;
  point.p_chunk = info.cp;
  point.p_offset = goal - info.point;
  point.p_point = goal;
  adjust_offset (point);
  folded_go_bol_1 (point);
  return folded_point_column_1 (goal, point);
}

long
Buffer::folded_point_linenum (point_t goal) const
{
  return folded_point_linenum_column (goal, 0);
}

long
Buffer::folded_point_column (const Point &point) const
{
  update_fold_info info;
  update_fold_chunk (point.p_point, info);
  return folded_point_column_1 (point.p_point, info);
}

long
Buffer::folded_linenum_point (Point &pbuf, long goal)
{
  assert (goal >= 1);
  if (goal == 1)
    {
      pbuf.p_point = 0;
      pbuf.p_chunk = b_chunkb;
      pbuf.p_offset = 0;
      return 1;
    }

  Chunk *cp = b_chunkb;
  point_t point = 0;
  long linenum = 1;

  while (1)
    {
      parse_fold_chunk (cp);
      long l = linenum + cp->c_nbreaks;
      if (l >= goal)
        {
          if (goal - linenum == 1)
            {
              pbuf.p_offset = cp->c_first_eol + 1;
              linenum++;
            }
          else if (l == goal)
            {
              pbuf.p_offset = cp->c_last_eol + 1;
              linenum = goal;
            }
          else
            {
              int o;
              for (o = cp->c_first_eol;; o++)
                if (cp->break_p (o) && ++linenum == goal)
                  break;
              pbuf.p_offset = o + 1;
            }
          pbuf.p_point = point + pbuf.p_offset;
          pbuf.p_chunk = cp;
          adjust_offset (pbuf);
          break;
        }
      linenum = l;
      point += cp->c_used;
      if (!cp->c_next)
        {
          pbuf.p_chunk = cp;
          pbuf.p_point = point;
          pbuf.p_offset = cp->c_used;
          folded_go_bol_1 (pbuf);
          break;
        }
      cp = cp->c_next;
    }

  return linenum;
}

long
Buffer::folded_forward_column (Point &point, long ncolumns, long curcol,
                               int can_exceed, int restrict) const
{
  if (curcol >= ncolumns)
    return curcol;

  point_t limit = restrict ? b_contents.p2 : b_nchars;

  Point eol (point);
  folded_go_eol (eol);
  if (eol.p_point < limit)
    limit = eol.p_point;

  Chunk *cp = point.p_chunk;
  while (point.p_point < limit)
    {
      long nextcol = curcol + char_columns (cp->c_text[point.p_offset], curcol);
      if (!can_exceed && nextcol > ncolumns)
        break;
      curcol = nextcol;
      point.p_point++;
      point.p_offset++;
      if (point.p_offset == cp->c_used)
        {
          if (!cp->c_next)
            break;
          cp = cp->c_next;
          point.p_offset = 0;
        }
      if (curcol >= ncolumns)
        break;
    }
  point.p_chunk = cp;

  return curcol;
}

void
Buffer::folded_go_bol_1 (Point &point) const
{
  Chunk *cp = point.p_chunk;
  point.p_point -= point.p_offset;
  if (cp->c_nbreaks && point.p_offset && cp->c_first_eol < point.p_offset)
    {
      assert (cp->c_first_eol >= 0);
      assert (cp->break_p (cp->c_first_eol));
      int o;
      for (o = point.p_offset - 1; !cp->break_p (o); o--)
        ;
      point.p_chunk = cp;
      point.p_offset = o + 1;
      point.p_point += point.p_offset;
      adjust_offset (point);
    }
  else
    {
      while (1)
        {
          cp = cp->c_prev;
          if (!cp)
            {
              point.p_chunk = b_chunkb;
              point.p_offset = 0;
              point.p_point = 0;
              break;
            }
          point.p_point -= cp->c_used;
          if (cp->c_nbreaks)
            {
              assert (cp->c_last_eol >= 0);
              point.p_chunk = cp;
              point.p_offset = cp->c_last_eol + 1;
              point.p_point += point.p_offset;
              adjust_offset (point);
              break;
            }
        }
    }
}

void
Buffer::folded_go_bol (Point &point) const
{
  if (b_nfolded < 0)
    for (Chunk *cp = b_chunkb;; cp = cp->c_next)
      {
        parse_fold_chunk (cp);
        if (cp == point.p_chunk)
          break;
      }

  folded_go_bol_1 (point);
}

void
Buffer::folded_go_eol (Point &point) const
{
  Chunk *cp;
  if (b_nfolded >= 0)
    cp = point.p_chunk;
  else
    for (cp = b_chunkb;; cp = cp->c_next)
      {
        parse_fold_chunk (cp);
        if (cp == point.p_chunk)
          break;
      }

  point.p_point -= point.p_offset;
  if (cp->c_nbreaks && cp->c_last_eol >= point.p_offset)
    {
      assert (cp->c_last_eol >= 0);
      assert (cp->break_p (cp->c_last_eol));
      int o;
      for (o = point.p_offset; !cp->break_p (o); o++)
        ;
      point.p_chunk = cp;
      point.p_offset = o;
      point.p_point += point.p_offset;
    }
  else
    {
      while (1)
        {
          if (!cp->c_next)
            {
              point.p_chunk = cp;
              point.p_offset = cp->c_used;
              point.p_point += point.p_offset;
              break;
            }
          point.p_point += cp->c_used;
          cp = cp->c_next;
          if (cp->c_nbreaks)
            {
              assert (cp->c_first_eol >= 0);
              point.p_chunk = cp;
              point.p_offset = cp->c_first_eol;
              point.p_point += point.p_offset;
              break;
            }
        }
    }
}

void
Buffer::folded_goto_bol (Point &point) const
{
  folded_go_bol (point);
  if (point.p_point < b_contents.p1)
    goto_char (point, b_contents.p1);
}

void
Buffer::folded_goto_eol (Point &point) const
{
  folded_go_eol (point);
  if (point.p_point > b_contents.p2)
    goto_char (point, b_contents.p2);
}

int
Buffer::folded_forward_line (Point &opoint, long nlines)
{
  if (!nlines)
    return 0;
  Point npoint;
  long olinenum = folded_point_linenum (opoint);
  long nlinenum = max (1L, olinenum + nlines);
  if (olinenum == nlinenum)
    return 0;
  nlinenum = folded_linenum_point (npoint, nlinenum);
  if (olinenum == nlinenum)
    return 0;
  if (npoint.p_point < b_contents.p1)
    goto_char (npoint, b_contents.p1);
  else if (npoint.p_point > b_contents.p2)
    goto_char (npoint, b_contents.p2);
  else
    {
      opoint = npoint;
      return nlinenum - olinenum;
    }
  nlinenum = folded_point_linenum (npoint);
  if (olinenum == nlinenum)
    return 0;
  opoint = npoint;
  return nlinenum - olinenum;
}

long
Buffer::folded_goto_column (Point &point, long column, int exceed) const
{
  folded_go_bol (point);
  long curcol = folded_forward_column (point, column, 0, exceed, 1);
  if (point.p_point < b_contents.p1)
    {
      goto_char (point, b_contents.p1);
      curcol = folded_point_column (point);
    }
  return curcol;
}

lisp
Fforward_char (lisp n)
{
  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  return boole (wp->w_bufp->forward_char (wp->w_point,
                                          (!n || n == Qnil) ? 1 : fixnum_value (n)));
}

lisp
Fforward_line (lisp n)
{
  Window *wp = selected_window ();
  long l = wp->w_bufp->forward_line (wp->w_point, (!n || n == Qnil) ? 1 : fixnum_value (n));
  if (!l)
    return Qnil;
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  return make_fixnum (l);
}

lisp
Fforward_virtual_line (lisp n)
{
  Window *wp = selected_window ();
  long l = (wp->w_bufp->b_fold_columns == Buffer::FOLD_NONE
            ? wp->w_bufp->forward_line (wp->w_point,
                                        (!n || n == Qnil) ? 1 : fixnum_value (n))
            : wp->w_bufp->folded_forward_line (wp->w_point,
                                               (!n || n == Qnil) ? 1 : fixnum_value (n)));
  if (!l)
    return Qnil;
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  return make_fixnum (l);
}

lisp
Fforward_word (lisp n)
{
  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  return boole (wp->w_bufp->forward_word (wp->w_point,
                                          (!n || n == Qnil) ? 1 : fixnum_value (n)));
}

lisp
Fgoto_bol ()
{
  Window *wp = selected_window ();
  wp->w_bufp->goto_bol (wp->w_point);
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  return Qt;
}

lisp
Fgoto_eol ()
{
  Window *wp = selected_window ();
  wp->w_bufp->goto_eol (wp->w_point);
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  return Qt;
}

lisp
Fgoto_virtual_bol ()
{
  Window *wp = selected_window ();
  if (wp->w_bufp->b_fold_columns == Buffer::FOLD_NONE)
    wp->w_bufp->goto_bol (wp->w_point);
  else
    wp->w_bufp->folded_goto_bol (wp->w_point);
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  return Qt;
}

lisp
Fgoto_virtual_eol ()
{
  Window *wp = selected_window ();
  if (wp->w_bufp->b_fold_columns == Buffer::FOLD_NONE)
    wp->w_bufp->goto_eol (wp->w_point);
  else
    wp->w_bufp->folded_goto_eol (wp->w_point);
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  return Qt;
}

lisp
Fpoint_min ()
{
  return make_fixnum (selected_buffer ()->b_contents.p1);
}

lisp
Fpoint_max ()
{
  return make_fixnum (selected_buffer ()->b_contents.p2);
}

lisp
Fgoto_char (lisp n)
{
  Window *wp = selected_window ();
  wp->w_bufp->goto_char (wp->w_point, wp->w_bufp->coerce_to_point (n));
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  return Qt;
}

lisp
Fgoto_column (lisp n, lisp exceed)
{
  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  return make_fixnum (wp->w_bufp->goto_column (wp->w_point,
                                               fixnum_value (n),
                                               exceed && exceed != Qnil));
}

lisp
Fforward_column (lisp curcol, lisp n, lisp exceed)
{
  long ccol = fixnum_value (curcol);
  long ncol = fixnum_value (n);
  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  int col = wp->w_bufp->forward_column (wp->w_point, ncol, ccol,
                                        exceed && exceed != Qnil, 1);
  if (wp->w_point.p_point < wp->w_bufp->b_contents.p1)
    {
      wp->w_bufp->goto_char (wp->w_point, wp->w_bufp->b_contents.p1);
      col = wp->w_bufp->point_column (wp->w_point);
    }
  return make_fixnum (col);
}

lisp
Fkinsoku_goto_column (lisp lcolumn, lisp mode)
{
  long column = fixnum_value (lcolumn);
  if (column < MIN_FOLD_WIDTH || column > MAX_FOLD_WIDTH)
    FErange_error (lcolumn);
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;

  fold_parameter param;
  param.mode = !mode || mode == Qnil ? bp->kinsoku_mode () : fixnum_value (mode);
  param.extend_limit = bp->kinsoku_extend_limit ();
  param.shorten_limit = bp->kinsoku_shorten_limit ();

  bp->go_bol (wp->w_point);
  Point point = wp->w_point;
  bp->parse_fold_line (point, column, param);
  if (!point.p_chunk)
    bp->goto_char (wp->w_point, bp->b_contents.p2);
  else
    {
      point_t p = wp->w_point.p_point - wp->w_point.p_offset;
      for (const Chunk *cp = wp->w_point.p_chunk; cp != point.p_chunk; cp = cp->c_next)
        p += cp->c_used;
      point.p_point = p + point.p_offset;
      bp->check_range (point);
      wp->w_point = point;
    }
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  return Qt;
}

lisp
Fgoto_virtual_column (lisp n, lisp exceed)
{
  Window *wp = selected_window ();
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  return make_fixnum (wp->w_bufp->b_fold_columns == Buffer::FOLD_NONE
                      ? wp->w_bufp->goto_column (wp->w_point,
                                                 fixnum_value (n),
                                                 exceed && exceed != Qnil)
                      : wp->w_bufp->folded_goto_column (wp->w_point,
                                                        fixnum_value (n),
                                                        exceed && exceed != Qnil));
}

lisp
Fpoint ()
{
  return make_fixnum (selected_window ()->w_point.p_point);
}

lisp
Fset_mark (lisp point)
{
  Window *wp = selected_window ();
  if (!point || point == Qnil)
    wp->w_mark = wp->w_point.p_point;
  else if (point == Qt)
    wp->w_mark = NO_MARK_SET;
  else
    wp->w_mark = wp->w_bufp->coerce_to_restricted_point (point);
  return Qt;
}

lisp
Fmark (lisp force)
{
  point_t mark = selected_window ()->w_mark;
  if (mark == NO_MARK_SET)
    {
      if (force && force != Qnil)
        return Qnil;
      FEsimple_error (Eno_mark_set_in_this_window);
    }
  return make_fixnum (mark);
}

static lisp
region_limit (int begp)
{
  Window *wp = selected_window ();
  point_t point = wp->w_point.p_point;
  point_t mark = wp->w_mark;
  if (mark == NO_MARK_SET)
    FEsimple_error (Eno_mark_set_in_this_window);
  if (begp)
    return make_fixnum (min (point, mark));
  else
    return make_fixnum (max (point, mark));
}

lisp
Fregion_beginning ()
{
  return region_limit (1);
}

lisp
Fregion_end ()
{
  return region_limit (0);
}

lisp
Fcontinue_pre_selection ()
{
  Window *wp = selected_window ();
  if (wp->w_selection_type == Buffer::SELECTION_VOID
      || !(wp->w_selection_type & Buffer::PRE_SELECTION))
    return Qnil;
  (int &)wp->w_selection_type |= Buffer::CONTINUE_PRE_SELECTION;
  return Qt;
}

lisp
Fpre_selection_p ()
{
  return boole (selected_window ()->w_selection_type & Buffer::PRE_SELECTION);
}

lisp
Fget_selection_type ()
{
  Window *wp = selected_window ();
  return (wp->w_selection_type == Buffer::SELECTION_VOID
          ? Qnil
          : make_fixnum (wp->w_selection_type & Buffer::SELECTION_TYPE_MASK));
}

lisp
Fset_selection_type (lisp type, lisp temp)
{
  Window *wp = selected_window ();
  int otype = wp->w_selection_type & Buffer::SELECTION_TYPE_MASK;
  int ntype = fixnum_value (type);
  switch (ntype)
    {
    case Buffer::SELECTION_LINEAR:
    case Buffer::SELECTION_REGION:
    case Buffer::SELECTION_RECTANGLE:
      if (wp->w_selection_marker != NO_MARK_SET)
        {
          wp->w_selection_type = Buffer::selection_type (ntype);
          if (temp && temp != Qnil)
            (int &)wp->w_selection_type |= Buffer::PRE_SELECTION | Buffer::CONTINUE_PRE_SELECTION;
        }
      break;

    default:
      wp->w_selection_type = Buffer::SELECTION_VOID;
      wp->w_selection_point = NO_MARK_SET;
      wp->w_selection_marker = NO_MARK_SET;
      break;
    }
  if ((wp->w_selection_type & Buffer::SELECTION_TYPE_MASK) != otype
      && wp->w_selection_region.p1 != -1)
    wp->w_bufp->set_modified_region (wp->w_selection_region.p1,
                                     wp->w_selection_region.p2);
  return boole (wp->w_selection_type != Buffer::SELECTION_VOID);
}

lisp
Fstart_selection (lisp type, lisp temp, lisp point)
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  fixnum_value (type); /* check type only */
  if (point && point != Qnil)
    wp->w_selection_marker = bp->coerce_to_restricted_point (point);
  else
    wp->w_selection_marker = wp->w_point.p_point;
  wp->w_selection_point = NO_MARK_SET;
  if (Fset_selection_type (type, temp) == Qnil)
    return Qnil;
  wp->w_selection_column = (bp->b_fold_columns == Buffer::FOLD_NONE
                            ? bp->point_column (wp->w_point)
                            : bp->folded_point_column (wp->w_point));
  if (wp->w_selection_region.p1 != -1)
    bp->set_modified_region (wp->w_selection_region.p1,
                             wp->w_selection_region.p2);
  wp->w_selection_region.p1 = wp->w_selection_marker;
  wp->w_selection_region.p2 = wp->w_selection_marker;
  return Qt;
}

lisp
Fstop_selection ()
{
  Window *wp = selected_window ();
  if (wp->w_selection_type != Buffer::SELECTION_VOID
      && wp->w_selection_region.p1 != -1)
    {
      wp->w_bufp->set_modified_region (wp->w_selection_region.p1,
                                       wp->w_selection_region.p2);
      wp->w_selection_point = NO_MARK_SET;
      wp->w_selection_marker = NO_MARK_SET;
      wp->w_selection_type = Buffer::SELECTION_VOID;
    }
  return Qt;
}

lisp
Ffix_selection_point ()
{
  Window *wp = selected_window ();
  if (wp->w_selection_type == Buffer::SELECTION_VOID)
    return Qnil;
  wp->w_selection_point = wp->w_point.p_point;
  return Qt;
}

lisp
Fselection_point ()
{
  Window *wp = selected_window ();
  if (wp->w_selection_type == Buffer::SELECTION_VOID)
    FEsimple_error (Eno_mark_set_in_this_window);
  return make_fixnum (wp->w_selection_point == NO_MARK_SET
                      ? wp->w_point.p_point
                      : wp->w_selection_point);
}

lisp
Fselection_mark ()
{
  Window *wp = selected_window ();
  if (wp->w_selection_type == Buffer::SELECTION_VOID)
    FEsimple_error (Eno_mark_set_in_this_window);
  return make_fixnum (wp->w_selection_marker);
}

lisp
Freverse_region (lisp from, lisp to, lisp temp)
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  point_t p1 = bp->coerce_to_restricted_point (from);
  point_t p2 = bp->coerce_to_restricted_point (to);
  if (p1 > p2)
    swap (p1, p2);
  if (wp->w_reverse_region.p1 != NO_MARK_SET)
    bp->set_modified_region (wp->w_reverse_region.p1,
                             wp->w_reverse_region.p2);
  wp->w_reverse_region.p1 = p1;
  wp->w_reverse_region.p2 = p2;
  wp->w_reverse_temp = (temp && temp != Qnil
                        ? Buffer::selection_type (Buffer::PRE_SELECTION
                                                  | Buffer::CONTINUE_PRE_SELECTION)
                        : Buffer::SELECTION_VOID);
  return Qt;
}

lisp
Fclear_reverse_region ()
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  if (wp->w_reverse_region.p1 != NO_MARK_SET)
    bp->set_modified_region (wp->w_reverse_region.p1,
                             wp->w_reverse_region.p2);
  wp->w_reverse_region.p1 = NO_MARK_SET;
  wp->w_reverse_region.p2 = NO_MARK_SET;
  wp->w_reverse_temp = Buffer::SELECTION_VOID;
  return Qt;
}

lisp
Fmake_marker (lisp buffer)
{
  Buffer *bp = Buffer::coerce_to_buffer (buffer);
  lisp marker = make_marker ();
  bp->lmarkers = Fcons (marker, bp->lmarkers);
  xmarker_buffer (marker) = bp;
  xmarker_point (marker) = NO_MARK_SET;
  return marker;
}

void
Buffer::valid_marker_p (lisp marker) const
{
  check_marker (marker);
  if (!xmarker_buffer (marker))
    FEprogram_error (Edeleted_marker);
  if (xmarker_buffer (marker) != this)
    FEprogram_error (Emarker_on_different_buffer, marker);
}

lisp
Fset_marker (lisp marker, lisp point)
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  bp->valid_marker_p (marker);
  if (!point || point == Qnil)
    xmarker_point (marker) = wp->w_point.p_point;
  else
    xmarker_point (marker) = bp->coerce_to_restricted_point (point);
  return marker;
}

lisp
Funset_marker (lisp marker)
{
  check_marker (marker);
  xmarker_point (marker) = NO_MARK_SET;
  return Qt;
}

lisp
Fgoto_marker (lisp marker)
{
  Window FAR *wp = selected_window ();
  wp->w_bufp->valid_marker_p (marker);
  if (xmarker_point (marker) == NO_MARK_SET)
    FEsimple_error (Emarker_is_not_set);
  wp->w_bufp->goto_char (wp->w_point, xmarker_point (marker));
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  return Qt;
}

lisp
Fdelete_marker (lisp marker)
{
  check_marker (marker);
  Buffer *bp = xmarker_buffer (marker);
  if (!bp)
    return Qnil;
  if (processp (bp->lprocess)
      && Fprocess_marker (bp->lprocess) == marker)
    return Qnil;
  delq (marker, &bp->lmarkers);
  xmarker_buffer (marker) = 0;
  xmarker_point (marker) = NO_MARK_SET;
  return Qt;
}

lisp
Fmarker_point (lisp marker)
{
  check_marker (marker);
  return ((!xmarker_buffer (marker) || xmarker_point (marker) == NO_MARK_SET)
          ? Qnil : make_fixnum (xmarker_point (marker)));
}

lisp
Fmarker_buffer (lisp marker)
{
  check_marker (marker);
  return xmarker_buffer (marker) ? xmarker_buffer (marker)->lbp : Qnil;
}

lisp
Fscroll_window (lisp n)
{
  return boole (selected_window ()->scroll_window (fixnum_value (n)));
}

lisp
Fscroll_window_horizontally (lisp n)
{
  selected_window ()->scroll_window_horizontally (fixnum_value (n));
  return Qt;
}

int
Buffer::bolp (const Point &point) const
{
  return bobp (point) || point.prevch () == '\n';
}

int
Buffer::eolp (const Point &point) const
{
  return eobp (point) || point.ch () == '\n';
}

lisp
Fbolp ()
{
  Window *wp = selected_window ();
  return boole (wp->w_bufp->bolp (wp->w_point));
}

lisp
Feolp ()
{
  Window *wp = selected_window ();
  return boole (wp->w_bufp->eolp (wp->w_point));
}

lisp
Fvirtual_bolp ()
{
  Window *wp = selected_window ();
  if (wp->w_bufp->bolp (wp->w_point))
    return Qt;
  if (wp->w_bufp->b_fold_columns == Buffer::FOLD_NONE)
    return Qnil;
  Point point (wp->w_point);
  wp->w_bufp->folded_goto_bol (point);
  return boole (point.p_point == wp->w_point.p_point);
}

lisp
Fvirtual_eolp ()
{
  Window *wp = selected_window ();
  if (wp->w_bufp->eolp (wp->w_point))
    return Qt;
  if (wp->w_bufp->b_fold_columns == Buffer::FOLD_NONE)
    return Qnil;
  Point point (wp->w_point);
  wp->w_bufp->folded_goto_eol (point);
  return boole (point.p_point == wp->w_point.p_point);
}

lisp
Fbobp ()
{
  Window *wp = selected_window ();
  return boole (wp->w_bufp->bobp (wp->w_point));
}

lisp
Feobp ()
{
  Window *wp = selected_window ();
  return boole (wp->w_bufp->eobp (wp->w_point));
}

lisp
Fchar_after (lisp point, lisp)
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  Point p (wp->w_point);
  bp->goto_char (p, bp->coerce_to_point (point));
  if (bp->eobp (p))
    return make_char (0);
  return make_char (p.ch ());
}

lisp
Fchar_before (lisp point, lisp)
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  Point p (wp->w_point);
  bp->goto_char (p, bp->coerce_to_point (point));
  if (bp->bobp (p))
    return make_char (0);
  return make_char (p.prevch ());
}

lisp
Ffollowing_char ()
{
  Window *wp = selected_window ();
  if (wp->w_bufp->eobp (wp->w_point))
    return make_char (0);
  return make_char (wp->w_point.ch ());
}

lisp
Fpreceding_char (void)
{
  Window *wp = selected_window ();
  if (wp->w_bufp->bobp (wp->w_point))
    return make_char (0);
  return make_char (wp->w_point.prevch ());
}

lisp
Fcurrent_column ()
{
  Window *wp = selected_window ();
  return make_fixnum (wp->w_bufp->point_column (wp->w_point));
}

lisp
Fcurrent_virtual_column ()
{
  Window *wp = selected_window ();
  return make_fixnum (wp->w_bufp->b_fold_columns == Buffer::FOLD_NONE
                      ? wp->w_bufp->point_column (wp->w_point)
                      : wp->w_bufp->folded_point_column (wp->w_point));
}

lisp
Fgoal_column ()
{
  Window *wp = selected_window ();
  if (wp->w_disp_flags & Window::WDF_GOAL_COLUMN)
    {
      wp->w_goal_column = (wp->w_bufp->b_fold_columns == Buffer::FOLD_NONE
                           ? wp->w_bufp->point_column (wp->w_point)
                           : wp->w_bufp->folded_point_column (wp->w_point));
      wp->w_disp_flags &= ~Window::WDF_GOAL_COLUMN;
    }
  return make_fixnum (wp->w_goal_column);
}

lisp
Fset_goal_column (lisp goal)
{
  long n = fixnum_value (goal);
  if (n < 0)
    FErange_error (goal);
  Window *wp = selected_window ();
  if (wp->w_goal_column != n)
    {
      wp->w_goal_column = n;
      wp->w_disp_flags |= Window::WDF_SET_GOAL_COLUMN;
    }
  wp->w_disp_flags &= ~Window::WDF_GOAL_COLUMN;
  return Qt;
}

lisp
Fgoto_line (lisp goal)
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  bp->linenum_point (wp->w_point, max (1L, fixnum_value (goal)));
  if (wp->w_point.p_point < bp->b_contents.p1)
    bp->goto_char (wp->w_point, bp->b_contents.p1);
  else if (wp->w_point.p_point > bp->b_contents.p2)
    {
      bp->goto_char (wp->w_point, bp->b_contents.p2);
      bp->goto_bol (wp->w_point);
    }
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  return Qt;
}

lisp
Fcurrent_line_number ()
{
  Window *wp = selected_window ();
  return make_fixnum (wp->w_bufp->point_linenum (wp->w_point));
}

lisp
Fgoto_virtual_line (lisp goal)
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  if (bp->b_fold_columns == Buffer::FOLD_NONE)
    bp->linenum_point (wp->w_point, max (1L, fixnum_value (goal)));
  else
    bp->folded_linenum_point (wp->w_point, max (1L, fixnum_value (goal)));
  if (wp->w_point.p_point < bp->b_contents.p1)
    bp->goto_char (wp->w_point, bp->b_contents.p1);
  else if (wp->w_point.p_point > bp->b_contents.p2)
    {
      bp->goto_char (wp->w_point, bp->b_contents.p2);
      if (bp->b_fold_columns == Buffer::FOLD_NONE)
        bp->goto_bol (wp->w_point);
      else
        bp->folded_goto_bol (wp->w_point);
    }
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
  return Qt;
}

lisp
Fcurrent_virtual_line_number ()
{
  Window *wp = selected_window ();
  return make_fixnum (wp->w_bufp->b_fold_columns == Buffer::FOLD_NONE
                      ? wp->w_bufp->point_linenum (wp->w_point)
                      : wp->w_bufp->folded_point_linenum (wp->w_point));
}

void
Buffer::narrow_to_region (point_t p1, point_t p2)
{
  p1 = min (max (p1, point_t (0)), b_nchars);
  p2 = min (max (p2, point_t (0)), b_nchars);
  if (p1 > p2)
    swap (p1, p2);

  Window *wp = selected_window ();
  if (wp->w_bufp == this)
    {
      if (wp->w_point.p_point < p1)
        goto_char (wp->w_point, p1);
      else if (wp->w_point.p_point > p2)
        goto_char (wp->w_point, p2);
    }

  b_contents.p1 = p1;
  b_contents.p2 = p2;
  refresh_buffer ();
}

void
Buffer::widen ()
{
  b_contents.p1 = 0;
  b_contents.p2 = b_nchars;
  refresh_buffer ();
}

lisp
Fnarrow_to_region (lisp from, lisp to)
{
  Buffer *bp = selected_buffer ();
  bp->narrow_to_region (bp->coerce_to_point (from), bp->coerce_to_point (to));
  bp->b_narrow_depth++;
  return Qt;
}

lisp
Fwiden ()
{
  Buffer *bp = selected_buffer ();
  bp->widen ();
  bp->b_narrow_depth = 0;
  return Qt;
}

void
save_excursion::cleanup (int raise_excep)
{
  if (!se_bufp)
    return;
  Buffer *bp = se_bufp;
  se_bufp = 0;
  bp->b_excursion = se_prev;
  Window *wp = selected_window ();
  if (wp->w_bufp != bp && wp->minibuffer_window_p ())
    {
      if (raise_excep)
        FEsimple_error (Ecannot_switch_in_minibuffer_window);
      return;
    }
  wp->set_buffer (bp);
  bp->goto_char (wp->w_point, se_point);
  wp->w_disp_flags |= Window::WDF_GOAL_COLUMN;
}

save_restriction::~save_restriction ()
{
  if (sr_bufp)
    {
      sr_bufp->b_restriction = sr_prev;
      sr_bufp->b_narrow_depth = sr_depth;
      if (sr_depth)
        sr_bufp->narrow_to_region (sr_contents.p1, sr_contents.p2);
      else
        sr_bufp->widen ();
    }
}

lisp
Fcount_column (lisp string, lisp start, lisp lbuffer)
{
  check_string (string);
  int column = (!start || start == Qnil) ? 0 : fixnum_value (start);
  if (column < 0)
    FErange_error (start);
  int tab = ((!lbuffer || lbuffer == Qnil)
             ? app.default_tab_columns
             : Buffer::coerce_to_buffer (lbuffer)->b_tab_columns);
  for (const Char *p = xstring_contents (string), *pe = p + xstring_length (string);
       p < pe; p++)
    column += char_columns (*p, column, tab);
  return make_fixnum (column);
}

lisp
Fcurrent_line_columns ()
{
  Window *wp = selected_window ();
  Buffer *bp = wp->w_bufp;
  Point beg (wp->w_point), end (wp->w_point);
  if (wp->w_bufp->b_fold_columns == Buffer::FOLD_NONE)
    {
      bp->go_bol (beg);
      bp->go_eol (end);
    }
  else
    {
      bp->folded_go_bol (beg);
      bp->folded_go_eol (end);
    }
  return make_fixnum (bp->folded_point_column_1 (min (long (end.p_point + 1),
                                                      bp->b_nchars),
                                                 beg));
}

lisp
Fchar_columns (lisp c)
{
  check_char (c);
  return make_fixnum (char_width (xchar_code (c)));
}

lisp
Fextended_alphabet_char_p (lisp cc)
{
  check_char (cc);
  return boole (iso8859_word_char_p (xchar_code (cc)));
}
