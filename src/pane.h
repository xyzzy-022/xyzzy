#ifndef _PANE_H_
#define _PANE_H_

class pane;
class splitter;

#define XPIS_LEFT    0
#define XPIS_TOP     1
#define XPIS_RIGHT   2
#define XPIS_BOTTOM  3
#define XPIS_NOMOVE  4
#define XPIS_POSMASK 7
#define XPIS_ORDMASK 0x30
#define XPIS_OUTSIDE 0x10
#define XPIS_INSIDE  0x20
#define XPIS_NOORDER 0x30
#define XPIS_GROUP   0x40

#define XPIS_MASK   3

typedef HANDLE XPIHANDLE;

class splitter
{
  pane *s_head;
  pane *s_tail;
  int s_in_resize;
  int s_terminating;
  HWND s_hwnd;
  HWND s_hwnd_frame;
  RECT s_rect;

  void link_pane (pane *);
  void unlink_pane (pane *);
  pane *find_pane (const POINT &) const;

public:
  splitter ();
  ~splitter ();

  pane *create_pane (HWND, int, int, int);
  pane *add_pane (HWND, int, int, int);
  void remove_pane (pane *);
  int set_cursor ();
  void resize (const RECT &);
  void calc_geometry ();
  void calc_geometry (pane *, const RECT &);
  int move_splitter (int, int);
  void recalc_order (pane *, int);
  HWND hwnd () const {return s_hwnd;}
  pane *find_pane (XPIHANDLE) const;
  pane *find_pane (HWND) const;
  void init (HWND, HWND);
};

class pane
{
  pane *p_prev;
  pane *p_next;

  splitter *p_parent;
  HWND p_hwnd;

  int p_flags;
  int p_vert;
  int p_cursize;
  int p_idealsize;
  int p_minsize;
  int p_maxsize;
  int p_step;
  SIZE p_reqsize;
  RECT p_rect;
  int p_initialized;

  int calc_size (const POINT &, int, int, int, int, int) const;
  void calc_rect (RECT &, int) const;
public:
  pane (splitter *, HWND, int, int, int);
  ~pane () {}
  void *operator new (size_t size) {return malloc (size);}
  void operator delete (void *p) {free (p);}

  int pflags () const {return p_flags & XPIS_MASK;}
  pane *prev () const {return p_prev;}
  pane *next () const {return p_next;}
  pane *&prev () {return p_prev;}
  pane *&next () {return p_next;}
  int good () const {return p_initialized;}
  const RECT &rect () const {return p_rect;}
  HWND hwnd () const {return p_hwnd;}

  void calc_geometry (RECT &);
  void set_cursor () const
    {SetCursor (LoadCursor (0, p_vert ? IDC_SIZEWE : IDC_SIZENS));}
  int move_splitter (const POINT &);
  int set_size (int, int, int, int);
  int set_pos (int);
};

#endif /* _PANE_H_ */
