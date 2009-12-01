#ifndef _buffer_bar_h_
#define _buffer_bar_h_

#include "dockbar.h"
#include "DnD.h"

class buffer_bar: public tab_bar
{
private:
  static buffer_bar *b_bar;
  static Buffer *b_last_buffer;
  buffer_bar_drop_target b_drop_target;
  int b_drop_index;
  enum {DROP_TIMER_ID = 10};

  buffer_bar (dock_frame &);
  virtual ~buffer_bar () {b_bar = 0;}
  virtual int notify (NMHDR *, LRESULT &);
  int create (HWND);
  Buffer *nth (int i) const {return (Buffer *)tab_bar::nth (i);}
  Buffer *current () const;
  int insert (const Buffer *, int);
  int modify (const Buffer *, int);
  static char *set_buffer_name (const Buffer *, char *, int);
  virtual int need_text (TOOLTIPTEXT &);
  virtual void draw_item (const draw_item_struct &);
  void insert_buffers ();
  void delete_buffer (Buffer *);
  virtual void post_nc_destroy () {b_bar = 0;}
  Buffer *next_buffer (Buffer *, int) const;
  Buffer *top_buffer () const;
  Buffer *bottom_buffer () const;
  static void tab_color (const Buffer *, COLORREF &, COLORREF &);
  lisp buffer_list () const;
protected:
  virtual lisp context_menu (int);
  virtual LRESULT wndproc (UINT, WPARAM, LPARAM);
public:
  static int make_instance ();
  static void buffer_deleted (Buffer *bp)
    {if (b_bar) b_bar->delete_buffer (bp);}
  virtual void update_ui ();
  static Buffer *next_buffer (Buffer *bp)
    {return b_bar ? b_bar->next_buffer (bp, 1) : 0;}
  static Buffer *prev_buffer (Buffer *bp)
    {return b_bar ? b_bar->next_buffer (bp, -1) : 0;}
  static Buffer *get_top_buffer ()
    {return b_bar ? b_bar->top_buffer () : 0;}
  static Buffer *get_bottom_buffer ()
    {return b_bar ? b_bar->bottom_buffer () : 0;}
  void drag_enter (int, int);
  void drag_over (int, int);
  void drag_leave ();
  static lisp list_buffers ()
    {return b_bar ? b_bar->buffer_list () : 0;}
};

#endif /* _buffer_bar_h_ */
