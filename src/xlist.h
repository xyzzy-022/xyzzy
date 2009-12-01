#ifndef _xlist_h_
#define _xlist_h_

class xlistP
{
public:
  struct xlist_nodeP {xlist_nodeP *xn_prev, *xn_next;};
private:
  typedef xlist_nodeP *x_node_ptr;

  x_node_ptr x_head, x_tail;
public:
  xlistP () : x_head (0), x_tail (0) {}
  static int null (const x_node_ptr p) {return !p;}
  x_node_ptr head () const {return x_head;}
  x_node_ptr tail () const {return x_tail;}
  x_node_ptr add_head (x_node_ptr);
  x_node_ptr add_tail (x_node_ptr);
  x_node_ptr insert_before (x_node_ptr, x_node_ptr);
  x_node_ptr insert_after (x_node_ptr, x_node_ptr);
  x_node_ptr remove (x_node_ptr);
  x_node_ptr remove_head () {return remove (head ());}
  x_node_ptr remove_tail () {return remove (tail ());}
  x_node_ptr move_to_head (x_node_ptr p)
    {return p != head () ? add_head (remove (p)) : p;}
  x_node_ptr move_to_tail (x_node_ptr p)
    {return p != tail () ? add_tail (remove (p)) : p;}
  void empty () {x_head = x_tail = 0;}
  int empty_p () const {return !x_head;}
  int length () const;
};

template <class T>
class xlist: public xlistP
{
private:
  typedef T *node_ptr;
public:
  node_ptr head () const {return node_ptr (xlistP::head ());}
  node_ptr tail () const {return node_ptr (xlistP::tail ());}
  node_ptr add_head (node_ptr p) {return node_ptr (xlistP::add_head (p));}
  node_ptr add_tail (node_ptr p) {return node_ptr (xlistP::add_tail (p));}
  node_ptr insert_before (node_ptr p, node_ptr q)
    {return node_ptr (xlistP::insert_before (p, q));}
  node_ptr insert_after (node_ptr p, node_ptr q)
    {return node_ptr (xlistP::insert_after (p, q));}
  node_ptr remove (node_ptr p) {return node_ptr (xlistP::remove (p));}
  node_ptr remove_head () {return node_ptr (xlistP::remove_head ());}
  node_ptr remove_tail () {return node_ptr (xlistP::remove_tail ());}
  node_ptr move_to_head (node_ptr p)
    {return node_ptr (xlistP::move_to_head (p));}
  node_ptr move_to_tail (node_ptr p)
    {return node_ptr (xlistP::move_to_tail (p));}
};

template <class T>
class xlist_node: public xlistP::xlist_nodeP
{
public:
  T *prev () const {return (T *)xn_prev;}
  T *next () const {return (T *)xn_next;}
};

#endif /* _xlist_h_ */
