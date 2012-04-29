#include "stdafx.h"
#include "xlist.h"

xlistP::x_node_ptr
xlistP::add_head (x_node_ptr p)
{
  p->xn_prev = 0;
  p->xn_next = x_head;
  if (!null (x_head))
    x_head->xn_prev = p;
  else if (null (x_tail))
    x_tail = p;
  x_head = p;
  return p;
}

xlistP::x_node_ptr
xlistP::add_tail (x_node_ptr p)
{
  p->xn_next = 0;
  p->xn_prev = x_tail;
  if (!null (x_tail))
    x_tail->xn_next = p;
  else if (null (x_head))
    x_head = p;
  x_tail = p;
  return p;
}

xlistP::x_node_ptr
xlistP::insert_before (x_node_ptr p, x_node_ptr base)
{
  if (!null (base->xn_prev))
    base->xn_prev->xn_next = p;
  else
    x_head = p;
  p->xn_prev = base->xn_prev;
  base->xn_prev = p;
  p->xn_next = base;
  return p;
}

xlistP::x_node_ptr
xlistP::insert_after (x_node_ptr p, x_node_ptr base)
{
  if (!null (base->xn_next))
    base->xn_next->xn_prev = p;
  else
    x_tail = p;
  p->xn_next = base->xn_next;
  base->xn_next = p;
  p->xn_prev = base;
  return p;
}

xlistP::x_node_ptr
xlistP::remove (x_node_ptr p)
{
  if (!null (p->xn_next))
    p->xn_next->xn_prev = p->xn_prev;
  else
    x_tail = p->xn_prev;
  if (!null (p->xn_prev))
    p->xn_prev->xn_next = p->xn_next;
  else
    x_head = p->xn_next;
  return p;
}

int
xlistP::length () const
{
  int l = 0;
  for (x_node_ptr p = head (); p; p = p->xn_next, l++)
    ;
  return l;
}
