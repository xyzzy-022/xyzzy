#ifndef _xstrlist_h_
#define _xstrlist_h_

class xstring_node: public xlist_node <xstring_node>
{
public:
  char data[1];
  operator const char * () const {return data;}
};

class xstring_list: public xlist <xstring_node>
{
public:
  ~xstring_list ()
    {
      while (!empty_p ())
        delete [] (char *)remove_head ();
    }
  static xstring_node *alloc (const char *s)
    {
      xstring_node *p = (xstring_node *)new char [sizeof *p + strlen (s)];
      strcpy (p->data, s);
      return p;
    }
  void add (const char *s) {add_head (alloc (s));}
  lisp make_list () const
    {
      lisp r = Qnil;
      for (const xstring_node *p = head (); p; p = p->next ())
        r = xcons (make_string (*p), r);
      return r;
    }
};

class xstring_pair_node: public xlist_node <xstring_pair_node>
{
public:
  char *str2;
  char str1[2];
};

class xstring_pair_list: public xlist <xstring_pair_node>
{
public:
  ~xstring_pair_list ()
    {
      while (!empty_p ())
        delete [] (char *)remove_head ();
    }
  static xstring_pair_node *alloc (const char *s1, const char *s2)
    {
      int l1 = strlen (s1);
      xstring_pair_node *p =
        (xstring_pair_node *)new char [sizeof *p + l1 + strlen (s2)];
      p->str2 = p->str1 + l1 + 1;
      strcpy (p->str1, s1);
      strcpy (p->str2, s2);
      return p;
    }
  void add (const char *s1, const char *s2) {add_head (alloc (s1, s2));}
  lisp make_list (int pair) const
    {
      lisp r = Qnil;
      if (pair)
        for (const xstring_pair_node *p = head (); p; p = p->next ())
          r = xcons (xcons (make_string (p->str1),
                            xcons (make_string (p->str2), Qnil)),
                     r);
      else
        for (const xstring_pair_node *p = head (); p; p = p->next ())
          r = xcons (make_string (p->str1), r);
      return r;
    }
};

#endif /* _xstrlist_h_ */
