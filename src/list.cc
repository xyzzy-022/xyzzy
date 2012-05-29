#include "stdafx.h"
#include "ed.h"
#include "sequence.h"

int
xlist_length (lisp list)
{
  int l;
  for (l = 0; consp (list); l++, list = xcdr (list))
    QUIT;
  return l;
}

lisp
find_keyword (lisp var, lisp list, lisp defalt)
{
  for (lisp l = list; consp (l); l = xcdr (l))
    {
      lisp key = xcar (l);
      if (!symbolp (key))
        FEinvalid_keyword_list (list);
      l = xcdr (l);
      if (!consp (l))
        FEinvalid_keyword_list (list);
      if (var == key)
        return xcar (l);
    }
  return defalt;
}

lisp
safe_find_keyword (lisp var, lisp list, lisp defalt)
{
  for (lisp l = list; consp (l); l = xcdr (l))
    {
      lisp key = xcar (l);
      if (!symbolp (key))
        break;
      l = xcdr (l);
      if (!consp (l))
        break;
      if (var == key)
        return xcar (l);
    }
  return defalt;
}

lisp
memq (lisp item, lisp list)
{
  for (; consp (list); list = xcdr (list))
    if (xcar (list) == item)
      return list;
  return 0;
}

int
delq (lisp item, lisp *list)
{
  for (; consp (*list); list = &xcdr (*list))
    if (xcar (*list) == item)
      {
        *list = xcdr (*list);
        return 1;
      }
  return 0;
}

lisp
assq (lisp item, lisp alist)
{
  for (; consp (alist); alist = xcdr (alist))
    {
      lisp x = xcar (alist);
      if (consp (x) && xcar (x) == item)
        return x;
    }
  return 0;
}

int
delassq (lisp item, lisp *alist)
{
  for (; consp (*alist); alist = &xcdr (*alist))
    {
      lisp x = xcar (*alist);
      if (consp (x) && xcar (x) == item)
        {
          *alist = xcdr (*alist);
          return 1;
        }
    }
  return 0;
}

lisp
make_list (lisp x, ...)
{
  lisp top, p;
  top = p = xcons (x, Qnil);

  va_list ap;
  va_start (ap, x);
  while (1)
    {
      x = va_arg (ap, lisp);
      if (!x)
        break;
      xcdr (p) = xcons (x, Qnil);
      p = xcdr (p);
    }
  va_end (ap);
  return top;
}

// 15.1 CONSES

lisp
Fcar (lisp list)
{
  if (list == Qnil)
    return list;
  check_cons (list);
  return xcar (list);
}

lisp
Fcdr (lisp list)
{
  if (list == Qnil)
    return list;
  check_cons (list);
  return xcdr (list);
}

lisp
Fcons (lisp x, lisp y)
{
  return xcons (x, y);
}

static int
tree_equal (lisp x, lisp y, seq_testproc &t)
{
  if (consp (x))
    return (consp (y)
            && tree_equal (xcar (x), xcar (y), t)
            && tree_equal (xcdr (x), xcdr (y), t));
  return !consp (y) && t.test (x, y);
}

lisp
Ftree_equal (lisp x, lisp y, lisp keys)
{
  seq_testproc t (Qnil, keys);
  return boole (tree_equal (x, y, t));
}

// 15.2 LISTS

lisp
Fendp (lisp object)
{
  if (object == Qnil)
    return Qt;
  if (!consp (object))
    FEtype_error (object, Qlist);
  return Qnil;
}

lisp
Flist_length (lisp list)
{
  if (list == Qnil)
    return make_fixnum (0);
  if (!consp (list))
    FEtype_error (list, Qlist);

  int n = 0;
  for (lisp fast = list, slow = list;;
       n += 2, fast = Fcddr (fast), slow = Fcdr (slow))
    {
      if (Fendp (fast) != Qnil)
        return make_fixnum (n);
      if (Fendp (Fcdr (fast)) != Qnil)
        return make_fixnum (n + 1);
      if (n && fast == slow)
        return Qnil;
    }
}

lisp
Fnth (lisp n, lisp list)
{
  return Fcar (Fnthcdr (n, list));
}

lisp
Fnthcdr (lisp n, lisp list)
{
  int nth = fixnum_value (n);
  if (nth < 0)
    FErange_error (n);

  if (list == Qnil)
    return list;
  check_cons (list);

  while (nth > 0)
    {
      if (!consp (list))
        return Qnil;
      QUIT;
      nth--;
      list = xcdr (list);
    }
  return list;
}

lisp
Flast (lisp list, lisp n)
{
  lisp l = Flist_length (list);
  if (l == Qnil)
    FEprogram_error (Eargument_is_circle);
  int ll = fixnum_value (l);
  int nn;
  if (!n || n == Qnil)
    nn = 1;
  else
    {
      nn = fixnum_value (n);
      if (nn < 0)
        FErange_error (n);
    }
  ll -= nn;
  if (!nn)
    ll--;
  if (ll > 0)
    list = Fnthcdr (make_fixnum (ll), list);
  return nn ? list : Fcdr (list);
}

lisp
Flist (lisp args)
{
  if (!consp (args))
    return Qnil;
  lisp p = xcons (xcar (args), Qnil);
  lisp result = p;
  while (1)
    {
      args = xcdr (args);
      if (!consp (args))
        break;
      QUIT;
      xcdr (p) = xcons (xcar (args), Qnil);
      p = xcdr (p);
    }
  return result;
}

lisp
Flist_star (lisp args)
{
  if (!consp (args))
    FEtoo_few_arguments ();
  if (!consp (xcdr (args)))
    return xcar (args);
  lisp p = xcons (xcar (args), Qnil);
  lisp result = p;
  while (1)
    {
      args = xcdr (args);
      if (!consp (xcdr (args)))
        break;
      QUIT;
      xcdr (p) = xcons (xcar (args), Qnil);
      p = xcdr (p);
    }
  xcdr (p) = xcar (args);
  return result;
}

lisp
Fmake_list (lisp size, lisp keys)
{
  int l = fixnum_value (size);
  if (l < 0)
    FErange_error (size);
  lisp init = find_keyword (Kinitial_element, keys);
  lisp p = Qnil;
  for (int i = 0; i < l; i++)
    p = xcons (init, p);
  return p;
}

lisp
Fappend (lisp lists)
{
  if (!consp (lists))
    return Qnil;

  lisp x;
  while (!consp (x = xcar (lists)))
    {
      if (!consp (lists = xcdr (lists)))
        return x;
      if (x != Qnil)
        FEtype_error (x, Qlist);
      QUIT;
    }

  if (!consp (lists = xcdr (lists)))
    return x;

  lisp head = x;
  lisp cp = xcons (xcar (x), Qnil);
  lisp result = cp;
  for (x = xcdr (x); consp (x); x = xcdr (x), cp = xcdr (cp))
    xcdr (cp) = xcons (xcar (x), Qnil);
  if (x != Qnil)
    FEtype_error (head, Qlist);

  while (consp (xcdr (lists)))
    {
      while (1)
        {
          x = xcar (lists);
          if (!consp (xcdr (lists)))
            {
              xcdr (cp) = x;
              return result;
            }
          if (consp (x))
            break;
          QUIT;
          if (x != Qnil)
            FEtype_error (x, Qlist);
          lists = xcdr (lists);
        }

      do
        {
          QUIT;
          xcdr (cp) = xcons (xcar (x), Qnil);
          cp = xcdr (cp);
          x = xcdr (x);
        }
      while (consp (x));
      if (x != Qnil)
        FEtype_error (xcar (lists), Qlist);
      lists = xcdr (lists);
    }
  xcdr (cp) = xcar (lists);
  return result;
}

lisp
Fcopy_list (lisp list)
{
  if (list == Qnil)
    return Qnil;
  check_cons (list);
  lisp result = xcons (xcar (list), Qnil);
  lisp p = result;
  while (1)
    {
      list = xcdr (list);
      if (!consp (list))
        break;
      QUIT;
      xcdr (p) = xcons (xcar (list), Qnil);
      p = xcdr (p);
    }
  xcdr (p) = list;
  return result;
}

lisp
Fcopy_alist (lisp list)
{
  if (list == Qnil)
    return Qnil;
  check_cons (list);
  lisp al = xcar (list);
  lisp result = xcons (consp (al) ? xcons (xcar (al), xcdr (al)) : al, Qnil);
  lisp p = result;
  while (1)
    {
      list = xcdr (list);
      if (!consp (list))
        break;
      QUIT;
      al = xcar (list);
      xcdr (p) = xcons (consp (al) ? xcons (xcar (al), xcdr (al)) : al, Qnil);
      p = xcdr (p);
    }
  return result;
}

static lisp
copy_tree (lisp object, lisp seen)
{
  if (!consp (object))
    return object;
  QUIT;

  // circular list.
  lisp copy = assq (object, seen);
  if (copy)
    return xcdr (copy);

  copy = xcons (Qnil, Qnil);
  seen = xcons (xcons (object, copy), seen);
  xcar (copy) = copy_tree (xcar (object), seen);
  xcdr (copy) = copy_tree (xcdr (object), seen);

  return copy;
}

lisp
Fcopy_tree (lisp object)
{
  return copy_tree (object, Qnil);
}

lisp
Frevappend (lisp x, lisp y)
{
  while (consp (x))
    {
      y = xcons (xcar (x), y);
      x = xcdr (x);
    }
  return y;
}

lisp
Fnconc (lisp lists)
{
  for (; consp (lists); lists = xcdr (lists))
    {
      QUIT;
      lisp x = xcar (lists);
      if (consp (x))
        {
          lisp r = x;
          lisp splice = r;
          for (lisp elements = xcdr (lists); consp (elements);
               elements = xcdr (elements))
            {
              QUIT;
              for (; consp (xcdr (splice)); splice = xcdr (splice))
                QUIT;
              x = xcar (elements);
              if (consp (x))
                {
                  xcdr (splice) = x;
                  splice = x;
                }
              else if (x == Qnil)
                xcdr (splice) = Qnil;
              else if (xcdr (elements) != Qnil)
                FEtype_error (x, Qlist);
              else
                {
                  xcdr (splice) = x;
                  break;
                }
            }
          return r;
        }
      else if (x != Qnil)
        {
          if (xcdr (lists) != Qnil)
            FEtype_error (x, Qlist);
          return x;
        }
    }
  return Qnil;
}

#pragma optimize ("aw", off)
lisp
Fnreconc (lisp x, lisp y)
{
  while (consp (x))
    {
      lisp next = xcdr (x);
      xcdr (x) = y;
      y = x;
      x = next;
      QUIT;
    }
  return y;
}
#pragma optimize ("", on)

// 15.3

lisp
Frplaca (lisp x, lisp y)
{
  check_cons (x);
  xcar (x) = y;
  return x;
}

lisp
Frplacd (lisp x, lisp y)
{
  check_cons (x);
  xcdr (x) = y;
  return x;
}

// 15.4

static lisp
subst (lisp tree, lisp newi, test_proc &test)
{
  if (test.test (tree))
    return newi;
  if (!consp (tree))
    return tree;
  QUIT;
  lisp a = subst (xcar (tree), newi, test);
  protect_gc gcpro (a);
  lisp d = subst (xcdr (tree), newi, test);
  return (a == xcar (tree) && d == xcdr (tree)) ? tree : xcons (a, d);
}

lisp
Fsubst (lisp newi, lisp oldi, lisp tree, lisp keys)
{
  seq_testproc t (oldi, keys);
  return subst (tree, newi, t);
}

lisp
Fsubst_if (lisp newi, lisp oldi, lisp tree, lisp keys)
{
  seq_testproc_if t (oldi, keys);
  return subst (tree, newi, t);
}

lisp
Fsubst_if_not (lisp newi, lisp oldi, lisp tree, lisp keys)
{
  seq_testproc_if_not t (oldi, keys);
  return subst (tree, newi, t);
}

static lisp
nsubst (lisp tree, lisp newi, test_proc &test)
{
  if (test.test (tree))
    return newi;
  if (consp (tree))
    {
      QUIT;
      xcar (tree) = nsubst (xcar (tree), newi, test);
      xcdr (tree) = nsubst (xcdr (tree), newi, test);
    }
  return tree;
}

lisp
Fnsubst (lisp newi, lisp oldi, lisp tree, lisp keys)
{
  seq_testproc t (oldi, keys);
  return nsubst (tree, newi, t);
}

lisp
Fnsubst_if (lisp newi, lisp oldi, lisp tree, lisp keys)
{
  seq_testproc_if t (oldi, keys);
  return nsubst (tree, newi, t);
}

lisp
Fnsubst_if_not (lisp newi, lisp oldi, lisp tree, lisp keys)
{
  seq_testproc_if_not t (oldi, keys);
  return nsubst (tree, newi, t);
}

static lisp
sublis (lisp alist, lisp tree, seq_testproc &t)
{
  for (lisp al = alist; consp (al); al = xcdr (al))
    {
      lisp x = xcar (al);
      if (consp (x) && t.test (t.call_keyfn (tree), xcar (x)))
        return xcdr (x);
    }
  if (!consp (tree))
    return tree;
  QUIT;
  lisp a = sublis (alist, xcar (tree), t);
  protect_gc gcpro (a);
  lisp d = sublis (alist, xcdr (tree), t);
  return (a == xcar (tree) && d == xcdr (tree)) ? tree : xcons (a, d);
}

lisp
Fsublis (lisp alist, lisp tree, lisp keys)
{
  seq_testproc t (Qnil, keys);
  return sublis (alist, tree, t);
}

static lisp
nsublis (lisp alist, lisp tree, seq_testproc &t)
{
  for (lisp a = alist; consp (a); a = xcdr (a))
    {
      lisp x = xcar (a);
      if (consp (x) && t.test (t.call_keyfn (tree), xcar (x)))
        return xcdr (x);
    }
  if (consp (tree))
    {
      QUIT;
      xcar (tree) = nsublis (alist, xcar (tree), t);
      xcdr (tree) = nsublis (alist, xcdr (tree), t);
    }
  return tree;
}

lisp
Fnsublis (lisp alist, lisp tree, lisp keys)
{
  seq_testproc t (Qnil, keys);
  return nsublis (alist, tree, t);
}

// 15.5

static lisp
member (lisp list, test_proc &test)
{
  for (; consp (list); list = xcdr (list))
    if (test.test (xcar (list)))
      return list;
  return Qnil;
}

lisp
Fmember (lisp item, lisp list, lisp keys)
{
  seq_testproc t (item, keys);
  return member (list, t);
}

lisp
Fmember_if (lisp pred, lisp list, lisp keys)
{
  seq_testproc_if t (pred, keys);
  return member (list, t);
}

lisp
Fmember_if_not (lisp pred, lisp list, lisp keys)
{
  seq_testproc_if_not t (pred, keys);
  return member (list, t);
}

lisp
Fadjoin (lisp item, lisp list, lisp keys)
{
  lisp f = find_keyword (Kkey, keys);
  lisp x = f == Qnil ? item : funcall_1 (f, item);
  protect_gc gcpro (x);
  return (Fmember (x, list, keys) == Qnil ? xcons (item, list) : list);
}

// 15.6

lisp
Facons (lisp key, lisp datum, lisp alist)
{
  return xcons (xcons (key, datum), alist);
}

lisp
Fpairlis (lisp keys, lisp data, lisp alist)
{
  if (!alist)
    alist = Qnil;
  for (; consp (keys); keys = xcdr (keys), data = xcdr (data))
    {
      if (!consp (data))
        FEprogram_error (Elists_length_different);
      alist = Facons (xcar (keys), xcar (data), alist);
    }
  if (consp (data))
    FEprogram_error (Elists_length_different);
  return alist;
}

static lisp
assoc (lisp alist, test_proc &test)
{
  for (lisp al = alist; consp (al); al = xcdr (al))
    if (consp (al))
      {
        lisp x = xcar (al);
        if (consp (x) && test.test (xcar (x)))
          return x;
      }
  return Qnil;
}

lisp
Fassoc (lisp key, lisp alist, lisp keys)
{
  seq_testproc t (key, keys);
  return assoc (alist, t);
}

lisp
Fassoc_if (lisp pred, lisp alist, lisp keys)
{
  seq_testproc_if t (pred, keys);
  return assoc (alist, t);
}

lisp
Fassoc_if_not (lisp pred, lisp alist, lisp keys)
{
  seq_testproc_if_not t (pred, keys);
  return assoc (alist, t);
}

static lisp
rassoc (lisp alist, test_proc &test)
{
  for (lisp al = alist; consp (al); al = xcdr (al))
    if (consp (al))
      {
        lisp x = xcar (al);
        if (consp (x) && test.test (xcdr (x)))
          return x;
      }
  return Qnil;
}

lisp
Frassoc (lisp key, lisp alist, lisp keys)
{
  seq_testproc t (key, keys);
  return rassoc (alist, t);
}

lisp
Frassoc_if (lisp pred, lisp alist, lisp keys)
{
  seq_testproc_if t (pred, keys);
  return rassoc (alist, t);
}

lisp
Frassoc_if_not (lisp pred, lisp alist, lisp keys)
{
  seq_testproc_if_not t (pred, keys);
  return rassoc (alist, t);
}

lisp
subseq_list (lisp list, lisp lstart, lisp lend)
{
  int start, end;
  seq_start_end (xlist_length (list), start, end, lstart, lend);
  if (start == end)
    return Qnil;
  lisp src = Fnthcdr (lstart, list);
  lisp dst = xcons (Fcar (src), Qnil);
  lisp p = dst;
  while (++start < end)
    {
      src = Fcdr (src);
      xcdr (p) = xcons (Fcar (src), Qnil);
      p = xcdr (p);
    }
  return dst;
}

static int
tree_find (lisp tree, test_proc &test)
{
  QUIT;
  return (test.test (tree)
          || (consp (tree)
              && (tree_find (xcar (tree), test)
                  || tree_find (xcdr (tree), test))));
}

lisp
Fsi_tree_find (lisp item, lisp tree, lisp keys)
{
  seq_testproc t (item, keys);
  return boole (tree_find (tree, t));
}

static int
tree_count (lisp tree, test_proc &test)
{
  QUIT;
  if (test.test (tree))
    return 1;
  if (!consp (tree))
    return 0;
  return tree_count (xcar (tree), test) + tree_count (xcdr (tree), test);
}

lisp
Fsi_tree_count (lisp item, lisp tree, lisp keys)
{
  seq_testproc t (item, keys);
  return make_fixnum (tree_count (tree, t));
}

