#ifndef _list_h_
# define _list_h_

inline lisp Fcaar (lisp x) {return Fcar (Fcar (x));}
inline lisp Fcadr (lisp x) {return Fcar (Fcdr (x));}
inline lisp Fcdar (lisp x) {return Fcdr (Fcar (x));}
inline lisp Fcddr (lisp x) {return Fcdr (Fcdr (x));}
inline lisp Fcaaar (lisp x) {return Fcar (Fcar (Fcar (x)));}
inline lisp Fcaadr (lisp x) {return Fcar (Fcar (Fcdr (x)));}
inline lisp Fcadar (lisp x) {return Fcar (Fcdr (Fcar (x)));}
inline lisp Fcaddr (lisp x) {return Fcar (Fcdr (Fcdr (x)));}
inline lisp Fcdaar (lisp x) {return Fcdr (Fcar (Fcar (x)));}
inline lisp Fcdadr (lisp x) {return Fcdr (Fcar (Fcdr (x)));}
inline lisp Fcddar (lisp x) {return Fcdr (Fcdr (Fcar (x)));}
inline lisp Fcdddr (lisp x) {return Fcdr (Fcdr (Fcdr (x)));}
inline lisp Fcaaaar (lisp x) {return Fcar (Fcar (Fcar (Fcar (x))));}
inline lisp Fcaaadr (lisp x) {return Fcar (Fcar (Fcar (Fcdr (x))));}
inline lisp Fcaadar (lisp x) {return Fcar (Fcar (Fcdr (Fcar (x))));}
inline lisp Fcaaddr (lisp x) {return Fcar (Fcar (Fcdr (Fcdr (x))));}
inline lisp Fcadaar (lisp x) {return Fcar (Fcdr (Fcar (Fcar (x))));}
inline lisp Fcadadr (lisp x) {return Fcar (Fcdr (Fcar (Fcdr (x))));}
inline lisp Fcaddar (lisp x) {return Fcar (Fcdr (Fcdr (Fcar (x))));}
inline lisp Fcadddr (lisp x) {return Fcar (Fcdr (Fcdr (Fcdr (x))));}
inline lisp Fcdaaar (lisp x) {return Fcdr (Fcar (Fcar (Fcar (x))));}
inline lisp Fcdaadr (lisp x) {return Fcdr (Fcar (Fcar (Fcdr (x))));}
inline lisp Fcdadar (lisp x) {return Fcdr (Fcar (Fcdr (Fcar (x))));}
inline lisp Fcdaddr (lisp x) {return Fcdr (Fcar (Fcdr (Fcdr (x))));}
inline lisp Fcddaar (lisp x) {return Fcdr (Fcdr (Fcar (Fcar (x))));}
inline lisp Fcddadr (lisp x) {return Fcdr (Fcdr (Fcar (Fcdr (x))));}
inline lisp Fcdddar (lisp x) {return Fcdr (Fcdr (Fcdr (Fcar (x))));}
inline lisp Fcddddr (lisp x) {return Fcdr (Fcdr (Fcdr (Fcdr (x))));}

int xlist_length (lisp list);
lisp find_keyword (lisp var, lisp list, lisp defalt = Qnil);
lisp safe_find_keyword (lisp var, lisp list, lisp defalt = Qnil);

inline int
find_keyword_bool (lisp var, lisp list, int defalt = 0)
{
  return find_keyword (var, list, defalt ? Qt : Qnil) != Qnil;
}

inline int
find_keyword_bool (lisp var, lisp list, lisp defalt)
{
  return find_keyword (var, list, defalt) != Qnil;
}

inline int
find_keyword_int (lisp var, lisp list, int defalt = 0)
{
  lisp x = find_keyword (var, list, Qnil);
  return x == Qnil ? defalt : fixnum_value (x);
}

inline int
find_keyword_int (lisp var, lisp list, lisp defalt)
{
  lisp x = find_keyword (var, list, Qnil);
  return fixnum_value (x == Qnil ? defalt : x);
}

inline double
find_keyword_float (lisp var, lisp list, double defalt = 0)
{
  lisp x = find_keyword (var, list, Qnil);
  return x == Qnil ? defalt : coerce_to_double_float (x);
}

lisp subseq_list (lisp, lisp, lisp);

lisp memq (lisp, lisp);
int delq (lisp, lisp *);
lisp assq (lisp, lisp);
int delassq (lisp, lisp *);
lisp make_list (lisp x, ...);

inline lisp
list (lisp x)
{
  return xcons (x, Qnil);
}

inline lisp
list (lisp x, lisp y)
{
  return xcons (x, list (y));
}

inline lisp
list (lisp x, lisp y, lisp z)
{
  return xcons (x, list (y, z));
}

inline lisp
append (lisp x, lisp y)
{
  if (y == Qnil)
    return x;
  return Fappend (list (x, y));
}

#endif
