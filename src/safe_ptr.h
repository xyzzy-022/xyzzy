#ifndef _SAFE_PTR_H_
# define _SAFE_PTR_H_

template <class T>
class safe_ptr
{
  T *vec;

  safe_ptr (const safe_ptr &);
  safe_ptr &operator = (const safe_ptr &);
public:
  safe_ptr (T *p) : vec (p) {}
  ~safe_ptr () {delete [] vec;}
  operator T * () const {return vec;}
};

#endif
