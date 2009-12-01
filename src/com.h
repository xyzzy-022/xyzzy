#ifndef _com_h_
# define _com_h_

template <class T>
class safe_com
{
protected:
  T *u;
public:
  safe_com () : u (0) {}
  safe_com (T *u_) : u (u_) {}
  ~safe_com () {if (u) u->Release ();}
  T **operator & () {return &u;}
  operator T * () {return u;}
  T *operator -> () const {return u;}
};

#ifdef _SHLOBJ_H_

class safe_idl
{
protected:
  ITEMIDLIST *idl;
  IMalloc *ialloc;
public:
  safe_idl (IMalloc *ialloc_) : idl (0), ialloc (ialloc_) {}
  ~safe_idl () {if (idl) ialloc->Free (idl);}
  ITEMIDLIST **operator & () {return &idl;}
  operator ITEMIDLIST * () {return idl;}
};

class safe_vidl
{
protected:
  ITEMIDLIST **idls;
  int nidls;
  IMalloc *ialloc;
public:
  safe_vidl (IMalloc *ialloc_, ITEMIDLIST **idls_, int nidls_)
       : ialloc (ialloc_), idls (idls_), nidls (nidls_)
    {
      bzero (idls, sizeof *idls * nidls);
    }
  ~safe_vidl ()
    {
      for (int i = 0; i < nidls; i++)
        if (idls[i])
          ialloc->Free (idls[i]);
    }
};

#endif /* _SHLOBJ_H_ */

static inline void
ole_error (HRESULT r)
{
  if (!SUCCEEDED (r))
    FEsimple_win32_error (r);
}

static inline void
ole_error (HRESULT r, lisp cause)
{
  if (!SUCCEEDED (r))
    FEsimple_win32_error (r, cause);
}

#endif
