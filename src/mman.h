#ifndef _MMAN_H_
# define _MMAN_H_

class mapf
{
  HANDLE mf_hfile;
  HANDLE mf_hmap;
  DWORD mf_size;
  void *mf_base;

  void init ()
    {
      mf_hfile = INVALID_HANDLE_VALUE;
      mf_hmap = 0;
      mf_base = 0;
    }

public:
  mapf () {init ();}
  ~mapf () {close ();}
  void close ();
  int open (const char *, int = FILE_FLAG_SEQUENTIAL_SCAN, int = 0);
  const void *base () const {return mf_base;}
  DWORD size () const {return mf_size;}
  operator HANDLE () const {return mf_hfile;}
};

#endif
