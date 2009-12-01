#ifndef _thread_h_
#define _thread_h_

class worker_thread
{
private:
  HANDLE w_hthread;
  HANDLE w_hlock_event;
  HANDLE w_hterm_event;
  int w_intr;
protected:
  virtual void thread_main () = 0;
  int interrupted () const {return w_intr;}
  worker_thread ();
  virtual ~worker_thread ();
private:
  static u_int __stdcall thread_entry (void *);
public:
  int start ();
  int wait ();
  void interrupt () {w_intr = 1;}
  void destroy ();
};

template <class T>
class worker_thread_helper
{
  T *wt;
public:
  worker_thread_helper () : wt (0) {}
  worker_thread_helper (T *p) : wt (p) {}
  void attach (T *p) {wt = p;}
  ~worker_thread_helper () {if (wt) wt->destroy ();}
  operator T * () const {return wt;}
  T *operator -> () const {return wt;}
};

#endif /* _thread_h_ */
