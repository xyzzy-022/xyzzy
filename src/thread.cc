#include "stdafx.h"
#include "ed.h"
#include "thread.h"

worker_thread::worker_thread ()
     : w_hthread (0), w_hlock_event (0), w_hterm_event (0), w_intr (0)
{
}

worker_thread::~worker_thread ()
{
  if (w_hlock_event)
    CloseHandle (w_hlock_event);
  if (w_hterm_event)
    CloseHandle (w_hterm_event);
  if (w_hthread)
    CloseHandle (w_hthread);
}

u_int
worker_thread::thread_entry (void *arg)
{
  worker_thread *wt = (worker_thread *)arg;
  wt->thread_main ();
  SetEvent (wt->w_hterm_event);
  WaitForSingleObject (wt->w_hlock_event, INFINITE);
  delete wt;
  return 0;
}

int
worker_thread::start ()
{
  w_hlock_event = CreateEvent (0, 0, 0, 0);
  if (!w_hlock_event)
    return 0;

  w_hterm_event = CreateEvent (0, 0, 0, 0);
  if (!w_hterm_event)
    return 0;

  u_int threadid;
  w_hthread = (HANDLE)_beginthreadex (0, 0, thread_entry, this, 0, &threadid);
  if (!w_hthread)
    return 0;
  return 1;
}

void
worker_thread::destroy ()
{
  if (w_hthread)
    {
      interrupt ();
      SetEvent (w_hlock_event);
    }
}

int
worker_thread::wait ()
{
  if (!w_hthread)
    return 0;
  return app.kbdq.wait_event (w_hterm_event, 1);
}
