#include "stdafx.h"
#include "ed.h"
#include "listen.h"

void
lwait_object::cleanup ()
{
  if (hevent)
    {
      SetEvent (hevent);
      CloseHandle (hevent);
      hevent = 0;
    }
}

static void
decref_waitobj (lisp lwaitobj)
{
  if (xwait_object_hevent (lwaitobj)
      && !--xwait_object_ref (lwaitobj))
    ((lwait_object *)lwaitobj)->cleanup ();
}

void
Buffer::cleanup_waitobj_list ()
{
  for (lisp p = lwaitobj_list; consp (p); p = xcdr (p))
    {
      lisp lwaitobj = xcar (p);
      if (wait_object_p (lwaitobj))
        decref_waitobj (lwaitobj);
    }
  lwaitobj_list = Qnil;
}

lisp
Fsi_create_wait_object ()
{
  lisp lwaitobj = make_wait_object ();
  xwait_object_hevent (lwaitobj) = CreateEvent (0, 0, 0, 0);
  if (!xwait_object_hevent (lwaitobj))
    FEsimple_win32_error (GetLastError ());
  xwait_object_ref (lwaitobj) = 0;
  return lwaitobj;
}

lisp
Fsi_add_wait_object (lisp lwaitobj, lisp lbuffer)
{
  check_wait_object (lwaitobj);
  Buffer *bp = Buffer::coerce_to_buffer (lbuffer);
  bp->lwaitobj_list = xcons (lwaitobj, bp->lwaitobj_list);
  xwait_object_ref (lwaitobj)++;
  return Qt;
}

lisp
Fsi_remove_wait_object (lisp lwaitobj, lisp lbuffer)
{
  check_wait_object (lwaitobj);
  Buffer *bp = Buffer::coerce_to_buffer (lbuffer);
  if (lwaitobj == Qnil)
    bp->cleanup_waitobj_list ();
  else if (delq (lwaitobj, &bp->lwaitobj_list))
    decref_waitobj (lwaitobj);
  return Qt;
}

UINT wm_private_xyzzysrv;
static HANDLE hevent_listen;

void
init_listen_server ()
{
  hevent_listen = CreateEvent (0, 1, 0, 0);
  if (hevent_listen)
    SetProp (app.toplev, xyzzysrv_name, hevent_listen);
  wm_private_xyzzysrv = RegisterWindowMessage (xyzzysrv_name);
}

void
start_listen_server ()
{
  if (hevent_listen)
    SetEvent (hevent_listen);
}

void
end_listen_server ()
{
  if (hevent_listen)
    {
      SetEvent (hevent_listen);
      CloseHandle (hevent_listen);
      hevent_listen = 0;
      RemoveProp (app.toplev, xyzzysrv_name);
    }
}

lisp
Fstart_xyzzy_server ()
{
  if (!hevent_listen)
    init_listen_server ();
  start_listen_server ();
  return boole (hevent_listen);
}

lisp
Fstop_xyzzy_server ()
{
  end_listen_server ();
  return Qnil;
}

static int
eval_xyzzysrv_param (xyzzysrv_param *param)
{
  int r = 0;
  param->pid = 0;
  param->hevent = 0;
  param->hwnd = 0;
  if (!IsBadReadPtr (param, param->size))
    {
      r = -1;
      lisp stream = Qnil;
      protect_gc gcpro (stream);
      dynamic_bind dynb (Vsi_accept_kill_xyzzy, boole (param->kill_ok));
      try
        {
          save_cursor_depth cursor_depth;
          stream = Fmake_string_input_stream (make_string (param->data), 0, 0);
          lisp obj = Feval (Fread (stream, Qnil, Qnil, Qnil));
          if (wait_object_p (obj)
              && xwait_object_hevent (obj)
              && xwait_object_ref (obj))
            {
              param->pid = GetCurrentProcessId ();
              param->hevent = xwait_object_hevent (obj);
              param->hwnd = app.toplev;
            }
          r = 1;
        }
      catch (nonlocal_jump &)
        {
          print_condition (nonlocal_jump::data ());
        }
      if (stream != Qnil)
        {
          Fclose (stream, Qnil);
          refresh_screen (1);
        }
    }
  return r;
}

int
read_listen_server (WPARAM wparam, LPARAM lparam)
{
  HANDLE hproc = OpenProcess (PROCESS_DUP_HANDLE, 0, wparam);
  if (!hproc)
    return 0;

  HANDLE hmap;
  int r = DuplicateHandle (hproc, HANDLE (lparam), GetCurrentProcess (), &hmap,
                           0, 0, DUPLICATE_SAME_ACCESS);
  CloseHandle (hproc);
  if (!r)
    return 0;

  r = 0;
  void *v = MapViewOfFile (hmap, FILE_MAP_WRITE, 0, 0, 0);
  if (v && !IsBadReadPtr (v, sizeof (xyzzysrv_param)))
    {
      xyzzysrv_param *param = (xyzzysrv_param *)v;
      r = eval_xyzzysrv_param (param);
      UnmapViewOfFile (v);
    }
  CloseHandle (hmap);
  return r;
}
