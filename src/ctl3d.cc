#include "stdafx.h"
#include "sysdep.h"
#include "ctl3d.h"
#include "vfs.h"

static Ctl3d ctl3d;

Ctl3dState Ctl3d::state;

Ctl3d::~Ctl3d ()
{
  if (state.registered && state.Unregister)
    (*state.Unregister)(state.hinst);
  if (state.hlib)
    FreeLibrary (state.hlib);
}

void
Ctl3d::enable (HINSTANCE h)
{
  if (sysdep.Win4p () || state.registered)
    return;
  if (!state.hlib)
    {
      state.hlib = WINFS::LoadLibrary ("ctl3d32.dll");
      if (!state.hlib)
        return;
      state.Register = (BOOL (WINAPI *)(HINSTANCE))
        GetProcAddress (state.hlib, "Ctl3dRegister");
      state.Unregister = (BOOL (WINAPI *)(HINSTANCE))
        GetProcAddress (state.hlib, "Ctl3dUnregister");
      state.AutoSubclass = (BOOL (WINAPI *)(HINSTANCE))
        GetProcAddress (state.hlib, "Ctl3dAutoSubclass");
      state.UnAutoSubclass = (BOOL (WINAPI *)())
        GetProcAddress (state.hlib, "Ctl3dUnAutoSubclass");
      state.ColorChange = (BOOL (WINAPI *)())
        GetProcAddress (state.hlib, "Ctl3dColorChange");
      state.WinIniChange = (void (WINAPI *)())
        GetProcAddress (state.hlib, "Ctl3dWinIniChange");
      if (!state.Register || !state.Unregister
          || !state.AutoSubclass || !state.UnAutoSubclass
          || !state.ColorChange || !state.WinIniChange)
        {
          FreeLibrary (state.hlib);
          state.hlib = 0;
          return;
        }
    }
  if (state.Register && state.AutoSubclass)
    {
      state.hinst = h;
      (*state.Register)(state.hinst);
      (*state.AutoSubclass)(state.hinst);
      state.registered = 1;
    }
}

void
Ctl3d::color_change ()
{
  if (state.registered && state.ColorChange)
    (*state.ColorChange)();
}

void
Ctl3d::ini_change ()
{
  if (state.registered && state.WinIniChange)
    (*state.WinIniChange)();
}

Ctl3dThread::Ctl3dThread ()
{
  if (Ctl3d::state.registered && Ctl3d::state.AutoSubclass)
    (*Ctl3d::state.AutoSubclass)(Ctl3d::state.hinst);
}

Ctl3dThread::~Ctl3dThread ()
{
  if (Ctl3d::state.registered && Ctl3d::state.UnAutoSubclass)
    (*Ctl3d::state.UnAutoSubclass)();
}

