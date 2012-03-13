#include "ed.h"
#include "appid.h"

lisp
Fsi_app_user_model_id ()
{
  return xsymbol_value (Qapp_user_model_id);
}

void
appid::set ()
{
  HMODULE shell32 = GetModuleHandle ("shell32");
  if (!shell32)
    shell32 = LoadLibrary ("shell32");
  if (!shell32)
    return;

  SETAPPID SetAppId =
    (SETAPPID)GetProcAddress (shell32, "SetCurrentProcessExplicitAppUserModelID");
  if (!SetAppId)
    return;

  SetAppId (L"xyzzy");
}
