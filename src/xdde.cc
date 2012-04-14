#include "stdafx.h"
#ifdef __XYZZY__
# include "ed.h"
#else /* not __XYZZY__ */
# include <windows.h>
# include <malloc.h>
# ifndef alloca
#  define alloca _alloca
# endif
#endif /* not __XYZZY__ */
#include "xdde.h"
#ifndef DDE_CLIENT_ONLY
# ifdef __XYZZY__
#  include "sysdep.h"
#  define PLATFORM_WINDOWS_NT (sysdep.WinNTp ())
# endif /* __XYZZY__ */
#endif /* not DDE_CLIENT_ONLY */

DWORD Dde::dde_inst;
#ifndef DDE_CLIENT_ONLY
HSZ Dde::hsz_server;
#endif

static HDDEDATA CALLBACK
dde_callback (UINT type, UINT fmt, HCONV hconv, HSZ sz1, HSZ sz2,
              HDDEDATA dde_data, DWORD data1, DWORD data2)
{
  switch (type)
    {
    case XTYP_ADVDATA:
      return DDE_FNOTPROCESSED;

    default:
      return 0;

    case XTYP_XACT_COMPLETE:
      return 0;

#ifndef DDE_CLIENT_ONLY
    case XTYP_ADVSTART:
      return 0;

    case XTYP_ADVSTOP:
      return 0;

    case XTYP_ADVREQ:
      return 0;

    case XTYP_CONNECT:
      return HDDEDATA ((!fmt || fmt == CF_TEXT)
                       && Dde::verify_context ((CONVCONTEXT *)data1)
                       && Dde::find_topic (sz2, sz1, 0));

    case XTYP_WILDCONNECT:
      if (fmt && fmt != CF_TEXT)
        return 0;
      return Dde::wild_connect (sz1, sz2, (CONVCONTEXT *)data1);

    case XTYP_REQUEST:
    case XTYP_EXECUTE:
    case XTYP_POKE:
      {
        DdeCallbackInfo dci;
        dci.type = type;
        dci.fmt = fmt;
        dci.hconv = hconv;
        dci.topic = sz1;
        dci.item = sz2;
        dci.hdata = dde_data;
        dci.data1 = data1;
        dci.data2 = data2;
        return Dde::doprocess (dci);
      }

    case XTYP_CONNECT_CONFIRM:
      return 0;

    case XTYP_DISCONNECT:
      return 0;
#endif /* not DDE_CLIENT_ONLY */
    }
}

void
Dde::initialize ()
{
  if (dde_inst)
    return;
#ifdef DDE_CLIENT_ONLY
  int e = DdeInitialize (&dde_inst, dde_callback, APPCMD_CLIENTONLY, 0);
#else
  int e = DdeInitialize (&dde_inst, dde_callback, APPCMD_FILTERINITS, 0);
#endif
  if (e != DMLERR_NO_ERROR)
    throw Exception (e);
#ifndef DDE_CLIENT_ONLY
  register_server ();
#endif
}

void
Dde::cleanup ()
{
  if (dde_inst)
    {
#ifndef DDE_CLIENT_ONLY
      DdeNameService (dde_inst, 0, 0, DNS_UNREGISTER);
      delete_strings ();
#endif
      DdeUninitialize (dde_inst);
      dde_inst = 0;
    }
}

Dde::Dde ()
{
}

Dde::~Dde ()
{
  cleanup ();
}

static Dde dde_instance__;

#ifndef DDE_CLIENT_ONLY

# ifndef __XYZZY__

class osversion
{
public:
  osversion ();
  int winnt;
};

osversion::osversion ()
{
  OSVERSIONINFO o;
  o.dwOSVersionInfoSize = sizeof o;
  GetVersionEx (&o);
  winnt = o.dwPlatformId == VER_PLATFORM_WIN32_NT;
}

static osversion osver;

#  define PLATFORM_WINDOWS_NT (osver.winnt)

# endif /* not __XYZZY__ */

int
Dde::verify_context (CONVCONTEXT *cc)
{
  return (!cc || (cc->iCodePage == CP_WINANSI
                  && cc->qos.ImpersonationLevel >= SecurityImpersonation));
}

HDDEDATA
Dde::doprocess (DdeCallbackInfo &dci)
{
  HDDEDATA result = HDDEDATA (dci.type == XTYP_REQUEST ? 0 : DDE_FNOTPROCESSED);
  if (dci.fmt && dci.fmt != CF_TEXT)
    return result;

  DdeItemList *il = find_topic (hsz_server, dci.topic,
                                dci.type == XTYP_EXECUTE ? 0 : dci.item);
  if (!il)
    return result;
  if (dci.type == XTYP_EXECUTE)
    for (; il->item != DDE_EXECUTE_ITEM; il++)
      if (!il->item)
        return result;

  if (PLATFORM_WINDOWS_NT && !DdeImpersonateClient (dci.hconv))
    return result;

  try
    {
      result = (*il->callback)(&dci);
    }
  catch (Dde::Exception &)
    {
    }
# ifdef __XYZZY__
  catch (nonlocal_jump &)
    {
    }
# endif

  if (PLATFORM_WINDOWS_NT)
    RevertToSelf();

  return result;
}

DdeItemList *
Dde::find_topic (HSZ service, HSZ topic, HSZ item)
{
  if (service != hsz_server)
    return 0;
  for (DdeTopicList *t = DdeServerTopicList; t->topic; t++)
    if (!DdeCmpStringHandles (topic, t->hsz_topic))
      {
        if (!item)
          return t->items;
        for (DdeItemList *i = t->items; i->item; i++)
          if (i->item != DDE_EXECUTE_ITEM
              && (!DdeCmpStringHandles (item, i->hsz_item)
                  || (i->matcher && (*i->matcher)(i, item))))
            return i;
      }
  return 0;
}

HDDEDATA
Dde::wild_connect (HSZ topic, HSZ service, CONVCONTEXT *cc)
{
  if (!verify_context (cc))
    return 0;

  if (service && service != hsz_server)
    return 0;

  int ntopics;
  for (ntopics = 0; DdeServerTopicList[ntopics].topic; ntopics++)
    ;

  HSZ *hsz = (HSZ *)alloca ((ntopics + 1) * sizeof *hsz * 2);

  int i, j;
  for (i = 0, j = 0; i < ntopics; i++)
    if (!topic || !DdeCmpStringHandles (topic, DdeServerTopicList[ntopics].hsz_topic))
      {
        hsz[j++] = hsz_server;
        hsz[j++] = DdeServerTopicList[ntopics].hsz_topic;
      }
  hsz[j++] = 0;
  hsz[j++] = 0;

  return DdeCreateDataHandle (dde_inst, (BYTE *)hsz, sizeof *hsz * j,
                              0, 0, CF_TEXT, 0);
}

void
Dde::create_strings ()
{
  hsz_server = DdeCreateStringHandle (dde_inst, DdeServerName, CP_WINANSI);
  for (DdeTopicList *t = DdeServerTopicList; t->topic; t++)
    {
      t->hsz_topic = DdeCreateStringHandle (dde_inst, t->topic, CP_WINANSI);
      for (DdeItemList *i = t->items; i->item; i++)
        if (i->item != DDE_EXECUTE_ITEM)
          i->hsz_item = DdeCreateStringHandle (dde_inst, i->item, CP_WINANSI);
    }
}

void
Dde::delete_strings ()
{
  DdeFreeStringHandle (dde_inst, hsz_server);
  for (DdeTopicList *t = DdeServerTopicList; t->topic; t++)
    {
      DdeFreeStringHandle (dde_inst, t->hsz_topic);
      for (DdeItemList *i = t->items; i->item; i++)
        if (i->item != DDE_EXECUTE_ITEM)
          DdeFreeStringHandle (dde_inst, i->hsz_item);
    }
}

void
Dde::register_server ()
{
  create_strings ();
  DdeNameService (dde_inst, hsz_server, 0, DNS_REGISTER);
}

#endif /* not DDE_CLIENT_ONLY */
