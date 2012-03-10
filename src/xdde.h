#ifndef _XDDE_H_
# define _XDDE_H_

# include <ddeml.h>

#ifndef DDE_CLIENT_ONLY

struct DdeCallbackInfo
{
  UINT type;
  UINT fmt;
  HCONV hconv;
  HSZ topic;
  HSZ item;
  HDDEDATA hdata;
  DWORD data1;
  DWORD data2;
};

# define DDE_EXECUTE_ITEM ((char *)1)

struct DdeItemList
{
  HSZ hsz_item;
  HDDEDATA (__stdcall *callback)(DdeCallbackInfo *);
  const char *item;
  int (__stdcall *matcher)(const DdeItemList *, HSZ);
};

struct DdeTopicList
{
  HSZ hsz_topic;
  DdeItemList *items;
  const char *topic;
};

extern const char DdeServerName[];
extern DdeTopicList DdeServerTopicList[];

#endif /* not DDE_CLIENT_ONLY */

class Dde
{
  static DWORD dde_inst;

  static void cleanup ();

public:
  class Exception
    {
    public:
      int e;
      Exception ();
      Exception (int);
    };

protected:
  class DdeString
    {
      HSZ hsz;
      DdeString (const DdeString &);
      DdeString &operator = (const DdeString &);
    public:
      DdeString (const char *string);
      ~DdeString ();
      operator HSZ () const;
    };

#ifndef DDE_CLIENT_ONLY
  static HSZ hsz_server;

  static void register_server ();
  static void create_strings ();
  static void delete_strings ();
public:
  static int verify_context (CONVCONTEXT *);
  static HDDEDATA wild_connect (HSZ, HSZ, CONVCONTEXT *);
  static DdeItemList *find_topic (HSZ, HSZ, HSZ);
  static HDDEDATA doprocess (DdeCallbackInfo &);
#endif

public:
  Dde ();
  ~Dde ();
  static void initialize ();
  static DWORD instance ();
  static HCONV initiate (const char *, const char *);
  static void terminate (HCONV);
  static void execute (HCONV, long, const char *, int);
  static void execute (HCONV, long, const char *);
  static void poke (HCONV, long, const char *, const char *, int);
  static void poke (HCONV, long, const char *, const char *);
  static HDDEDATA request (HCONV, long, const char *);
};

class DdeDataP
{
protected:
  HDDEDATA hdata;
  DWORD len;
  BYTE *d;

  DdeDataP (const DdeDataP &);
  DdeDataP &operator = (const DdeDataP &);
public:
  DdeDataP (HDDEDATA);
  int length () const;
  const char *data () const;
};

class DdeData: public DdeDataP
{
public:
  DdeData (HDDEDATA);
  ~DdeData ();
};

class DdeDataAccess: public DdeDataP
{
public:
  DdeDataAccess (HDDEDATA);
  ~DdeDataAccess ();
};

inline DWORD
Dde::instance ()
{
  if (!dde_inst)
    initialize ();
  return dde_inst;
}

inline
Dde::Exception::Exception (int e_)
     : e (e_)
{
}

inline
Dde::Exception::Exception ()
     : e (DdeGetLastError (instance ()))
{
}

inline
Dde::DdeString::DdeString (const char *string)
{
  hsz = DdeCreateStringHandle (instance (), *string ? string : " ", CP_WINANSI);
  if (!hsz)
    throw Exception ();
}

inline
Dde::DdeString::~DdeString ()
{
  if (hsz)
    DdeFreeStringHandle (instance (), hsz);
}

inline
Dde::DdeString::operator HSZ () const
{
  return hsz;
}

inline HCONV
Dde::initiate (const char *serv, const char *topic)
{
  DdeString xserv (serv), xtopic (topic);
  HCONV h = DdeConnect (instance (), xserv, xtopic, 0);
  if (!h)
    throw Exception ();
  return h;
}

inline void
Dde::terminate (HCONV h)
{
  if (!DdeDisconnect (h))
    throw Exception ();
}

inline void
Dde::execute (HCONV hconv, long timeout, const char *data, int l)
{
  if (!DdeClientTransaction ((BYTE *)data, l, hconv, 0, 0,
                             XTYP_EXECUTE, timeout, 0))
    throw Exception ();
}

inline void
Dde::execute (HCONV hconv, long timeout, const char *data)
{
  execute (hconv, timeout, data, strlen (data) + 1);
}

inline void
Dde::poke (HCONV hconv, long timeout, const char *item, const char *data, int l)
{
  DdeString xitem (item);
  if (!DdeClientTransaction ((BYTE *)data, l, hconv, xitem,
                             CF_TEXT, XTYP_POKE, timeout, 0))
    throw Exception ();
}

inline void
Dde::poke (HCONV hconv, long timeout, const char *item, const char *data)
{
  poke (hconv, timeout, item, data, strlen (data) + 1);
}

inline HDDEDATA
Dde::request (HCONV hconv, long timeout, const char *item)
{
  DdeString xitem (item);
  HDDEDATA data = DdeClientTransaction (0, 0, hconv, xitem,
                                        CF_TEXT, XTYP_REQUEST,
                                        timeout, 0);
  if (!data)
    throw Exception ();
  return data;
}

inline
DdeDataP::DdeDataP (HDDEDATA h_)
     : hdata (h_)
{
  d = DdeAccessData (hdata, &len);
}

inline int
DdeDataP::length () const
{
  return len;
}

inline const char *
DdeDataP::data () const
{
  return (const char *)d;
}

inline
DdeData::DdeData (HDDEDATA h_)
     : DdeDataP (h_)
{
  if (!d)
    {
      int e = DdeGetLastError (Dde::instance ());
      DdeFreeDataHandle (hdata);
      throw Dde::Exception (e);
    }
}

inline
DdeData::~DdeData ()
{
  if (d)
    DdeUnaccessData (hdata);
  DdeFreeDataHandle (hdata);
}

inline
DdeDataAccess::DdeDataAccess (HDDEDATA h_)
     : DdeDataP (h_)
{
  if (!d)
    throw Dde::Exception ();
}

inline
DdeDataAccess::~DdeDataAccess ()
{
  if (d)
    DdeUnaccessData (hdata);
}

#endif
