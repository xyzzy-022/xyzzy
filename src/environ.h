#ifndef _environ_h_
# define _environ_h_

class environ
{
public:
  static int save_window_size;
  static int save_window_position;
  static int restore_window_size;
  static int restore_window_position;
  static int load_geometry (int, POINT *, SIZE *);
  static void save_geometry ();
};

class Registry
{
protected:
  static const char base[];
  HKEY hkey;
  Registry ();
  ~Registry ();
public:
  static const char Settings[];
  int fail () const;
};

inline
Registry::Registry ()
     : hkey (0)
{
}

inline
Registry::~Registry ()
{
  if (hkey)
    RegCloseKey (hkey);
}

inline int
Registry::fail () const
{
  return !hkey;
}

class ReadRegistry: public Registry
{
protected:
  void open_local (const char *);
public:
  int get (const char *, void *, DWORD, DWORD) const;
  int get (const char *, int *) const;
  int get (const char *, long *) const;
  int get (const char *, char *, int) const;
  int get (const char *, void *, int) const;
  int query (const char *, DWORD *) const;
  ReadRegistry (const char *);
  ReadRegistry (HKEY, const char *);
};

inline
ReadRegistry::ReadRegistry (const char *subkey)
{
  open_local (subkey);
}

inline int
ReadRegistry::get (const char *key, int *x) const
{
  return get (key, x, sizeof *x, REG_DWORD) == sizeof *x;
}

inline int
ReadRegistry::get (const char *key, long *x) const
{
  return get (key, x, sizeof *x, REG_DWORD) == sizeof *x;
}

inline int
ReadRegistry::get (const char *key, char *buf, int size) const
{
  return get (key, buf, size, REG_SZ);
}

inline int
ReadRegistry::get (const char *key, void *buf, int size) const
{
  return get (key, buf, size, REG_BINARY);
}

class WriteRegistry: public Registry
{
public:
  int set (const char *, DWORD, const void *, int) const;
  int set (const char *, long) const;
  int set (const char *, const char *) const;
  int set (const char *, const char *, int) const;
  int set (const char *, const void *, int) const;
  int remove (const char *) const;
  WriteRegistry (const char *);
};

inline int
WriteRegistry::set (const char *key, long val) const
{
  return set (key, REG_DWORD, &val, sizeof val);
}

inline int
WriteRegistry::set (const char *key, const char *val) const
{
  return set (key, REG_SZ, val, strlen (val) + 1);
}

inline int
WriteRegistry::set (const char *key, const char *val, int size) const
{
  return set (key, REG_SZ, val, size);
}

inline int
WriteRegistry::set (const char *key, const void *val, int size) const
{
  return set (key, REG_BINARY, val, size);
}

class EnumRegistry: public ReadRegistry
{
public:
  EnumRegistry (const char *subkey) : ReadRegistry (subkey) {}
  EnumRegistry (HKEY h, const char *subkey) : ReadRegistry (h, subkey) {}
  operator HKEY () const {return hkey;}
};

struct decoded_time
{
  int year;
  int mon;
  int day;
  int hour;
  int min;
  int sec;
  int dow;
  int timezone;
  int daylight;
};

void decode_universal_time (lisp, decoded_time *);
lisp decoded_time_to_universal_time (int, int, int, int, int, int, int);
lisp file_time_to_universal_time (const FILETIME &);

#endif
