#ifndef _pathname_h_
# define _pathname_h_

# define WPATH_MAX (PATH_MAX / 2)

typedef Char pathbuf_t[WPATH_MAX * 2 + 1];

struct pathname
{
  const Char *dev;
  const Char *deve;
  const Char *trail;
  const Char *traile;
};

# define SEPCHAR '/'

#endif
