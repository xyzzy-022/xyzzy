#include "gen-stdafx.h"

typedef struct
{
  const char *command;
  void *action;
} gensrc_action;

typedef int (__cdecl *GENACTION) (int argc, char **argv);

#if defined(GEN_SRC1)
#include "gen-action1.h"
#elif defined(GEN_SRC2)
#include "gen-action2.h"
#endif

int
main (int argc, char **argv)
{
  if (argc < 2)
    {
      fprintf (stderr, "too few argments\n");
      exit (2);
    }

  const char *cmd = argv[1];
  for (int i = 0; i < numberof (actions); i++)
    {
      if (!strcmp (cmd, actions[i].command))
        return ((GENACTION)(actions[i].action))(argc - 1, &argv[1]);
    }

  fprintf (stderr, "unknown command: %s\n", cmd);
  exit (2);
}
