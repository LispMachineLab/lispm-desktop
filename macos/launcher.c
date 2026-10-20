/* launcher.c --- LispM.app's executable

   Copyright (C) 2026  Ryan.K
   SPDX-License-Identifier: GPL-3.0-or-later

   LispM.app is GNU Emacs under LispM's name and icon.  Its executable is
   this program, which runs the Emacs beside it, in Contents/MacOS, with a
   checkout of LispM Desktop as its init directory, and hands on the
   arguments it was given.  It replaces itself with Emacs, keeping its
   process, so macOS goes on seeing LispM.

   install.sh compiles it with LISPM_INIT_DIRECTORY set to the checkout; a
   leading "~/" is the home directory of whoever opens the application.  */

#include <limits.h>
#include <mach-o/dyld.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifndef LISPM_INIT_DIRECTORY
#define LISPM_INIT_DIRECTORY "~/.lispm/lispm-desktop"
#endif

/* Write into BUFFER, of SIZE bytes, the directory DIRECTORY names, "~/"
   at its start standing for HOME.  Return 0, or -1 when it does not fit
   or HOME is unknown.  */
static int
expand_directory (char *buffer, size_t size, const char *directory, const char *home)
{
  int written;

  if (strncmp (directory, "~/", 2) == 0)
    {
      if (home == NULL || home[0] == '\0')
        return -1;
      written = snprintf (buffer, size, "%s/%s", home, directory + 2);
    }
  else
    written = snprintf (buffer, size, "%s", directory);
  return (written < 0 || (size_t) written >= size) ? -1 : 0;
}

int
main (int argc, char *argv[])
{
  char self[PATH_MAX];
  char resolved[PATH_MAX];
  char emacs[PATH_MAX];
  char directory[PATH_MAX];
  char option[PATH_MAX + sizeof "--init-directory="];
  uint32_t size = sizeof self;
  char *slash;
  char **arguments;
  int i;

  if (_NSGetExecutablePath (self, &size) != 0 || realpath (self, resolved) == NULL)
    {
      fputs ("LispM: cannot find its own executable\n", stderr);
      return 1;
    }
  slash = strrchr (resolved, '/');
  if (slash == NULL)
    return 1;
  *slash = '\0';
  if (snprintf (emacs, sizeof emacs, "%s/Emacs", resolved) >= (int) sizeof emacs
      || expand_directory (directory, sizeof directory, LISPM_INIT_DIRECTORY, getenv ("HOME")) != 0)
    {
      fputs ("LispM: a path is too long, or HOME is not set\n", stderr);
      return 1;
    }
  snprintf (option, sizeof option, "--init-directory=%s", directory);

  arguments = calloc ((size_t) argc + 2, sizeof *arguments);
  if (arguments == NULL)
    return 1;
  arguments[0] = emacs;
  arguments[1] = option;
  for (i = 1; i < argc; i++)
    arguments[i + 1] = argv[i];
  arguments[argc + 1] = NULL;

  execv (emacs, arguments);
  perror ("LispM: cannot run Emacs");
  return 1;
}
