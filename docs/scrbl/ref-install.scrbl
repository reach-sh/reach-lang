#lang scribble/manual
@(require "lib.rkt"
          scribble/bnf)

@title[#:version reach-vers #:tag "ref-install"]{Installation}

Reach is designed to work on POSIX systems with @link["https://en.wikipedia.org/wiki/Make_(software)"]{make}, @link["https://www.docker.com/get-started"]{Docker}, and @link["https://docs.docker.com/compose/install/"]{Docker Compose} installed.
The best way to install Docker on Mac and Windows is with @link["https://www.docker.com/products/docker-desktop"]{Docker Desktop}.

@margin-note{You probably already have @exec{make} installed.
For example, OS X and many other POSIX systems come with @exec{make}, but some versions of Linux do not include it by default and will require you to install it.
If you're on Ubuntu, you can run @exec{sudo apt install make} to get it.}

You can install Reach by running:

@cmd{curl https://docs.reach.sh/reach -o reach ; chmod +x reach}

in your project repository.
You can copy this file to other repositories or move it to a directory in your @envref{PATH}, like @exec{~/bin}.
(@defenv{PATH} is a UNIX environment variable listing each of the directories that contain programs you can run in a shell session.)

@margin-note{If you're using Windows, consult @seclink["guide-windows"]{the guide to using Reach on Windows}.}

