#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "guide-windows"]{Using Reach on Windows}

Reach assumes the presence of a POSIX-compliant shell, as well as @link["https://www.docker.com/get-started"]{Docker} and @link["https://docs.docker.com/compose/install/"]{Docker Compose}.

The best way to get this set up on Windows is to

@itemlist[
#:style 'ordered

@item{Install Windows 10, version 2004 or higher.}

@item{Enable the @link["https://docs.microsoft.com/en-us/windows/wsl/install-win10"]{Windows Subsystem for Linux} feature, specifically the WSL 2 option.}

@item{Download and install @link["https://www.docker.com/products/docker-desktop"]{Docker Desktop}, and enable the @link["https://docs.docker.com/docker-for-windows/wsl/"]{Docker Desktop WSL 2 backend}.}

@item{Follow the instructions from Docker on @link["https://docs.docker.com/docker-for-windows/wsl/#develop-with-docker-and-wsl-2"]{developing with Docker and WSL 2}.}

]

At this point, you'll have your @link["https://code.visualstudio.com/download"]{VSCode} IDE set up with a terminal in your Linux distribution.

You can now follow the instructions in the @seclink["ref-install"]{reference manual on installation} or the @seclink["tut-1"]{first part of the tutorial} to download, install, and execute all Reach commands.

@margin-note{You can @link["https://www.youtube.com/watch?v=wczwWvBdMTE"]{watch a five minute video} going through these steps on YouTube.}
