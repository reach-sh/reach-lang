


# {#guide-windows} Using Reach on Windows

Reach assumes the presence of a POSIX-compliant shell, as well as [Docker](https://www.docker.com/get-started) and [Docker Compose](https://docs.docker.com/compose/install/).

The best way to get this set up on Windows is to

1. Install Windows 10, version 2004 or higher.
2. Enable the [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10) feature, specifically the WSL 2 option.
3. Download and install [Docker Desktop](https://www.docker.com/products/docker-desktop), and enable the [Docker Desktop WSL 2 backend](https://docs.docker.com/docker-for-windows/wsl/).
4. Follow the instructions from Docker on [developing with Docker and WSL 2](https://docs.docker.com/docker-for-windows/wsl/#develop-with-docker-and-wsl-2).


At this point, you'll have your [VSCode](https://code.visualstudio.com/download) IDE set up with a terminal in your Linux distribution.

You can now follow the instructions in the [reference manual on installation](##install) or the [first part of the tutorial](##tut-1) to download, install, and execute all Reach commands.

::: note
You can [watch a five minute video](https://www.youtube.com/watch?v=wczwWvBdMTE) going through these steps on YouTube.
:::
