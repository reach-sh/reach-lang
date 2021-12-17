


# {#ref-install} Installation

Reach is designed to work on POSIX systems with [make](https://en.wikipedia.org/wiki/Make_(software)), [Docker](https://www.docker.com/get-started), and [Docker Compose](https://docs.docker.com/compose/install/) installed.
The best way to install Docker on Mac and Windows is with [Docker Desktop](https://www.docker.com/products/docker-desktop).

:::note
You probably already have `make` installed.
For example, OS X and many other POSIX systems come with `make`, but some versions of Linux do not include it by default and will require you to install it.
If you're on Ubuntu, you can run `sudo apt install make` to get it.
:::


You can install Reach by running:


```
$ curl https://docs.reach.sh/reach -o reach ; chmod +x reach
```


in your project repository.
You can copy this file to other repositories or move it to a directory in your `PATH`, like `~/bin`.
(`PATH` is a UNIX environment variable listing each of the directories that contain programs you can run in a shell session.)

:::note
If you're using Windows, consult [the guide to using Reach on Windows](##guide-windows).
:::


