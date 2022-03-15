# {#ref-qs} Installation and Setup

This quickstart guide outlines the step-by-step instructions for getting started with programming in Reach.
You can install on the following operating systems:

* [Windows](##ref-qs-win)
* [Linux](##ref-qs-linux)
* [MacOS](##ref-qs-mac)

# {#ref-qs-win} Windows

Reach requires WSL 2 and [Docker Desktop](https://www.docker.com/get-started) to be installed.

## {#ref-qs-win-prereqs} Prerequisites

* Windows 10 installed with version 2004 or higher,

  or

* Windows 11

## {#ref-qs-win-install} Installation

Click the Windows icon, type `CMD`, and then click `Run as Administrator`.
There are a number of commands that need to be run to get Windows ready for Reach.

To install WSL on Windows, run:

``` cmd
$ wsl --install
```

:::note
Some Windows 10 users need to run `dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart` to install WSL.
:::

To then install Ubuntu, run:

``` cmd
$ wsl --install -d ubuntu
```

In the Command Prompt window, run the following command to set the WSL version to 2:

``` cmd
$ wsl --set-version ubuntu 2
``` 

Next, download and install [Docker Desktop](https://www.docker.com/products/docker-desktop).

:::note
Make sure to follow Docker's instructions for installation and configuration.
Those instructions are subject to change and are outside of our control.
:::

Click the `Settings` (gear) icon along the top of the Docker app.

![Settings icon in Docker`](/quickstart/settings-icon.png)

Verify that `WSL 2.0` is checked, as well as `Use Docker Compose V2`.

![WSL 2 settings in Docker`](/quickstart/wsl-2.png)
![Docker Compose in Docker`](/quickstart/docker-compose.png)

Click `Resources` in the left-hand menu, and make sure that that `Enable integration with additional distros` is checked, and that `Ubuntu` is selected.

![WSL integration in Docker`](/quickstart/wsl-integrate.png)

Click the Ubuntu icon in the Windows Start-up menu to open the Ubuntu terminal.
You will need to provide a `username` and `password` for Ubuntu.

In the terminal, run the following to install `make`:

```cmd
$ sudo apt install make
```

Next, run the following to allow `apt` to use repositories containing https:

```cmd
$ sudo apt install ca-certificates curl gnupg lsb-release
```

Create and navigate to the reach directory with the following command:

``` cmd
$ mkdir -p ~/reach && cd ~/reach
```

Download Reach with the following command:

``` cmd
$ curl https://docs.reach.sh/reach -o reach ; chmod +x reach
```

Reach is successfully downloaded if the following command returns a version number:

```
$ ./reach version
```

You are now ready to start programming in Reach.
Check out our [Rock Paper Scissors!](##tut) tutorial to get started.

# {#ref-qs-linux} Linux

Reach requires [make](https://en.wikipedia.org/wiki/Make_(software)), [Docker Engine](https://docs.docker.com/get-docker/), and [Docker Compose](https://docs.docker.com/compose/install/).

## {#ref-qs-linux-prereqs} Prerequisites

* A compatible version of Linux.

  We suggest using the most recent version of Ubuntu and all instructions are written for that system.
  Check the [Docker Engine](https://docs.docker.com/engine/install/) page for supported distros. 

## {#ref-qs-linux-install} Installation

Follow the [Docker Engine](https://docs.docker.com/engine/install/) instructions for installing on your version of Linux.
The following instructions assume that Ubuntu is the installed distro.
Check the commands required for completing the tasks below for your distro.

In the terminal, run the following to install `make`:

```cmd
$ sudo apt install make
```

Next, run the following to allow `apt` to use repositories containing https:

```cmd
$ sudo apt install ca-certificates curl gnupg lsb-release
```

After that, run:

```cmd
$ sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
```

And then, run:

```cmd
$ sudo echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \ $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
```

Create and navigate to the reach directory with the following command:

``` cmd
$ mkdir -p ~/reach && cd ~/reach
```

Download Reach with the following command:

``` cmd
$ curl https://docs.reach.sh/reach -o reach ; chmod +x reach
```

Reach is successfully downloaded if the following command returns a version number:

```
$ ./reach version
```

You are now ready to start programming in Reach.
Check out our [Rock Paper Scissors!](##tut) tutorial to get started.

# {#ref-qs-mac} MacOS

Reach is compatible with current M1 chips and previous Intel chips running macOS 10.15 or newer. 
Installation instructions should not differ regardless of the MacOS architecture. 
Reach requires installing [Docker](https://www.docker.com/get-started).

## {#ref-qs-mac-install} Installation

`make` should be preinstalled.
Test this by opening `terminal` and running the following command:

``` cmd
$ make --version
```

Download [Docker Desktop](https://www.docker.com/get-started) and follow the prompts to complete application setup.

According to the [Docker Docs](https://docs.docker.com/compose/install/), "Docker Desktop for Mac includes Compose along with other Docker apps, so Mac users do not need to install Compose separately."

To verify that Docker Compose is operating on your Mac, execute the command:

``` cmd
$ docker-compose --version
```

When it returns a version number, Reach is ready to be installed. 
We recommend creating a parent directory named `reach` that will contain all of your Reach projects as subdirectories. i.e.

```
reach
|   reach
|
└───tut
|   |   index.rsh
|   |   index.mjs
|   
└───project2
|   |   index.rsh
|   |   index.mjs
│
└───project3
```

Create and navigate to the `reach` directory with the following command:

``` cmd
$ mkdir -p ~/reach && cd ~/reach
```

Download Reach with the following command:

``` cmd
$ curl https://docs.reach.sh/reach -o reach ; chmod +x reach
```

Reach is successfully downloaded if the following command returns a version number:

```
$ ./reach version
```

You are now ready to start programming in Reach.
Check out our [Rock Paper Scissors!](##tut) tutorial to get started.

If you have any issues running `reach`, please check the [Troubleshooting](##ref-ts) page for solutions.