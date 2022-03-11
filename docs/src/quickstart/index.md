# (#ref-qs) Installation and Setup

This quickstart guide outlines the step-by-step instructions for getting started programming in Reach for Windows, Linux, and Mac.

# (#ref-qs-win) Windows

Reach requires [make](https://en.wikipedia.org/wiki/Make_(software)), [Docker](https://www.docker.com/get-started), and [Docker Compose](https://docs.docker.com/compose/install/).

## (#ref-qs-win-prereqs) Prerequisites

+Windows 10 installed with version 2004 or higher

## (#ref-qs-win-install) Installation

Click the Windows icon, type `CMD`, and then click `Run as Administrator`.
There are a number of commands that need to be run to get Windows ready for Reach.

In the Command Prompt window, run:

``` cmd
$ wsl --install -d ubuntu
```

This installs Ubuntu WSL, which is used via command line later.

In the Command Prompt window, run:

``` cmd
$ wsl --set-version ubuntu 2
``` 

This sets the WSL to version 2, which is required by Docker and Reach.

Next, download and install [Docker Desktop](https://www.docker.com/products/docker-desktop).

:::note
Make sure to follow Docker's instructions for installation and configuration.
Those instructions are subject to change and are outside of our control.
:::

Click the `Settings`(gear) icon along the top of the Docker app.

![Settings icon in Docker`](/qs/settings-icon.png)

Verify that `WSL 2.0` is checked, as well as `Use Docker Compose V2`.

![WSL 2 settings in Docker`](/qs/wsl-2.png)
![Docker Compose in Docker`](/qs/docker-compose.png)

Click `Resources` in the left-hand menu, and check that `Enable integration with additional distros` is checked, and that `Ubuntu` is selected.

![WSL integration in Docker`](/qs/wsl-integrate.png)

Double-click the Ubuntu icon in the Windows Start-up menu to open the Ubuntu terminal.
You will need to provide a `username` and `password` for Ubuntu.

In the terminal, run the following to install `make`:

```cmd
$ sudo apt install make
```

Next, run the following to allow `apt` to use repositories containing https:

```cmd
$ sudo apt-get install ca-certificates curl gnupg lsb-release
```

Create and navigate to the reach directory with the following command:

``` cmd
$ mkdir -p ~/reach && cd ~/reach
```

Download Reach with the following command:

``` cmd
$ curl https://docs.reach.sh/reach -o reach ; chmod +x reach
```

Reach is successfully downloaded with the following command returns a version number:

```
$ ./reach version
```

You are now ready to start programming in Reach.
Check out our [Rock Paper Scissors!](##tut) tutorial to get started.

# (#ref-qs-linux) Linux

Reach requires [make](https://en.wikipedia.org/wiki/Make_(software)), [Docker Engine](https://docs.docker.com/get-docker/), and [Docker Compose](https://docs.docker.com/compose/install/).

## (#ref-qs-linux-prereqs) Prerequisites

+A compatible version of Linux.
Check the [Docker Engine](https://docs.docker.com/engine/install/) page for supported distros. 

## (#ref-qs-linux-install) Installation

Follow the [Docker Engine](https://docs.docker.com/engine/install/) instructions for installing on your version of Linux.

In the terminal, run the following to install `make`:

```cmd
$ sudo apt install make
```

Next, run the following to allow `apt` to use repositories containing https:

```cmd
$ sudo apt-get install ca-certificates curl gnupg lsb-release
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

Reach is successfully downloaded with the following command returns a version number:

```
$ ./reach version
```

You are now ready to start programming in Reach.
Check out our [Rock Paper Scissors!](##tut) tutorial to get started.

# (ref-qs-mac) MacOS

Reach is compatible with legacy Mac architecture and the new M1 architecture. 
Installation instructions should not differ regardless of the MacOS architecture. 
Reach requires [make](https://en.wikipedia.org/wiki/Make_(software)), [Docker](https://www.docker.com/get-started), and [Docker Compose](https://docs.docker.com/compose/install/).

## (#ref-qs-mac-install) Installation

`make` should be preinstalled.
Test this by opening `terminal` and running the following command:

``` cmd
$ make --version
```

Check if Docker is installed with the following command:

``` cmd
$ docker --version
```

If it's not installed, download [Docker Desktop](https://www.docker.com/get-started) and follow the prompts to complete application setup.

According to the [Docker Docs](https://docs.docker.com/compose/install/), "Docker Desktop for Mac includes Compose along with other Docker apps, so Mac users do not need to install Compose separately."

To verify that Docker Compose is operating on your mac, execute the command:

``` cmd
$ docker-compose --version
```

When all three commands return version numbers, Reach is ready to be installed. 
We recommend creating a parent directory `reach` that will contain the `reach` programming language and all of your Reach projects as subdirectories. i.e.

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

Create and navigate to the reach directory with the following command:

``` cmd
$ mkdir -p ~/reach && cd ~/reach
```

Download Reach with the following command:

``` cmd
$ curl https://docs.reach.sh/reach -o reach ; chmod +x reach
```

Reach is successfully downloaded with the following command returns a version number:

```
$ ./reach version
```

## (ref-qs-mac-troubleshooting) MacOS Troubleshooting

Receiving the following error on **M1 Macs**:

``` cmd
The requested image's platform (linux/amd64) does not match the detected host platform (linux/arm64/v8) and no specific platform was requested`
```

* Ensure Docker and Docker-compose are installed.
* Check which mode the terminal is in by executing `$ arch` in the terminal.
On M1 machines, the output should be `arm64`.
* Re-download the reach script in the desired directory:
`$ curl https://docs.reach.sh/reach -o reach ; chmod +x reach`
* Make sure the latest `reach-cli` image is installed: `$ docker pull reachsh/reach-cli:latest`
* Use `reach-cli` to update the other Reach docker images by running `$ ./reach update` in the terminal.

## (ref-qs-troubleshooting) Reach installation and configuration troubleshooting

If you receive one of the following errors: 

```cmd
reach: /app/tmp/out.sh: openFile: permission denied (Permission denied)
```

OR

```
chmod: cannot access '/tmp/reach.../out.sh': No such file or directory
./reach: 1: /tmp/reach.../out.sh: not found
```

* This generally means that Reach was previously run or installed with `sudo` and directories were created that only `root` can access. 

* If this is the case, delete the local copy of the Reach program and reinstall per the instructions.

* If use of `sudo` during installation is not the cause then try [updating Docker](https://techdirectarchive.com/2021/10/17/how-to-manually-update-docker-desktop/) and Docker-compose.

"I'm running Windows 8 and cannot install Docker."

Unfortunately, Docker Desktop does not support Windows 8.
Also, Reach is not able to run on Windows 8. 

Possible solutions are:
* Upgrade to Windows 10 or Windows 11.
* Partition the drive (or use a second drive) and install of a copy of a supported Linux distro.
* Run a VM with a supported OS.
It is likely that this option has the worst experience due to the splitting of system resources.

Executing `./reach version` on Linux outputs `./reach: Is a directory`

* This error usually occurs when `reach` is being called from a directory where it is not installed.
Compare where Reach is installed with the current working directory. 
* Check the current working directory with the terminal command `pwd`.

### Reach configuration and REACH_CONNECTOR_MODE

`Missing REACH_CONNECTOR_MODE`

* Reach expects to be told to connect to a specific consensus network.
Running ONE of the following commands should fix this error:

`export REACH_CONNECTOR_MODE=ALGO`

`export REACH_CONNECTOR_MODE=ETH`

`export REACH_CONNECTOR_MODE=CFX`

When executing `./reach config`, the compiler prints the following message:

```
Reach detected an existing configuration file at...

You appear to be using the `bash` shell, with environment configuration stored in /home/USER/.profile.
Run the following command to activate your new configuration:

$ . /home/USER/.profile
```

* The compiler is asking you to execute the following command in your terminal.
Note that `USER` designates your terminal's user name.

`. /home/USER/.profile`

### Troubleshooting Reach VSCode Extension

"I don't see syntax highlighting in VSCode despite having the Reach extension installed and activated."

* Usually this is because VSCode needs to be restarted after installing the extension. 

* The extension is actively being updated.
Sometimes after an update VSCode will need to be restarted in order for syntax highlighting to begin working again. 

* Please report any errors you encounter in the [Discord #help](https://discord.com/channels/628402598663290882/749639931399241792) channel.