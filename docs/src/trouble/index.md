# {#ref-ts} Troubleshooting

If you receive one of the following errors: 

```cmd
reach: /app/tmp/out.sh: openFile: permission denied (Permission denied)
```

or

```
chmod: cannot access '/tmp/reach.../out.sh': No such file or directory
./reach: 1: /tmp/reach.../out.sh: not found
```

* This generally means that Reach was previously run or installed with `sudo` and directories were created that only `root` can access. 

* If this is the case, delete the local copy of the Reach program and reinstall per the instructions.

* If use of `sudo` during installation is not the cause then try [updating Docker](https://techdirectarchive.com/2021/10/17/how-to-manually-update-docker-desktop/) and Docker-compose.

"I'm running Windows 8 and cannot install Docker."

Unfortunately, Docker Desktop does not support Windows 8.
Thus, Reach is not able to run on Windows 8. 

Possible solutions are:
* Upgrade to Windows 10 or Windows 11.
* Partition the drive (or use a second drive) and install a copy of a supported Linux distro.
* Run a VM with a supported OS.

  It is likely that running a VM has the worst experience due to the splitting of system resources.

Executing `./reach version` on Linux outputs `./reach: Is a directory`

* This error usually occurs when `reach` is being called from a directory where it is not installed.

  Compare where Reach is installed with the current working directory. 

* Check the current working directory with the terminal command `pwd`.

If running `./reach run` seems to require `sudo` it's possible one or more of the following is at issue:

* A directory used by `reach` was created with `sudo` or by another account.

  Make sure that the current user has read/write permissions in the directory. 

* Docker might have been installed using Snap.

  Snap installation in Ubuntu creates permissions issues.
  Uninstall Docker and then re-install using the `apt` command in the terminal.

## {#ref-ts-vscode} Troubleshooting Reach VSCode Extension

"I don't see syntax highlighting in VSCode despite having the Reach extension installed and activated."

* Usually this is because VSCode needs to be restarted after installing the extension. 

* The extension is actively being updated.

  Sometimes after an update, VSCode needs to be restarted in order for syntax highlighting to begin working again. 

## {#ref-qs-mac-troubleshooting} MacOS troubleshooting

Receiving the following error on **M1 Macs**:

``` cmd
The requested image's platform (linux/amd64) does not match the detected host platform (linux/arm64/v8) and no specific platform was requested`
```

* Check which mode the terminal is in by executing `$ arch` in the terminal.

  On M1 machines, the output should be `arm64`.

* Re-download the `reach` script in the desired directory:

```cmd
$ curl https://docs.reach.sh/reach -o reach ; chmod +x reach
```

* Make sure the latest `reach-cli` image is installed: `$ docker pull reachsh/reach-cli:latest`
* Use `reach-cli` to update the other Reach docker images by running `$ ./reach update` in the terminal.

If the information in this troubleshooting guide does not fix your issue, please report any errors you encounter in the [Discord #help](https://discord.com/channels/628402598663290882/749639931399241792) channel.
This page will be updated based on issues and solutions discovered in that channel.