# Emacs configurations

Cross platform emacs configuration.
Should work in Linux/Mac OS X/Windows, but some tools and packages may only be available in Linux/Mac.

## Docker

The`Dockerfile` can be used to create a working docker container based on Debian.
This container contains Emacs25, mu4e and git(for magit).

Example usage to start Emacs:
``` shell
docker build -t emacs-storvik .
docker run -it --rm emacs-storvik:latest
```

Note that `--rm` flag can be removed to avoid re-fetching all packages each time Emacs docker container is run.

## Windows Subsytem for Linux

One descent solution when running Emacs in Windows is using WSL.
To achieve this the following must be done:

Install a recent version of emacs:
``` shell
add-apt-repository ppa:ubuntu-elisp/ppa
apt-get update
apt-get install emacs25
```

Set the DISPLAY environmental variable:

``` shell
export DISPLAY=localhost:0.0      ## Bash
set -x DISPLAY localhost:0.0      ## Fish
```

Install MobaXTerm and run it.
Start Emacs and enjoy!
