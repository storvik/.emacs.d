# Emacs configurations

Cross platform emacs configuration.
Should work in Linux/Mac OS X/Windows, but some tools and packages may only be available in Linux/Mac.

## Files

- `init.el` is the starting point when opening Emacs
- `emacs_init.org` contains all common configs which works on all platforms
- `emacs_<platform>.org` contains platform specific configs
- `emacs_unix.org` contains configs which works in Linux and Mac OS X only

## Docker

The`Dockerfile` can be used to create a working docker container based on Debian.
This container contains Emacs25, mu4e and git(for magit).

Example usage to start Emacs:
``` shell
docker build -t emacs-storvik .
docker run -it --rm emacs-storvik:latest
```

Note that `--rm` flag can be removed to avoid re-fetching all packages each time Emacs docker container is run.
