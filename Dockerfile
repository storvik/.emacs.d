FROM debian:latest
LABEL maintainer="Petter S. Storvik"

RUN apt-get update && apt-get install -y emacs25 maildir-utils mu4e git

RUN mkdir /root/.emacs.d
COPY init /root/.emacs.d/init
COPY elisp /root/.emacs.d/elisp
COPY snippets /root/.emacs.d/snippets
COPY init.el /root/.emacs.d/

CMD "/usr/bin/emacs"
