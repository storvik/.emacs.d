# Stormacs - Storviks Emacs config

This is my opinionated Emacs config.

## Featuring

- [Elpaca package manager](https://github.com/progfolio/elpaca)
- [Vertico completion](https://github.com/minad/vertico)
- [Consult completing-read](https://github.com/minad/consult)
- [Orderless](https://github.com/oantolin/orderless)
- [Meow modal editing](https://github.com/meow-edit/meow)
- [Denote note taking](https://github.com/protesilaos/denote)

## Config structure

``` text
│
├── early-init.el                       - early init file called before init.el
│
├── init.el                             - init.el, main init file
│
├── hosts/                              - host configs
│
├── site-lisp/                          - third party lisp code
│
├── stormacs-lisp/                      - general lisp code
│
└── stormacs-modules/                   - modules, consisting of packages and settings
```
