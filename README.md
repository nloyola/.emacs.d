# Emacs configuration

The file `init.el` sets up initial GUI settings, bootstraps
**use-package**, and parses `config.org`, which is where the most of the
configuration is done.

The file `config.org` is parsed and each emacs-lisp block is evaluated
individually. If a block generates an error, Emacs does not halt,
instead it continues and accumulates the errors. If any errors were
encountered, they will be reported in the `*init errors*` buffer. This
idea comes from the Wasamasa's
[Emacs Ninja blog](http://emacsninja.com/posts/failing-gracefully.html),
but I changed the parsing to use the
[emacs-pl](https://github.com/jwiegley/emacs-pl) parsing library, and
it also ignores lisp blocks tagged with `:tangle no`.

*Since `emacs-pl` is not available in MELPA or ELPA it has to be
downloaded directly from GitHub.*
