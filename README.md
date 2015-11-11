# Emacs configuration

The file `init.el` sets up initial GUI settings, bootstraps
use-package, and parses `config.org`, which is where the most of the
configuration is done.

The file `config.org` is parsed and each emacs-lisp block is evaluated
individually. If a block generates an error, Emacs does not halt,
instead it continues and accumulates the errors. If any errors were
encountered, they will be reported in the `*init errors*` buffer.
