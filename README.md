# Infmacs, Inferior Emacs interaction environment

Connect to a remote Emacs instance as you would a remote Common Lisp
instance running Swank with SLIME. Expressions can be evaluated in the
"inferior" Emacs as if it was local.

This is a work in progress.

## Quickstart

1. Start an Infmacs server in batch mode.

        emacs -Q -batch -l infmacs.el --eval '(infmacs-batch-start 3333)'

2. Connect to it from your normal Emacs with `M-x infmacs`.
   Connections are *only* accepted from localhost.

3. Activate `infmacs-minor-mode` in an `emacs-lisp-mode` buffer. All
   evaluations are redirected to the remote instance for evaluation.
