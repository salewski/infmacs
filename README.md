# Infmacs
## Inferior Emacs interaction environment

Connect to a remote Emacs instance as you would a remote Common Lisp
instance running Swank with SLIME. Expressions can be evaluated in the
"inferior" Emacs as if it was local.

The purpose is to make Emacs Lisp an even better general programming
language. A batch-mode Emacs instance can be made interactive and
programmed much like a Common Lisp image with Swank.

## Quickstart

1. Start an Infmacs server in batch mode.

        emacs -Q -batch -l infmacs.el --eval '(infmacs-batch-start 3333)'

2. Connect to it from your normal Emacs with `M-x infmacs`, providing
   a host and port (3333 in this case). For security reasons,
   connections are *only* accepted from localhost.

3. Activate `infmacs-minor-mode` in an `emacs-lisp-mode` buffer. All
   evaluations will be redirected to the remote instance for
   evaluation.

## Features

Full Emacs' Unicode support is maintained across the connection.

~~~el
(infmacs-eval-synchronously t '(concat "N" "aïve"))
;; => "Naïve"
~~~

In addition to the interactive interface (the minor mode and REPL),
there is a programmatic interface to the inferior Emacs.

~~~el
(defun start-remote-httpd-server (host)
  (let ((connection (infmacs-connection host 3333)))
    (unwind-protect
        (infmacs-eval connection '(httpd-start))
      (infmacs-close connection))))
~~~
