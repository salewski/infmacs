;; infmacs.el --- inferior/remote Emacs interaction -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Infmacs provides remote interaction with an Emacs instance from an
;; Emacs, much like SLIME+Swank. Communication is performed over a TCP
;; socket and the "inferior" Emacs would generally be run in batch
;; mode (`infmacs-batch-start').

;; TODO:
;; * REPL

;;; Code:

(require 'cl-lib)
(require 'eieio)

;; Emacs as a SERVER:

(defclass infmacs-server ()
  ((proc :initarg :proc
         :reader infmacs-server-proc)
   (clients :initform ()
            :accessor infmacs-server-clients))
  (:documentation "An instance of an Infmacs server."))

(defun infmacs-open (port)
  "Start up a new server on PORT, returning a server object."
  (let* ((server nil) ; to be closed over
         (proc
          (make-network-process
           :name "infmacs-server"
           :service port
           :server t
           :host "localhost"
           :family 'ipv4
           :coding 'raw-text
           :filter-multibyte nil
           :sentinel (lambda (proc status)
                       (infmacs--server-sentinel server proc status))
           :filter (lambda (proc content)
                     (infmacs-filter proc content #'infmacs--respond)))))
    (prog1 (setf server (make-instance 'infmacs-server :proc proc))
      (process-put proc :server server))))

(defmethod infmacs-close ((server infmacs-server))
  "Close an Infmacs SERVER along with all clients."
  (mapc #'delete-process (infmacs-server-clients server))
  (delete-process (infmacs-server-proc server)))

(defmethod infmacs-live-p ((server infmacs-server))
  "Return non-nil if SERVER is still alive."
  (process-live-p (infmacs-server-proc server)))

(defun infmacs--server-sentinel (server proc status)
  "Runs every time a client connects or changes state."
  (if (string-match-p "^open from" status)
      (infmacs-register-client server proc)
    (infmacs-unregister-client server proc)))

(defmethod infmacs-register-client ((server infmacs-server) proc)
  "Register PROC with INFMACS server."
  (push proc (infmacs-server-clients server))
  (setf (process-get proc :server) server
        (process-get proc :fill-buffer)
        (generate-new-buffer " *infmacs-filler*")))

(defmethod infmacs-unregister-client ((server infmacs-server) proc)
  "Clean up after PROC and remove from SERVER's client list."
  (let ((buffer (process-get proc :fill-buffer)))
    (when buffer (kill-buffer buffer))
    (setf (infmacs-server-clients server)
          (cl-delete proc (infmacs-server-clients server)))))

(defun infmacs-filter (proc content handler)
  "Try to read a request from the client and give it to HANDLER.
The handler is called with two arguments, PROC and the request object."
  (let ((buffer (process-get proc :fill-buffer)))
    (with-current-buffer buffer
      (set-buffer-multibyte nil)
      (setf (point) (point-max))
      (insert content)
      (set-buffer-multibyte t)
      (setf (point) (point-min))
      ;; Attempt to parse the buffer.
      (let ((request (condition-case nil
                         (prog1 (read (current-buffer))
                           (delete-region (point-min) (point)))
                       (end-of-file nil)
                       (invalid-read-syntax nil))))
        (when request
          (funcall handler proc request))))))

(defun infmacs--respond (proc request)
  "Respond to PROC for REQUEST."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (prin1 (infmacs--handle request) (current-buffer))
    (process-send-region proc (point-min) (point-max))))

(defun infmacs--handle (request)
  "Process request and return the response value."
  (with-temp-buffer
    (let ((standard-output (current-buffer))
          (expr (plist-get request :expr)))
      (condition-case e
          (let ((output (list :value (prin1-to-string (eval expr t)))))
            (when (> (buffer-size) 0)
              (setf output (nconc output (list :stdout (buffer-string)))))
            output)
        (error `(:error ,e))))))

(cl-defun infmacs-batch-start (&optional (port (+ 1024 (mod (random) 64511))))
  "For running an Infmacs server in batch mode, never returning.
Invoking like so will start the server on a random port:
    emacs -Q -batch -l infmacs.el -f infmacs-batch-start"
  (if (null noninteractive)
      (error "Only call `infmacs-start' in batch mode!")
    (let ((_server (infmacs-open port)))
      (princ (format "Server opened on port %d\n" port))
      (while t
        (sleep-for 60)))))

;; Emacs as a CLIENT:

(defvar infmacs-default-connection nil
  "A single global connection for redirecting evaluation requests..")

(defclass infmacs-connection ()
  ((proc :initarg :proc
         :reader infmacs-connection-proc)
   (buffer :initform (generate-new-buffer " *infmacs-filler*")
           :reader infmacs-connection-buffer))
  (:documentation "A connection to another Emacs process running an Infmacs."))

(defun infmacs-connect (host port)
  "Connect to Infmacs server at HOST and PORT, returning a connection object."
  (let* ((proc (make-network-process
               :name "infmacs"
               :service port
               :host host
               :family 'ipv4
               :filter (lambda (proc content)
                         (infmacs-filter proc content #'infmacs-result))))
        (client (make-instance 'infmacs-connection :proc proc)))
    (prog1 client
      (process-put proc :client client)
      (process-put proc :fill-buffer (infmacs-connection-buffer client)))))

(defmethod infmacs-close ((infmacs infmacs-connection))
  "Close the connection to INFMACS."
  (delete-process (infmacs-connection-proc infmacs)))

(defmethod infmacs-live-p ((infmacs infmacs-connection))
  "Return non-nil if INFMACS. is still alive."
  (process-live-p (infmacs-connection-proc infmacs)))

(defun infmacs-eval (infmacs expr)
  "Evaluate EXPR in INFMACS server."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (prin1 `(:expr ,expr) (current-buffer))
    (setf (point) (point-min))
    (unless (ignore-errors (read (current-buffer)))
      (error "Cannot evaluate unreadable value."))
    (process-send-region (infmacs-connection-proc infmacs)
                         (point-min) (point-max))))

(defun infmacs-result (_proc response)
  "Handle REPONSE from the server connected to PROC."
  (let ((value (plist-get response :value))
        (output (plist-get response :stdout))
        (error (plist-get response :error)))
    (cond (error (funcall #'signal (car error) (cdr error)))
          (output (message "%s%s" output value))
          ((message "%s" value)))))

;; Misc

(defun infmacs--try-int (string)
  "Return the integer expressed in STRING if it looks like an integer."
  (when (string-match-p "^ *[0-9]+ *$" string)
    (read string)))

;; As a MINOR MODE:

(defvar infmacs-default-host "localhost"
  "Default host when prompting the user.")

(defvar infmacs-default-port nil
  "Default port when prompting the user for input.")

(defun infmacs-read-host-port ()
  "Ask the user for a host and port."
  (let* ((host (read-string (format "Host (%s): " infmacs-default-host)
                            nil nil infmacs-default-host))
         (clipboard (x-get-selection))
         (port (read-number "Port: " (or infmacs-default-port
                                         (infmacs--try-int clipboard)))))
    (list host port)))

(defun infmacs (host port)
  "Connect to a running Infmacs server."
  (interactive (infmacs-read-host-port))
  (when infmacs-default-connection (infmacs-close infmacs-default-connection))
  (setf infmacs-default-connection (infmacs-connect host port))
  (message "Connected to %s:%d." host port))

(defvar infmacs-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-x C-e") #'infmacs-eval-last-sexp)
      (define-key map (kbd "C-M-x") #'infmacs-eval-defun)
      (define-key map (kbd "C-c C-k") #'infmacs-eval-buffer)))
  "Keymap for `infmacs-minor-mode'.")

(define-minor-mode infmacs-minor-mode
  "Mode that redirects in-buffer evaluation to an \"inferior\" Emacs."
  :lighter " Infmacs"
  :keymap infmacs-minor-mode-map)

(defun infmacs-eval-last-sexp (&optional prefix)
  "Like `eval-last-sexp' but do so in the \"inferior\" Emacs."
  (interactive "P")
  (infmacs-eval infmacs-default-connection (preceding-sexp)))

(defun infmacs-eval-defun ()
  "Like `eval-defun' but do so in the \"inferior\" Emacs."
  (interactive)
  (let ((expr (save-excursion
                (beginning-of-defun)
                (setq form (read (current-buffer))))))
    (infmacs-eval infmacs-default-connection form)))

(provide 'infmacs)

;;; infmacs.el ends here
