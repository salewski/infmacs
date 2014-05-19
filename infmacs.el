;; infmacs.el --- inferior/remote Emacs interaction -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

;; Emacs as a SERVER:

(cl-defstruct (infmacs-server (:constructor infmacs-server--create))
  "An instance of an Infmacs server currently handling requests."
  proc clients)

(defun infmacs-open (port)
  "Start up a new server on PORT, returning a server object."
  (let* ((server nil) ; to be closed over
         (proc
          (make-network-process
           :name "infmacs-server"
           :service port
           :server t
           :host "localhost"
           :coding 'raw-text
           :filter-multibyte nil
           :sentinel (lambda (proc status)
                       (infmacs-server-sentinel server proc status))
           :filter (lambda (proc content)
                     (infmacs-filter proc content #'infmacs-respond)))))
    (prog1 (setf server (infmacs-server--create :proc proc))
      (process-put proc :struct server))))

(defun infmacs-close (server)
  "Close an Infmacs SERVER along with all clients."
  (mapc #'delete-process (infmacs-server-clients server))
  (delete-process (infmacs-server-proc server)))

(defun infmacs-server-sentinel (server proc status)
  "Runs every time a client connects or changes state."
  (if (string-match-p "^open from" status)
      (infmacs-register-client server proc)
    (infmacs-unregister-client server proc)))

(defun infmacs-register-client (server proc)
  "Register PROC with INFMACS server."
  (push proc (infmacs-server-clients server))
  (setf (process-get proc :server) server
        (process-get proc :fill-buffer)
        (generate-new-buffer " *infmacs-filler*")))

(defun infmacs-unregister-client (server proc)
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

(defun infmacs-respond (proc request)
  "Respond to PROC for REQUEST."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (prin1 (infmacs-handle request) (current-buffer))
    (process-send-region proc (point-min) (point-max))))

(defun infmacs-handle (request)
  "Process request and return the response value."
  (let ((expr (plist-get request :expr)))
    (condition-case e
        `(:value ,(eval expr t))
      (error `(:error ,e)))))

(cl-defun infmacs-batch-start (&optional (port (+ 1024 (mod (random) 64511))))
  (if (null noninteractive)
      (error "Only call `infmacs-start' in batch mode!")
    (let ((infmacs (infmacs-open port)))
      (princ (format "Server opened on port %d\n" port))
      (while t
        (sleep-for 60)))))

;; Emacs as a CLIENT:

(cl-defstruct (infmacs-connection (:constructor infmacs-connection--create))
  "A connection to another Emacs process running an Infmacs server."
  proc buffer)

(defun infmacs-connect (host port)
  "Connect to Infmacs server at HOST and PORT, returning a connection object."
  (let* ((proc (make-network-process
               :name "infmacs"
               :service port
               :host host
               :family 'ipv4
               :filter (lambda (proc content)
                         (infmacs-filter proc content #'infmacs-result))))
        (buffer (generate-new-buffer " *infmacs-filler*"))
        (client (infmacs-connection--create :proc proc :buffer buffer)))
    (prog1 client
      (process-put proc :struct client)
      (process-put proc :fill-buffer buffer))))

(defun infmacs-eval (infmacs expr)
  "Evaluate EXPR in INFMACS server."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (prin1 `(:expr ,expr) (current-buffer))
    (process-send-region (infmacs-connection-proc infmacs)
                         (point-min) (point-max))))

(defun infmacs-result (proc response)
  "Handle REPONSE from the server connected to PROC."
  (setf foo response)
  (let ((value (plist-get response :value))
        (error (plist-get response :error)))
    (if error
        (funcall #'signal (car error) (cdr error))
      (message "%s" value))))

(provide 'infmacs)

;;; infmacs.el ends here
