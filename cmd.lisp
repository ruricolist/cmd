(defpackage #:cmd/cmd
  (:nicknames #:cmd)
  (:use #:cl #:alexandria #:serapeum #:cmd/hooks)
  (:import-from :uiop
                :process-info-input
                :process-info-error-output
                :process-info-output
                :process-info-pid
                :pathname-equal
                :getcwd
                :os-unix-p
                :native-namestring
                :native-namestring
                :os-windows-p :file-exists-p :getenv
                :pathname-directory-pathname
                :absolute-pathname-p
                :directory-pathname-p)
  (:import-from :trivia :match)
  (:import-from :shlex)
  (:import-from :uiop/launch-program
                :process-info)
  (:export
    :cmd :$cmd :cmd? :cmd! :cmd&
    :sh :$sh :sh? :sh! :sh&
    :pipeline                           ;Not yet implemented.
    :with-cmd-dir
    :*shell*
    :*visual-commands*
    :*command-wrappers*
    :*terminal*
    :vterm-terminal
    :cmdq))
(in-package :cmd)

;;; External executables, isolated for Guix compatibility.
(def +env+ "env")
(def +kill+ "kill")
(def +ps+ "ps")
(def +pwd+ "pwd")
(def +sh+ "/bin/sh")
(def +tr+ "tr")

(defvar *shell*
  (let ((shell (getenv "SHELL")))
    (if (emptyp shell)
        (if (os-unix-p)
            +sh+
            "cmd.exe")
        shell))
  "The shell to use for shell commands.

Defaults to $SHELL.")

(defun current-dir ()
  (let ((dpd *default-pathname-defaults*))
    (if (typep dpd 'absolute-directory-pathname)
        dpd
        (let ((dir (truename dpd)))
          (if (typep dir 'directory-pathname)
              dir
              (pathname-directory-pathname dir))))))

(deftype plist ()
  '(or null (cons symbol list)))

(defun can-use-env-c? ()
  (and (os-unix-p)
       (zerop
        (nth-value 2
          (uiop:run-program
           `(,+env+ "-C"
                    ,(native-namestring
                      (user-homedir-pathname))
                    "pwd")
           :ignore-error-status t
           :output nil
           :error-output nil)))))

(defparameter *can-use-env-c*
  (can-use-env-c?))

(defun update-can-use-env-c ()
  (setf *can-use-env-c* (can-use-env-c?)))

(uiop:register-image-restore-hook 'update-can-use-env-c)

(defparameter *keyword-abbrevs*
  ;; >| would be |>\|| in Lisp syntax, not worth it.
  ;; >? for Fish-style noclobber?
  '((:in :directory _)
    (:< :input _)
    ((:> :1>) :output _)
    ((:>> :1>>) :if-output-exists :append :output _)
    (:2> :error-output _)
    (:2>> :if-error-output-exists :append :error-output _)
    ((:&> :>&)
     :output _ :error-output _)
    ((:&>> :>>&)
     :if-error-output-exists :append
     :if-output-exists :append
     :error-output _
     :output _)))

(defun expand-keyword-abbrevs (args)
  (collecting
    (doplist (k v args)
      (if-let (match (assoc k *keyword-abbrevs*
                            :test #'member
                            :key #'ensure-list))
        (apply #'collect (substitute v '_ (rest match)))
        (collect k v)))))

(defun call/cmd-dir (fn dir)
  (let* ((*default-pathname-defaults* dir)
         ;; Resolve based on the usual rules.
         (*default-pathname-defaults* (current-dir)))
    (funcall fn)))

(defmacro with-cmd-dir (dir &body body)
  (with-thunk (body)
    `(call/cmd-dir ,body ,dir)))

(defvar *visual-commands* '()
  "List of commands that should be run in a `*terminal*' emulator.
Also see `*command-wrappers*'.")

(defvar *command-wrappers* '("sudo" "env")
  "Commands that fire up other commands.
This list is used by `visual-command-p' to check if the wrapped command is a
visual one.
See `*visual-commands*'.")

(defun visual-command-p (command)
  "Return true if the COMMAND list runs one of the programs in `*visual-commands*'.
`*command-wrappers*' are supported, i.e.

  env FOO=BAR sudo -i powertop

works."
  (labels ((basename (arg)
             (namestring (pathname-name arg)))
           (flag? (arg)
             (string^= "-" arg))
           (variable? (arg)
             (and (< 1 (length arg))
                  (string*= "=" (subseq arg 1))))
           (first-positional-argument (command)
             "Return the argument that's not a flag, not a variable setting and
not in `*command-wrappers*'."
             (when command
               (if (or (flag? (first command))
                       (variable? (first command))
                       (find (basename (first command))
                             *command-wrappers*
                             :test #'string=))
                   (first-positional-argument (rest command))
                   (first command)))))
    (and-let* ((cmd (first-positional-argument command)))
      (find (basename cmd)
            *visual-commands*
            :test #'string=))))

(defun vterm-terminal (cmd)
  "Run visual command CMD in Emacs' `vterm'."
  (list
   "emacsclient" "--eval"
   (let ((*print-case* :downcase))
     (write-to-string
      `(progn
         (vterm)
         (vterm-insert ,(string-join cmd " "))
         (vterm-send-return))))))

(defvar *terminal* (cond
                     ((resolve-executable "xterm")
                      '("xterm" "-e"))
                     ((resolve-executable "emacs")
                       #'vterm-terminal))
  "The terminal is either
- a list of arguments after which the visual command is appended,
- or a function of one argument, the list of commands, returning the new list of
commands.
See `*visual-commands*'.")

(defun maybe-visual-command (cmd)
  (if (visual-command-p cmd)
      (if (functionp *terminal*)
          (funcall *terminal* cmd)
          (append *terminal* cmd))
      cmd))

(defmacro define-cmd-variant (name sh-name lambda-list &body body)
  (let ((docstring (and (stringp (car body)) (pop body))))
    `(progn
       (defun ,name ,lambda-list
         ,@(unsplice docstring)
         ,@body)
       (define-compiler-macro ,name (cmd &rest args)
         `(locally (declare (notinline ,',name))
            (,',name ,@(simplify-cmd-args (cons cmd args)))))
       (defun ,sh-name (cmd &rest kwargs &key &allow-other-keys)
         ,(fmt "Like `~(~a~)' for a shell command.

Takes a single argument (along with keyword arguments for redirection)
and passes it to a shell.

The shell defaults to the value of `cmd:*shell*' (which in turn
defaults to the value of SHELL in the environment)."
               name)
         (apply #'as-shell #',name cmd kwargs)))))

(defun shell-arg ()
  ;; NB Even Powershell supports -c.
  (if (equal *shell* "cmd.exe") "/c" "-c"))

;; Inline so it propagates the ftype.
(defsubst as-shell (fn cmd &rest kwargs &key &allow-other-keys)
  (declare (function fn) (string cmd))
  (apply fn *shell* (shell-arg) (list cmd) kwargs))

(defclass cmd ()
  ((argv :initarg :argv :accessor cmd-argv)
   (kwargs :initarg :kwargs :accessor cmd-kwargs)))

(-> cmdq (&rest t) cmd)
(define-cmd-variant cmdq shq (cmd &rest args)
  (receive (argv kwargs) (parse-cmd-args (cons cmd args))
    (make 'cmd :argv argv :kwargs kwargs)))

(defun launch-cmd (cmd &rest overrides &key &allow-other-keys)
  (multiple-value-call #'cmd&
    (values-list (cmd-argv cmd))
    (values-list overrides)
    (values-list (cmd-kwargs cmd))))

(-> $cmd (&rest t) string)
(define-cmd-variant $cmd $sh (cmd &rest args)
  "Return the results of CMD as a string, stripping any trailing
newlines, like $(cmd) would in a shell.

By default stderr is discarded."
  (chomp
   (with-output-to-string (s)
     (multiple-value-call #'cmd
       :output s
       cmd (values-list args)
       :error-output nil))))

(-> cmd? (&rest t) (values boolean integer &optional))
(define-cmd-variant cmd? sh? (cmd &rest args)
  "Run a program, returning T if it passed, nil otherwise.
By default the output is discarded.

Returns the actual exit code as a second value."
  (let ((exit-code
          (multiple-value-call #'cmd
            :ignore-error-status t
            cmd (values-list args)
            :output nil
            :error-output nil)))
    (if (zerop exit-code)
        (values t 0)
        (values nil exit-code))))

(-> cmd! (&rest t) (values &optional))
(define-cmd-variant cmd! sh! (cmd &rest args)
  "Run CMD purely for its side effects, discarding all output and returning nothing."
  (apply #'cmd
         :output nil
         :error-output nil
         cmd args)
  (values))

(-> cmd (&rest t) (values integer &optional))
(define-cmd-variant cmd sh (cmd &rest args)
  "Run a program.

CMD should be a string naming a program. This command will be run with
its current directory set to the value of the current directory in a
thread-safe manner.

The current directory is based on `*default-pathname-defaults*', not on the OS-level working directory, as the OS-level directory is useless for multi-threaded programs.

A list of strings or pathnames is added to the list of arguments.

A string in ARGS is split into a list of tokens using shell-style
tokenization rules. (To protect a string with spaces, either add
quotation marks, or enclose it in a singleton list.)

A pathname in ARGS is translated to a native namestring and passed as
an argument to the command. The native namestring is not permitted to
start with a dash.

A property list is treated as a list of keyword arguments to
`uiop:run-program'. Certain keywords are treated as abbreviations:
e.g. `:>' is an abbreviation for `:output'. Abbreviations can be
compound: e.g. `:>>' affects both `:output' and `:if-exists'.

By default, standard output is sent to `*standard-output*', and error
output is sent to `*message-stream*'.

On Windows, the .exe suffix may be omitted from the name of the
executable."
  (multiple-value-bind (proc tokens args)
      (apply #'cmd& cmd args)
    (await proc
           :ignore-error-status (getf args :ignore-error-status)
           :tokens tokens)))

(-> cmd& (&rest t) (values process-info list list &optional))
(define-cmd-variant cmd& sh& (cmd &rest args)
  "Like `cmd', but run asynchronously and return a handle on the process (as from `launch-program')."
  (receive (tokens args) (parse-cmd-args (cons cmd args))
    (setf tokens (maybe-visual-command tokens))
    (setf tokens (cons (exe-string (car tokens)) (cdr tokens)))
    (setf args (expand-keyword-abbrevs args))
    (when *message-hook*
      (run-hook *message-hook*
                (fmt "$ ~{~a~^ ~}" (mapcar #'shlex:quote tokens))))
    (destructuring-bind (&key (output *standard-output*)
                           (error-output *error-output*)
                         &allow-other-keys)
        args
      (flet ((launch (args)
               (values
                (multiple-value-call #'launch-program-with-redirects
                  tokens
                  (values-list args)
                  :output output
                  :error-output error-output)
                tokens args)))
        (if-let (here-string (getf args :<<<))
          (with-input-from-string (in here-string)
            (launch
             ;; Insert the new redirection in the same place as the
             ;; old one to make sure keyword-override rules are
             ;; respected.
             (let* ((suffix (member :<<< args))
                    (prefix (ldiff args suffix)))
               (append prefix
                       (list :input in)
                       (cddr suffix)))))
          (launch args))))))

(defun simplify-cmd-args (args)
  (nlet rec ((args-in args)
             (args-out '()))
    (match args-in
      ((list)
       (reverse args-out))
      ((list (and _ (type keyword)))
       (error "Dangling keyword argument to cmd."))
      ((list* (and k (type keyword)) v rest)
       (rec rest
            (cons (if (constantp v)
                      `'(,k ,v)
                      `(list ,k ,v))
                  args-out)))
      ((list* (and s (type string)) xs)
       (rec xs
            (cons `(quote (,@(split-cmd s)))
                  args-out)))
      ((list* (and p (type pathname)) xs)
       (rec xs
            (cons (stringify-pathname p)
                  args-out)))
      ((list* x xs)
       (rec xs (cons x args-out))))))

(defun launch-program-with-redirects (argv &rest args)
  (destructuring-bind (&key input output error-output &allow-other-keys) args
    (let ((proc (multiple-value-call #'launch-program-in-dir*
                  argv
                  (if (typep input 'cmd)
                      (let ((subproc (launch-cmd input :output :stream)))
                        (values :input (process-info-output subproc)))
                      (values))
                  (if (typep output 'cmd)
                      (values :output :stream)
                      (values))
                  (if (typep error-output 'cmd)
                      (values :error-output :stream)
                      (values))
                  (values-list args))))
      (cond
        ((and (typep output 'cmd)
              (typep error-output 'cmd))
         (error "Not implemented yet"))
        ((typep output 'cmd)
         (launch-cmd output :input (process-info-output proc)))
        ((typep error-output 'cmd)
         (launch-cmd error-output :input (process-info-error-output proc)))
        (t proc)))))

(defun launch-program-in-dir* (tokens &rest args)
  "Run a program (with uiop:run-program) in the current base directory."
  (let ((dir (stringify-pathname
              (or (getf args :directory)
                  (current-dir)))))
    (apply #'launch-program-in-dir dir tokens
           (remove-from-plist args :directory))))

(defun launch-program-in-dir (dir tokens
                              &rest args
                              &key
                              &allow-other-keys)
  (let* ((cmd
           ;; NB The :directory argument to launch-program may end up
           ;; calling `chdir', which is unacceptable.
           (wrap-with-dir dir tokens))
         (proc
           (multiple-value-call #'uiop:launch-program cmd
             (values-list args))))
    (run-hook *proc-hook* proc)
    proc))

;;; From https://GitHub.com/GrammaTech/cl-utils/blob/master/shell.lisp
;;; (MIT license).
(defun kill-process (process &key urgent)
  "Terminate PROCESS and all its descendants.
On Unix, sends a TERM signal by default, or a KILL signal if URGENT."
  (if (and (os-unix-p)
           ;; ECL doesn't start a new process group for
           ;; launch-program, so this would kill the Lisp process.
           (not (eql :ecl (uiop:implementation-type))))
      ;; Kill the entire process group (process and its children).
      (uiop:run-program
       (fmt "~a -~d -$(~a -o pgid= ~d | ~a -d ' ')"
            +kill+
            (if urgent 9 15)
            +ps+
            (process-info-pid process)
            +tr+))
      ;; If non-unix, utilize the standard terminate process
      ;; which should be acceptable in most cases.
      (uiop:terminate-process process :urgent urgent)))

(defun await-all (procs &rest args)
  (do-each (proc (reshuffle procs))
    (apply #'await proc args)))

(defun await (proc &key ignore-error-status tokens)
  "Wait for PROC to finish."
  (nest
   (let ((out (process-info-output proc))
         (err (process-info-error-output proc))))
   (handler-bind ((serious-condition
                    ;; Flush output on error.
                    (lambda (e) (declare (ignore e))
                      (finish-output out)
                      (finish-output err)))))
   (let ((abnormal? t)))
   (unwind-protect
        (multiple-value-prog1
            (let ((status (uiop:wait-process proc)))
              (cond ((zerop status)
                     status)
                    (ignore-error-status
                     status)
                    (t
                     (cerror "IGNORE-ERROR-STATUS"
                             'uiop:subprocess-error
                             :command tokens
                             :code status
                             :process proc)
                     status)))
          (setf abnormal? nil))
     (when abnormal?
       (kill-process proc)))))

(defun parse-cmd-args (args)
  (nlet rec ((args args)
             (tokens '())
             (plist '()))
    (match args
      ((list)
       (values (nreverse tokens)
               (nreverse plist)))
      ((list* (and arg (type integer)) args)
       (rec (cons (princ-to-string arg)
                  args)
            tokens
            plist))
      ((list* (and arg (type string)) args)
       (rec args
            (nreconc (split-cmd arg) tokens)
            plist))
      ((list* (and arg (type pathname)) args)
       (rec args
            (cons (stringify-pathname arg) tokens)
            plist))
      ((list* (and arg (type plist)) args)
       (rec args
            tokens
            (revappend arg plist)))
      ((list* (and arg (type sequence)) args)
       (rec args
            (nreconc
             (collecting
               (do-each (token arg)
                 (collect (etypecase token
                            (string token)
                            (pathname (stringify-pathname token))))))
             tokens)
            plist))
      ((list (and _ (type keyword)))
       (error "Dangling keyword argument to cmd."))
      ((list* (and k (type keyword)) v args)
       (rec args
            tokens
            (nreconc (list k v) plist)))
      ((list* arg _)
       (error "Can't use ~a as a cmd argument." arg)))))

(defun wrap-with-dir (dir tokens)
  "Wrap TOKENS with the necessary code to run the process in DIR.

The OS-level current directory is per-process, not per thread. Using
`chdir' could lead to race conditions. Instead, we arrange for the new
process to change its own working directory."
  (when (pathname-equal dir (getcwd))
    (return-from wrap-with-dir tokens))
  (destructuring-bind (command . args) tokens
    (cond (*can-use-env-c*
           ;; When there is a recent version of GNU env installed, the
           ;; -C switch lets us do Bernstein chaining without spinning
           ;; up a shell.
           `(,+env+ "-C" ,dir ,command ,@args))
          ((not (os-windows-p))
           `(,+sh+
             "-c"
             ;; Use Bernstein chaining; change to the directory in $1,
             ;; shift, and exec the rest of the argument array.
             "set -e; CDPATH='' cd -P \"$1\"; shift; exec \"$@\""
             ;; Terminate processing of shell options; everything
             ;; after this is passed through.
             "--"
             ,dir
             ,command
             ,@args))
          ;; This looks weird, but it actually works, because the
          ;; Windows API to start a process is called with a
          ;; string rather than an array. We could just as well
          ;; pass a string, but then we would have to do our own
          ;; escaping.
          (t
           `("cmd"
             "/c"
             ;; Note that /d is required for cd to work across drives.
             "cd" "/d" ,dir
             ;; Ampersand is the command separator.
             "&" ,command ,@args)))))

(defun stringify-pathname (arg)
  (unless (pathnamep arg)
    (return-from stringify-pathname arg))
  (lret ((string
          (let ((string (native-namestring arg)))
            (if (and (os-windows-p)
                     (featurep :ccl)
                     (position #\/ string))
                ;; Work around a CCL bug; issue #103 on GitHub.
                (substitute #\\ #\/ string)
                string))))
    (when (string^= "-" string)
      ;; Should we ignore the unsafe file names if `--' or
      ;; `---' is already present in the list of tokens?
      (cerror "Allow the unsafe file name"
              "File name ~a begins with a dash"
              string))))

(defun exe-string (p)
  (stringify-pathname (exe p)))

(defun split-cmd (cmd)
  ;; NB UIOP expects simple-strings for arguments.
  (mapcar (op (coerce _ '(simple-array character (*))))
          (shlex:split cmd :whitespace-split nil
                           :punctuation-chars t)))
