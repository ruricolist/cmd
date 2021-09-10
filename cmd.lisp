(uiop:define-package #:cmd/cmd
  (:nicknames #:cmd)
  (:use #:cl #:alexandria #:serapeum #:cmd/hooks)
  (:import-from
   :uiop
   :delete-file-if-exists
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
  (:import-from :trivia :match :ematch)
  (:import-from :shlex)
  (:import-from :uiop/launch-program
   :process-info)
  (:import-from #:trivial-garbage
                #:make-weak-hash-table)
  (:export
   :cmd :$cmd :cmd? :cmd! :cmd&
   :sh :$sh :sh? :sh! :sh&
   :with-cmd-dir
   :psub
   :*shell*
   :*visual-commands*
   :*command-wrappers*
   :*terminal*
   :vterm-terminal))
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

(-> current-dir () (values absolute-directory-pathname &optional))
(defun current-dir ()
  "Get the current directory based on `*default-pathname-defaults*'."
  (let ((dpd *default-pathname-defaults*))
    (if (typep dpd 'absolute-directory-pathname)
        dpd
        (let ((dir (truename dpd)))
          (if (typep dir 'directory-pathname)
              dir
              (pathname-directory-pathname dir))))))

(defun can-use-env-c? ()
  "Return T if we can use env -C to launch a program in the current
directory, instead of using a shell."
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
  (can-use-env-c?)
  "Save whether we can use env -C.")

(defun update-can-use-env-c ()
  (setf *can-use-env-c* (can-use-env-c?)))

(uiop:register-image-restore-hook 'update-can-use-env-c)

(defconst +redirection-operators+
  '(:< :> :1> :>> :1>> :|>\|| :2> :2>> :|2>\|| :&> :>& :&>> :>>& :<<< :>? :2>?)
  "All redirection operators that can be parsed in tokenized strings.")

(defconst +subcommand-dividers+
  ;; TODO &&, ||, etc.
  '(:|\||)
  "All supported subcommand dividers (e.g. pipelines).")

(deftype redirection-operator ()
  '#.(cons 'member +redirection-operators+))

(deftype subcommand-divider ()
  '#.(cons 'member +subcommand-dividers+))

(defconstructor string-token
  (string (simple-array character (*))))

(defun make-string-token (string)
  (string-token (coerce string '(simple-array character (*)))))

(defun flatten-string-tokens (list)
  (mapcar (lambda (item)
            (if (typep item 'string-token)
                (string-token-string item)
                item))
          list))

(deftype token ()
  '(or subcommand-divider keyword string-token))

(defun expand-redirection-abbrev (keyword)
  (assure list
    (case-of (or (eql :in) redirection-operator) keyword
      (:in '(:directory _))
      (:< '(:input _))
      ((:> :1>) '(:output _))
      ((:>> :1>>) '(:if-output-exists :append :output _))
      (:|>\|| '(:if-output-exists :supersede :output _))
      (:2> '(:error-output _))
      (:2>> '(:if-error-output-exists :append :error-output _))
      (:|2>\|| '(:if-error-output-exists :supersede :error-output _))
      ((:&> :>&) '(:output _ :error-output _))
      ((:&>> :>>&) '(:if-error-output-exists :append
                     :if-output-exists :append
                     :error-output _
                     :output _))
      (:>? '(:if-output-exists :error :output _))
      (:2>? '(:if-error-output-exists :error :error-output _))
      (:<<< '(:<<< _))
      (otherwise nil))))

(def +dividers+ '(:|\||))

(defun expand-keyword-abbrevs (args)
  (collecting
    (doplist (k v args)
      (if-let (exp (expand-redirection-abbrev k))
        (apply #'collect (substitute v '_ exp))
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
  (setf command (flatten-string-tokens command))
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

(defvar *subprocs*
  (make-weak-hash-table :weakness :key)
  "A table from process to subprocesses.")

(defun subprocs (proc)
  (synchronized ('*subprocs*)
    (gethash proc *subprocs*)))

(defun (setf subprocs) (value proc)
  (check-type value list)
  (synchronized ('*subprocs*)
    (setf (gethash proc *subprocs*) value)))

(defun register-subproc (proc subproc)
  "Register SUBPROC as a subprocess of PROC and return SUBPROC."
  (register-subprocs proc subproc)
  (values))

(defun register-subprocs (proc &rest subprocs)
  "Register SUBPROC as a subprocess of PROC."
  (synchronized ('*subprocs*)
    (unionf (subprocs proc) subprocs)
    (values)))

(defun kill-subprocs (proc &key urgent)
  "Kill all subprocesses of PROC."
  (synchronized ('*subprocs*)
    (dolist (subproc (subprocs proc))
      (etypecase subproc
        ;; Arbitrary cleanup.
        (function (funcall subproc))
        (process-info
         (kill-subprocs subproc :urgent urgent)
         (kill-process-group subproc :urgent urgent))))))

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
  ((argv :reader cmd-argv)
   ;; Currently mutable for the :<<< hack.
   (kwargs :accessor cmd-kwargs))
  (:documentation "A single subcommand, with argv and kwargs ready to
  pass to `uiop:launch-program'."))

(defclass substitution (cmd)
  ()
  (:documentation "A command substitution."))

(defclass psub (substitution) ()
  (:documentation "A process substitution."))

(defmethod print-object ((self cmd) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (argv kwargs) self
      (format stream "~a ~s" argv kwargs)))
  self)

(defmethod initialize-instance :after ((self cmd) &key
                                                    ((:argv raw-argv) nil)
                                                    ((:kwargs short-kwargs) nil))
  (assert (evenp (length short-kwargs)))
  (when (null raw-argv)
    (error "No argv!"))
  (with-slots (argv kwargs) self
    (setf argv
          ;; NB UIOP expects simple-strings for arguments.
          (maybe-visual-command
           (and raw-argv
                (cons (exe-string (car raw-argv))
                      (cdr raw-argv))))
          kwargs (expand-keyword-abbrevs short-kwargs))))

(defun parse-cmd (args)
  (multiple-value-bind (argv kwargs) (argv+kwargs args)
    (make 'cmd :argv argv :kwargs kwargs)))

(defun split-pipeline (args)
  "Split ARGS into two values: the last command in the pipeline, and any previous commands."
  (let* ((args (parse-cmd-args args))
         (tail args))
    (loop for new-tail = (member :|\|| tail)
          while new-tail
          do (setf tail (rest new-tail)))
    (values tail
            (ldiff args tail))))

(-> stage-pipeline (list) cmd)
(defun stage-pipeline (cmds)
  "Return CMDS as a single command that can be passed to `launch-pipeline'."
  (reduce (lambda (outer inner)
            (make 'cmd
                  :argv (cmd-argv outer)
                  :kwargs (list* :output inner (cmd-kwargs outer))))
          cmds
          :from-end t))

(-> cmdq (&rest t) cmd)
(define-cmd-variant cmdq shq (cmd &rest args)
  (parse-cmd (cons cmd args)))

(-> psub (&rest t) psub)
(define-cmd-variant psub psub-shell (cmd &rest args)
  (multiple-value-bind (argv kwargs) (argv+kwargs (cons cmd args))
    (make 'psub :argv argv :kwargs kwargs)))

(defun mktemp ()
  (stringify-pathname
   (uiop:with-temporary-file (:pathname p :keep t :prefix "cmd")
     p)))

(defun launch-psubs (argv)
  "Launch any process substitutions in ARGV. Return two values: the
new argv and a list of subprocesses (or other cleanup forms)."
  (with-collectors (new-argv cleanup)
    (dolist (arg argv)
      (if (typep arg 'psub)
          (let ((temp (mktemp)))
            (new-argv temp)
            (cleanup (lambda () (delete-file-if-exists temp)))
            (cleanup (launch-cmd arg :output temp :error-output nil)))
          (new-argv arg)))))

(defun launch-cmd (cmd &rest overrides &key &allow-other-keys)
  "Auxiliary function for launching CMD with overrides."
  (multiple-value-call #'cmd&
    (values-list (cmd-argv cmd))
    (values-list overrides)
    (values-list (cmd-kwargs cmd))))

(-> $cmd (&rest t) (values string &optional))
(define-cmd-variant $cmd $sh (cmd &rest args)
  "Return the results of CMD as a string, stripping any trailing
newlines, like $(cmd) would in a shell.

By default stderr is discarded."
  (chomp
   (with-output-to-string (s)
     (multiple-value-bind (final subs)
         (split-pipeline (cons cmd args))
       (multiple-value-call #'cmd
         (values-list subs)
         :output s
         (values-list final)
         :error-output nil)))))

(-> cmd? (&rest t) (values boolean integer &optional))
(define-cmd-variant cmd? sh? (cmd &rest args)
  "Run a program, returning T if it passed, nil otherwise.
By default the output is discarded.

Returns the actual exit code as a second value."
  (mvlet* ((final subs (split-pipeline (cons cmd args)))
           (exit-code
            (multiple-value-call #'cmd
              (values-list subs)
              :ignore-error-status t
              (values-list final)
              :output nil
              :error-output nil)))
    (if (zerop exit-code)
        (values t 0)
        (values nil exit-code))))

(-> cmd! (&rest t) (values &optional))
(define-cmd-variant cmd! sh! (cmd &rest args)
  "Run CMD purely for its side effects, discarding all output and returning nothing."
  (multiple-value-bind (final subs) (split-pipeline (cons cmd args))
    (multiple-value-call #'cmd
      (values-list subs)
      :output nil
      :error-output nil
      (values-list final)))
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
  (receive (proc tokens args) (apply #'cmd& cmd args)
    (await proc
           :ignore-error-status (getf args :ignore-error-status)
           :tokens tokens)))

(eval-always
  (defun simplify-cmd-args (args)
    "Simplify ARGS at compile time (for compiler macros)."
    (nlet rec ((args-in args)
               (args-out '()))
      (match args-in
        ((list)
         (reverse args-out))
        ((list (and _ (type keyword)))
         (error "Dangling keyword argument to cmd."))
        ((list* (and k (or :pipeline (type subcommand-divider))) rest)
         (rec rest
              (cons k args-out)))
        ((list* (and k (type keyword)) v rest)
         (rec rest
              (cons (if (constantp v)
                        `'(,k ,v)
                        `(list ,k ,v))
                    args-out)))
        ((list* (and s (type string)) xs)
         (rec xs
              (revappend (split-cmd s)
                         args-out)))
        ((list* (and p (type pathname)) xs)
         (rec xs
              (cons (make-string-token (stringify-pathname p))
                    args-out)))
        ((list* x xs)
         (rec xs (cons x args-out)))))))

(-> cmd& (&rest t) (values process-info list list &optional))
(define-cmd-variant cmd& sh& (cmd &rest args)
  "Like `cmd', but run asynchronously and return a handle on the process (as from `launch-program')."
  (mvlet* ((final subs (split-pipeline (cons cmd args)))
           (final (parse-cmd final))
           (subs (mapcar #'cmdq (split-sequence :|\|| subs :remove-empty-subseqs t)))
           (pipeline (append1 subs final)))
    (when *message-hook*
      (run-hook *message-hook*
                (fmt "$ ~{~{~a~^ ~}~^ | ~}"
                     (mapcar (op (mapcar #'shlex:quote _))
                             (flatten-string-tokens
                              (mapcar #'cmd-argv pipeline))))))
    (flet ((launch ()
             (let* ((cmd (stage-pipeline pipeline))
                    (argv (cmd-argv cmd))
                    (kwargs (cmd-kwargs cmd)))
               (values
                (apply #'launch-pipeline
                       argv
                       kwargs)
                (flatten-string-tokens argv)
                kwargs))))
      (if-let (here-string (getf (cmd-kwargs final) :<<<))
        (with-input-from-string (in here-string)
          (symbol-macrolet ((args (cmd-kwargs final)))
            (setf args
                  (let* ((suffix (member :<<< args))
                         (prefix (ldiff args suffix)))
                    (append prefix
                            (list :input in)
                            (cddr suffix)))))
          (launch))
        (launch)))))

(defun launch-pipeline (argv &rest args)
  ;; TODO Need an equivalent to pipefail. Checking the process exit
  ;; codes won't work; on SBCL at least, in a pipeline the exit status
  ;; is apparently always 0.
  (destructuring-bind (&key input
                         (output *standard-output*) (error-output *error-output*)
                       &allow-other-keys) args
    (mvlet* ((prev (and (typep input 'cmd)
                        (launch-cmd input :output :stream)))
             (argv psubs (launch-psubs argv))
             (proc (multiple-value-call #'launch-program-in-dir*
                     argv
                     (if prev
                         (values :input (process-info-output prev))
                         (values))
                     (if (typep output 'cmd)
                         (values :output :stream)
                         (values))
                     (if (typep error-output 'cmd)
                         (values :error-output :stream)
                         (values))
                     (values-list args)
                     :output output
                     :error-output error-output)))
      (apply #'register-subprocs proc psubs)
      (when prev
        (register-subproc proc prev))
      (cond
        ((and (typep output 'cmd)
              (typep error-output 'cmd))
         (error "Not implemented yet"))
        ((typep output 'cmd)
         (lret ((next (launch-cmd output :input (process-info-output proc))))
           (register-subproc next proc)))
        ((typep error-output 'cmd)
         (lret ((enext (launch-cmd error-output :input (process-info-error-output proc))))
           (register-subproc enext proc)))
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
           (apply #'launch-program cmd args)))
    (run-hook *proc-hook* proc)
    proc))

(defun launch-program (cmd &rest args)
  "Like `uiop:launch-program', but unwrapping string tokens at the last possible moment."
  (apply #'uiop:launch-program
         (flatten-string-tokens cmd)
         args))

;;; From https://GitHub.com/GrammaTech/cl-utils/blob/master/shell.lisp
;;; (MIT license).
(defun kill-process-group (process &key urgent)
  "Terminate PROCESS and all its descendants.
On Unix, sends a TERM signal by default, or a KILL signal if URGENT."
  (kill-subprocs process :urgent urgent)
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
            +tr+)
       :ignore-error-status t)
      ;; If non-unix, utilize the standard terminate process
      ;; which should be acceptable in most cases.
      (uiop:terminate-process process :urgent urgent)))

(-> await (process-info &key (:ignore-error-status t) (:tokens list))
  fixnum)
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
        (prog1
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
     (progn
       (kill-subprocs proc)
       (when abnormal?
         (kill-process-group proc))))))

(defun parse-cmd-args (args &key (split t))
  "Lex ARGs.
The result is a list of strings, subcommand dividers, and keyword
arguments."
  (nlet rec ((args args)
             (acc '()))
    (match args
      ((list)
       (nreverse acc))
      ((list* (and arg (type (or string-token substitution))) args)
       (rec args
            (cons arg acc)))
      ;; TODO We should also handle floats, but how to print
      ;; exponents? And what about fractions?
      ((list* (and arg (type integer)) args)
       (rec args
            (cons (make-string-token (princ-to-string arg))
                  acc)))
      ((list* (and arg (type character)) args)
       (rec args
            (cons (make-string-token (string arg))
                  acc)))
      ((list* (and arg (type string)) args)
       (rec args
            (if split
                (revappend (split-cmd arg) acc)
                (cons (make-string-token arg)
                      acc))))
      ((list* (and arg (type pathname)) args)
       (rec args
            (cons (stringify-pathname arg) acc)))
      ((list* (and arg (type list)) args)
       (rec args
            (revappend (parse-cmd-args arg :split nil)
                       acc)))
      ((list (and _ (type keyword)))
       (error "Dangling keyword argument to cmd."))
      ((list* (and _ (eql :pipeline)) args)
       (rec (cons :|\|| args) acc))
      ((list* (and k (type subcommand-divider)) args)
       (rec args (cons k acc)))
      ((list* (and k (type keyword)) v args)
       (rec args
            (list* v k acc)))
      ((list* arg _)
       (error "Can't use ~a as a cmd argument." arg)))))

(defun argv+kwargs (args)
  "Parse ARGS and split them into an argv and keyword arguments."
  (nlet rec ((args (parse-cmd-args args))
             (argv '())
             (kwargs '()))
    (ematch args
      ((list)
       (values (nreverse argv)
               (nreverse kwargs)))
      ((list* (and arg (type (or string-token substitution))) args)
       (rec args
            (cons arg argv)
            kwargs))
      ((list* (type subcommand-divider) _)
       (error "Subcommand delimiter in cmd args: ~a" args))
      ((list* (and k (type keyword)) v args)
       (rec args
            argv
            (list* v k kwargs))))))

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

(-> stringify-pathname ((or string pathname))
    (simple-array character (*)))
(defun stringify-pathname (arg)
  (unless (pathnamep arg)
    (return-from stringify-pathname arg))
  (lret ((string
          (coerce
           (let ((string (native-namestring arg)))
             (if (and (os-windows-p)
                      (featurep :ccl)
                      (position #\/ string))
                 ;; Work around a CCL bug; issue #103 on GitHub.
                 (substitute #\\ #\/ string)
                 string))
           '(simple-array character (*)))))
    (when (string^= "-" string)
      ;; Should we ignore the unsafe file names if `--' or
      ;; `---' is already present in the list of tokens?
      (cerror "Allow the unsafe file name"
              "File name ~a begins with a dash"
              string))))

(defun exe-string (p)
  (etypecase p
    ((or string pathname)
     (stringify-pathname (exe p)))
    (string-token
     (make-string-token
      (exe-string (string-token-string p))))))

(defun split-cmd (cmd)
  (mapcar (lambda (arg)
            (assure (or keyword string-token)
              (or (find arg +redirection-operators+ :test #'string=)
                  (find arg +subcommand-dividers+ :test #'string=)
                  (make-string-token arg))))
          (shlex:split cmd :whitespace-split nil
                           :punctuation-chars t)))
