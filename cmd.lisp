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
   :directory-pathname-p
   :ensure-directory-pathname
   :directory-exists-p)
  (:import-from :trivia :match :ematch)
  (:import-from :shlex)
  (:import-from :uiop/launch-program
   :process-info)
  (:import-from #:trivial-garbage
                #:make-weak-hash-table)
  (:export
   :cmd :$cmd :cmd? :cmd! :cmd&
   :sh :$sh :sh? :sh! :sh&
   :parse-cmd-dsl
   :with-cmd-dir
   :with-working-directory
   :current-directory
   :psub
   :psub-echo
   :psub-format
   :*shell*
   :*visual-commands*
   :*command-wrappers*
   :*terminal*
   :vterm-terminal
   :*cmd-env*
   :*cmd-path*
   :*null-output*
   :*null-error-output*
   :cmd-error
   :cmd-error-stderr))
(in-package :cmd)

;;; External executables, isolated for Guix compatibility.
(def +env+ "env")
(def +kill+ "kill")
(def +ps+ "ps")
(def +pwd+ "pwd")
(def +sh+ "/bin/sh")
(def +tr+ "tr")

(defparameter *null-output* nil
  "Null device for standard output.

By binding this variable you can redirect output that would otherwise
be sent to the null device.")

(defparameter *null-error-output* nil
  "Null device for standard error.

By binding this variable you can redirect error output that would
otherwise be sent to the null device.

Note that when error output is not specifically redirected, this
variable is bound to a stream that stashes stderr output for error
reporting.")

(defvar *shell*
  (let ((shell (getenv "SHELL")))
    (if (emptyp shell)
        (if (os-unix-p)
            +sh+
            "cmd.exe")
        shell))
  "The shell to use for shell commands.

Defaults to $SHELL.")

(declaim (type (soft-list-of cons) *cmd-env*))
(defvar *cmd-env* '()
  "Alist of extra environment variables.")

(declaim (type (soft-list-of (or pathname string)) *cmd-path*))
(defvar *cmd-path* '()
  "Extra directories to check for executables.")

(defun cmd-env (&aux (env *cmd-env*) (path-list *cmd-path*))
  (assert (every #'absolute-pathname-p path-list))
  (let* ((old-path-list
           (split-sequence #\: (uiop:getenv "PATH")))
         (new-path-list
           (mapcar #'native-namestring path-list))
         (path-env
           (and new-path-list
                (not (subsetp new-path-list
                              old-path-list
                              :test #'equal))
                (cons "PATH"
                      (fmt "~{~a~^:~}"
                           (nub (append new-path-list old-path-list)))))))
    (declare (type (soft-list-of string) old-path-list new-path-list))
    (if path-env
        (cons path-env env)
        env)))

(defun wrap-cmd-env (cmd &aux (env (cmd-env)))
  (if (null env) cmd
      (if (not (os-unix-p))
          (progn
            (cerror "Run without the extra environment variables"
                    "Cannot use ~s, not on Unix."
                    '*cmd-env*)
            cmd)
          `(,+env+ ,@(loop for (k . v) in env
                           collect (fmt "~a=~a"
                                        (validate-env-var k)
                                        v))
                   ,@cmd))))

(-> validate-env-var (string-designator) string)
(defun validate-env-var (name)
  "Check that NAME is a valid (portable) name for an environment
variable."
  (let ((name (string name)))
    (if (and (every (lambda (char)
                      (or (eql char #\_)
                          (digit-char-p char 10)
                          (and (alpha-char-p char)
                               (ascii-char-p char))
                          (find char "!%,@")))
                    name)
             (or (emptyp name)
                 (not (digit-char-p (aref name 0) 10))))
        name
        (error "Bad name for an environment variable: ~a" name))))

(-> resolve-dir ((or string pathname))
  (values absolute-directory-pathname &optional))
(defun resolve-dir (dir)
  "Resolve DIR into an absolute directory based on
`*default-pathname-defaults*`, supplemented with the OS-level working
directory if that is not absolute."
  (let ((dir (ensure-directory-pathname dir)))
    (if (typep dir 'absolute-pathname) dir
        (ensure-directory-pathname
         (if (typep *default-pathname-defaults* 'absolute-pathname)
             (path-join *default-pathname-defaults* dir)
             (path-join (uiop:getcwd)
                        *default-pathname-defaults*
                        dir))))))

(-> current-directory () (values absolute-directory-pathname &optional))
(defun current-directory ()
  "Get the absolute current directory based on `*default-pathname-defaults*'."
  (resolve-dir *default-pathname-defaults*))

(-> (setf current-directory) (pathname)
  (values absolute-directory-pathname &optional))
(defun (setf current-directory) (value)
  (setf *default-pathname-defaults*
        (assure absolute-directory-pathname
          (resolve-dir value))))

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
  '(:< :> :<> :1> :>> :1>> :|>\|| :2> :2>> :|2>\|| :&> :>& :&>> :>>& :<<< :>? :2>?)
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

(deftype parseable ()
  '(or keyword string list
    string-token substitution subcommand-divider
    integer character pathname))

(defun expand-redirection-abbrev (keyword)
  (assure list
    (case-of (or (eql :in) redirection-operator) keyword
      (:in '(:directory _))
      (:< '(:input _))
      ((:> :1>) '(:output _))
      ((:<>) '(:input _ :output _))
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

(defun expand-keyword-aliases (args)
  (collecting
    (doplist (k v args)
      (let ((exp (expand-redirection-abbrev k)))
        (cond (exp
               (apply #'collect (substitute v '_ exp)))
              ((eql k :check)
               (collect :ignore-error-status (not v)))
              (t (collect k v)))))))

(defun call/cmd-dir (fn dir)
  (let* ((*default-pathname-defaults* (resolve-dir dir)))
    (funcall fn)))

(defmacro with-working-directory ((dir) &body body)
  "Run BODY with DIR as the current directory.
Calls to `cmd' and its variants with the dynamic extent of the
`with-working-directory' form will use `dir' as their working directory."
  (with-thunk (body)
    `(call/cmd-dir ,body ,dir)))

(defmacro with-cmd-dir (dir &body body)
  "Deprecated; use `with-working-directory' instead."
  (simple-style-warning "~s is deprecated, please use ~s"
                        'with-cmd-dir
                        'with-working-directory)
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
             (when-let (name (pathname-name arg))
               (namestring name)))
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
                       (when-let (basename (basename (first command)))
                         (find basename
                               *command-wrappers*
                               :test #'string=)))
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
   (let ((*package* (find-package :cmd/cmd)))
     (write-to-string
      `(progn
         (vterm)
         (vterm-insert ,(string-join (flatten-string-tokens cmd) " "))
         (vterm-send-return))
      :case :downcase))))

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
         (kill-process-group subproc :urgent urgent))))
    (remhash proc *subprocs*)))

(defcondition cmd-error (uiop:subprocess-error)
  ((stderr :initarg :stderr :type string :reader cmd-error-stderr))
  (:default-initargs :stderr "")
  (:report (lambda (c s)
             (format s "Subprocess ~@[~S~% ~]~@[with command ~S~% ~]exited with error~@[ code ~D~]~@[~2%=== ERROR OUTPUT ===~%~a~]"
                     (uiop:subprocess-error-process c)
                     (uiop:subprocess-error-command c)
                     (uiop:subprocess-error-code c)
                     (let ((stderr (cmd-error-stderr c)))
                       (unless (emptyp stderr)
                         (ellipsize stderr 10000)))))))

(defun get-stderr-output-stream-string (s)
  "Get output from S.
Note this will only be called if an error is signaled."
  (finish-output s)
  (let* ((end (file-position s))
         (seq (make-array end :element-type (stream-element-type s))))
    (file-position s 0)
    (read-sequence seq s)
    (prog1 seq
      (ignore-errors
       (close s)))))

(defun call/stderr-file (fn)
  (uiop:with-temporary-file (:pathname p :keep nil)
    (with-open-file (s p
                       :direction :io
                       :element-type 'character
                       :allow-other-keys t
                       :if-exists :overwrite
                       ;; For CCL.
                       :sharing :external)
      #-windows (delete-file p)
      (handler-bind ((uiop:subprocess-error
                       (lambda (c)
                         (error
                          'cmd-error
                          :process (uiop:subprocess-error-process c)
                          :command (uiop:subprocess-error-command c)
                          :code (uiop:subprocess-error-code c)
                          :stderr (get-stderr-output-stream-string s)))))
        (funcall fn s)))))

(defmacro with-stderr-file ((var &key) &body body)
  (with-thunk (body var)
    `(call/stderr-file ,body)))

(defmacro with-stderr-caching ((&key) &body body)
  (with-thunk (body)
    `(if *null-error-output*
         (funcall ,body)
         (with-stderr-file (*null-error-output*)
           (,body)))))

(defmacro define-cmd-variant (name sh-name lambda-list &body body)
  (let ((docstring (and (stringp (car body)) (pop body))))
    `(progn
       (defun ,name ,lambda-list
         ,@(unsplice docstring)
         (with-stderr-caching ()
           ,@body))
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

(defclass substitution ()
  ()
  (:documentation "A command substitution."))

(defgeneric launch-substitution (sub)
  (:documentation "Launch a command substitution.
Should always return two values, both lists:
1. A list of new arguments for the argv.
2. A list of cleanup forms."))

(defclass psub-echo (substitution)
  ((string :initarg :string :reader psub-echo-string :type string))
  (:documentation "A process substitution that just echoes a string.")
  (:default-initargs
   :string (error "No string!")))

(defun psub-echo (string)
  "Allow passing STRING to a command that expects a file.
This is practically equivalent to

    (psub \"echo\" (list string))

Except that it doesn't actually launch an external program."
  (check-type string string)
  (make 'psub-echo :string string))

(defun psub-format (control-string &rest args)
  "Format ARGS using CONTROL-STRING and pass the result as a file to
the enclosing command.

This is practically equivalent to

    (psub \"echo\" (list (format nil \"?\" control-string args))

Except that it doesn't actually launch an external program."
  (declare (dynamic-extent args))
  (psub-echo (fmt "~?" control-string args)))

(define-compiler-macro psub-format (&whole call control-string &rest args)
  (if (stringp control-string)
      `(psub-format (formatter ,control-string) ,@args)
      call))

(defmethod launch-substitution ((sub psub-echo))
  (let ((temp (mktemp)))
    (write-string-into-file (psub-echo-string sub)
                            temp
                            :if-exists :rename
                            :if-does-not-exist :create)
    (values (list (stringify-pathname temp))
            (list (lambda () (delete-file-if-exists temp))))))

(defclass psub (substitution cmd) ()
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
          kwargs (expand-keyword-aliases short-kwargs))))

(defmethod launch-substitution ((arg psub))
  (let ((temp (mktemp)))
    (values
     (list (stringify-pathname temp))
     (list (lambda () (delete-file-if-exists temp))
           (launch-cmd arg :output temp :error-output nil)))))

(defun parse-cmd (args)
  (receive (argv kwargs) (argv+kwargs args)
    (make 'cmd :argv argv :kwargs kwargs)))

(defun parse-cmd-dsl (command)
  "Parse COMMAND like `cmd' does.
Returns two values: the fully tokenized command, and a list of
redirection arguments (in the format expected by
`uiop:launch-program').

This can be used to write your own functions that support the `cmd' DSL."
  (check-type command list)
  (let ((cmd (parse-cmd command)))
    (values (flatten-string-tokens (cmd-argv cmd))
            (flatten-string-tokens (cmd-kwargs cmd)))))

(defun split-pipeline (args)
  "Split ARGS into two values: the last command in the pipeline, and any previous commands."
  (let* ((args (parse-cmd-args args))
         (tail args))
    (loop for new-tail = (member :|\|| tail)
          while new-tail
          do (setf tail (rest new-tail)))
    (values tail
            (ldiff args tail))))

(-> stage-pipeline (list) (values cmd &optional))
(defun stage-pipeline (cmds)
  "Return CMDS as a single command that can be passed to `launch-pipeline'."
  (reduce (lambda (outer inner)
            (make 'cmd
                  :argv (cmd-argv outer)
                  :kwargs (list* :output inner (cmd-kwargs outer))))
          cmds
          :from-end t))

(-> cmdq (&rest t) (values cmd &optional))
(define-cmd-variant cmdq shq (cmd &rest args)
  (parse-cmd (cons cmd args)))

(-> psub (&rest t) (values psub &optional))
(define-cmd-variant psub psub-shell (cmd &rest args)
  (receive (argv kwargs) (argv+kwargs (cons cmd args))
    (make 'psub :argv argv :kwargs kwargs)))

(-> get-tmpfs ()
  (values (or null absolute-directory-pathname) &optional))
(defun get-tmpfs ()
  "Get a suitable tmpfs."
  (declare (notinline $cmd))            ;Bootstrapping.
  (when (os-unix-p)
    (or (let ((dir (getenv "XDG_RUNTIME_DIR")))
          (unless (emptyp dir)
            (ensure-directory-pathname dir)))
        (or (directory-exists-p
             (make-pathname
              :directory `(:absolute
                           "run"
                           "user"
                           ,($cmd "id -u"))))
            (directory-exists-p #P"/run/shm")
            (directory-exists-p #P"/dev/shm")))))

(defun mktemp ()
  "Create a temporary file for use with process substition.
When possible use a tmpfs."
  (let ((uiop:*temporary-directory*
          (or (get-tmpfs) uiop:*temporary-directory*)))
    (uiop:with-temporary-file (:pathname p :keep t :prefix "cmd")
      p)))

(defun launch-psubs (argv)
  "Launch any process substitutions in ARGV. Return two values: the
new argv and a list of subprocesses (or other cleanup forms)."
  (with-collectors (new-argv cleanup)
    (dolist (arg argv)
      (if (typep arg 'substitution)
          (receive (new-args cleanups)
              (launch-substitution arg)
            (mapc #'new-argv new-args)
            (mapc #'cleanup cleanups))
          (new-argv arg)))))

(defun launch-cmd (cmd &rest overrides &key &allow-other-keys)
  "Auxiliary function for launching CMD with overrides."
  (multiple-value-call #'cmd&
    (values-list (cmd-argv cmd))
    (values-list overrides)
    (values-list (cmd-kwargs cmd))))

(-> $cmd (&rest t) (values string integer &optional))
(define-cmd-variant $cmd $sh (cmd &rest args)
  "Return the results of CMD as a string, stripping any trailing
newlines, like $(cmd) would in a shell.

As a second value, return the error status.

By default stderr is discarded."
  (let* ((exit-code)
         (string
           (chomp
            (with-output-to-string (s)
              (receive (final subs)
                  (split-pipeline (cons cmd args))
                (setf exit-code
                      (multiple-value-call #'cmd
                        (values-list subs)
                        :output s
                        (values-list final)
                        :error-output *null-error-output*)))))))
    (values string exit-code)))

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
  (receive (final subs) (split-pipeline (cons cmd args))
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
`uiop:launch-program'. Certain keywords are treated as abbreviations:
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
        ((list* (and k (type keyword)) v rest)
         (rec rest
              (cons (if (constantp v)
                        `'(,k ,v)
                        `(list ,k ,v))
                    args-out)))
        ((list* (and x (type parseable)) xs)
         (etypecase-of parseable x
           (string
            (let ((tokens (split-cmd x)))
              (rec xs
                   ;; A subtlety: an argument after a literal keyword
                   ;; (that is not a subcommand divider) is not
                   ;; parsed, but an argument after a string that
                   ;; parses as a keyword is itself parsed. To
                   ;; preserve that behavior we can't expand at
                   ;; runtime if the last token is a keyword.
                   (if (typep (lastcar tokens) 'keyword)
                       (cons x args-out)
                       (revappend tokens args-out)))))
           (pathname
            (rec xs
                 (cons (make-string-token (stringify-pathname x))
                       args-out)))
           (integer
            (rec xs
                 (cons (make-string-token (princ-to-string x))
                       args-out)))
           (character
            (rec xs
                 (cons (make-string-token (string x))
                       args-out)))
           ((or subcommand-divider keyword list
                string-token substitution)
            (rec xs (cons x args-out)))))
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
                    (argv (flatten-string-tokens (cmd-argv cmd)))
                    (kwargs (flatten-string-tokens (cmd-kwargs cmd))))
               (values
                (apply #'launch-pipeline
                       argv
                       kwargs)
                argv
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
                         (output *standard-output*)
                         (error-output
                          (make-broadcast-stream
                           ;; Stash stderr output for error reporting.
                           *null-error-output*
                           *error-output*))
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
  "Run a program (with `uiop:launch-program') in the current base directory."
  (let ((dir (stringify-pathname
              (or (getf args :directory)
                  (current-directory)))))
    (apply #'launch-program-in-dir dir tokens
           (remove-from-plist args :directory))))

(defun override-default-output-and-error-output (args)
  "Override null output with `*null-output*' and null error output
with `*null-error-output*'."
  (destructuring-bind (&key output error-output &allow-other-keys) args
    (append
     (and (null output)
          `(:output ,*null-output*))
     (and (null error-output)
          `(:error-output ,*null-error-output*))
     args)))

(defun launch-program-in-dir (dir tokens
                              &rest args
                              &key
                              &allow-other-keys)
  (let* ((cmd
           (wrap-cmd-env
            ;; NB The :directory argument to launch-program may end up
            ;; calling `chdir', which is unacceptable.
            (wrap-with-dir dir tokens)))
         (args
           (override-default-output-and-error-output args))
         (proc
           (apply #'uiop:launch-program cmd args)))
    (run-hook *proc-hook* proc)
    proc))

;;; From https://GitHub.com/GrammaTech/cl-utils/blob/master/shell.lisp
;;; (MIT license).
(defun kill-process-group (process &key urgent)
  "Terminate PROCESS and all its descendants.
On Unix, sends a TERM signal by default, or a KILL signal if URGENT."
  (uiop:close-streams process)
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
       (uiop:close-streams proc)
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
      ((list (and _ (type keyword)))
       (error "Dangling keyword argument to cmd."))
      ((list* (and k (type subcommand-divider)) args)
       (rec args (cons k acc)))
      ((list* (and k (type keyword)) v args)
       (rec args (list* v k acc)))
      ((list* (and arg (type parseable)) args)
       (etypecase-of parseable arg
         ((or string-token substitution keyword subcommand-divider)
          (rec args (cons arg acc)))
         ;; TODO We should also handle floats, but how to print
         ;; exponents? And what about fractions?
         (integer
          (rec args
               (cons (make-string-token (princ-to-string arg))
                     acc)))
         (character
          (rec args
               (cons (make-string-token (string arg))
                     acc)))
         (string
          (rec args
               (if split
                   (revappend (split-cmd arg) acc)
                   (cons (make-string-token arg)
                         acc))))
         (pathname
          (rec args
               (cons (make-string-token (stringify-pathname arg))
                     acc)))
         (list
          (rec args
               (revappend (parse-cmd-args arg :split nil)
                          acc)))))
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
  (etypecase arg
    (string (coerce arg '(simple-array character (*))))
    (pathname
     (stringify-pathname
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
                  string)))))))

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
