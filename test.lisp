(uiop:define-package :cmd/test
  (:use :cl :cmd/cmd :fiveam :alexandria :serapeum)
  ;; Internal symbols.
  (:import-from
    :cmd/cmd
    :expand-keyword-abbrevs
    :split-cmd
    :flatten-string-tokens
    :kill-process-group
    :wrap-cmd-env
    :vterm-terminal)
  (:import-from :uiop :os-unix-p :subprocess-error)
  (:export :run-tests))
(in-package :cmd/test)

(def-suite cmd)
(in-suite cmd)

(defun run-tests ()
  (run! 'cmd))

(test filename-starts-with-dash
  (signals error
    (eval '(cmd "ls" #p"-file"))))

(defmacro unix-test (name &body body)
  `(test ,name
     (if (os-unix-p)
         (progn ,@body)
         (skip "Not on Unix"))))

(unix-test unix-cmd
  (is (equal* "hello"
              ($cmd "echo hello")
              ($cmd '("echo" "hello"))
              ($cmd "echo" #p"hello")
              ($cmd '("echo" #p "hello"))
              ($sh "echo hello")
              ($sh "echo 'hello'")
              ($sh "echo \"hello\"")))
  (let ((file (asdf:system-relative-pathname :cmd "test/literal.txt")))
    (is (equal (chomp (read-file-into-string file))
               ($cmd "cat" file)))))

(unix-test here-string
  (is (equal* ($cmd "bash -c" '("read x; echo \"$x\"") :<<< "hello")
              ($cmd "bash -c" '("read x; echo \"$x\"; exit 1") :<<< "hello"
                    :ignore-error-status t)
              ($sh "read x; echo \"$x\"" :<<< "hello")
              "hello")))

(unix-test pipelines
  (is (string= "oof" ($cmd "echo 'foo' | rev")))
  (is (string= (fmt "rab~%oof") ($cmd "echo -e 'foo\\nbar' | rev | tac")))
  (let ((string1
          ($cmd "cat /usr/share/dict/words"
               "|" '("sort")
               "|" '("uniq" "-c")
               "|" '("sort" "-nrs")
               "|" '("head" "-3")))
        (string2
          ($cmd "cat /usr/share/dict/words | sort | uniq -c | sort -nrs | head -3")))
    (is (length= 3
                 (lines string1)
                 (lines string2)))
    (is (equal string1 string2))))

(test expand-keyword-abbrevs
  (is
   (equal
    (expand-keyword-abbrevs '(:|2>\|| "bar.txt"))
    '(:if-error-output-exists :supersede
      :error-output "bar.txt"))))

(test split-cmd
  (flet ((split-cmd (x) (flatten-string-tokens (split-cmd x))))
    (is (equal '("x" :> "y") (split-cmd "x > y")))
    (is (equal '("x" :|\|| "y" :|\|| "z") (split-cmd "x | y | z")))))

(unix-test pipefail
  (signals subprocess-error
    (cmd "bash -c 'echo hello; exit 1'" :> nil))
  ;; TODO This doesn't work on CCL or SBCL. The problem is that the
  ;; exit code actually gets set to zero.
  ;; (signals subprocess-error
  ;;   (cmd "bash -c 'echo hello; exit 1' | rev"))
  )

(unix-test tokenize-regression
  (is-true (cmd? "echo \"sleep 5000\" | grep -qo -e 'sleep 5000'")))

(unix-test kill-pipeline
  (let ((proc (cmd& "sleep 5000 | echo 'done'")))
    (kill-process-group proc)
    (is (null (cmd? "pidof sleep")))))

(unix-test psub
  (is-true (cmd? "diff" (psub "echo 'x'") (psub "echo 'x'")))
  (is (equal "1c1"
             (first
              (lines
               ($cmd "diff"
                     :ignore-error-status t
                     (psub "echo -e 'hello\nworld'")
                     (psub "echo -e 'goodbye\nworld'")))))))

(unix-test psub-echo
  (is-true (cmd? "diff" (psub-echo "x") (psub-echo "x")))
  (is (equal "2c2"
             (first
              (lines
               ($cmd "diff"
                     :ignore-error-status t
                     (psub-format "hello~%world")
                     (psub-format "hello~%dolly")))))))

(unix-test stringify-regression
  (finishes (cmd! :in "/tmp" "ls")))

(unix-test output-file-regression
  (let ((file (string+ "/tmp/cmd-hello-" (random 10000))))
    ;; Bug only happens when file gets passed through as a string token.
    (cmd (fmt "echo -n hello > ~a" file))
    (is (equal "hello" (read-file-into-string file)))
    (uiop:delete-file-if-exists file)))

(test with-working-directory
  (let* ((tmp (uiop:temporary-directory))
         (new-dir-name (string+ "cmd-test-dir-" (random 10000) ".foo"))
         (new-dir
           (ensure-directories-exist
            (uiop:ensure-directory-pathname
             (path-join tmp new-dir-name)))))
    (unwind-protect
         (with-working-directory (new-dir)
           (is (equal *default-pathname-defaults*
                      (path-join tmp
                                 (make-pathname
                                  :directory `(:relative ,new-dir-name)))))
           (let ((subdir
                   (ensure-directories-exist
                    (path-join new-dir
                               (make-pathname
                                :directory '(:relative "subdir.foo"))))))
             (with-working-directory ("subdir.foo")
               (equal *default-pathname-defaults* subdir))))
      (uiop:delete-directory-tree
       new-dir :validate (op (string*= ".foo" (namestring _)))))))

(unix-test dont-parse-keyword-value-as-arg
  (with-working-directory ((uiop:temporary-directory))
    (let ((subdir (string+ "temp-" (random 10000) ".foo")))
      (cmd "mkdir" subdir)
      (unwind-protect
           (with-working-directory (subdir)
             (let* ((x (string+ "x-" (random 10000) ".foo"))
                    (y (string+ "y-" (random 10000) ".foo"))
                    (string (string+ x " " y)))
               (cmd "echo hello" :> string)
               (is (uiop:file-exists-p string))
               (is (not (uiop:file-exists-p x)))
               (is (not (string*= y (read-file-into-string string))))))
        (uiop:delete-directory-tree
         (path-join (uiop:temporary-directory)
                    (make-pathname
                     :directory `(:relative ,subdir)))
         :validate (op (string*= ".foo" (namestring _))))))))

(unix-test do-parse-keywordlike-string-value-as-arg
  (with-working-directory ((uiop:temporary-directory))
    (let ((subdir (string+ "temp-" (random 10000) ".foo")))
      (cmd "mkdir" subdir)
      (unwind-protect
           (with-working-directory (subdir)
             (let* ((x (string+ "x-" (random 10000) ".foo"))
                    (y (string+ "y-" (random 10000) ".foo"))
                    (string (string+ x " " y)))
               (cmd "echo hello" ">" string)
               (is (not (uiop:file-exists-p string)))
               (is (uiop:file-exists-p x))
               (is (string*= y (read-file-into-string x)))))
        (uiop:delete-directory-tree
         (path-join (uiop:temporary-directory)
                    (make-pathname
                     :directory `(:relative ,subdir)))
         :validate (op (string*= ".foo" (namestring _))))))))

(test wrap-cmd-env
  (is (equal '("hello")
             (let ((*cmd-env* '()))
               (wrap-cmd-env '("hello")))))
  (is (equal '("env" "GIT_PAGER=cat" "hello")
             (let ((*cmd-env* '(("GIT_PAGER" . "cat"))))
               (wrap-cmd-env '("hello"))))))

(unix-test cmd-env-no-escape
  (is (equal "foo=bar"
             (let* ((var-name (gensym))
                    (*cmd-env* `((,var-name . "foo=bar"))))
               ($cmd (fmt "sh -c 'echo $~a'" var-name))))))

(unix-test cmd-env-valid-name
  (signals error
    (let* ((*cmd-env* `(("invalid=name" . "foo=bar"))))
      ($cmd (fmt "sh -c 'echo ${invalid-name}'")))))

(test vterm-cmd-package
  (let ((*package* (find-package :keyword)))
    (is (some (op (search "(vterm)" _))
              (vterm-terminal '())))))

(defun foo (string)
  (cmd:$cmd "bash -c 'echo $0; echo busted >&2; exit 1'" string))

(defun some-user-function ()
  (let ((cmd::*null-error-output* (make-string-output-stream)))
    (handler-case (foo "hello")
      (uiop/run-program:subprocess-error ()
        (princ (get-output-stream-string cmd::*null-error-output*))
        ;; print or resignal an error using stderr output here
        ))))

(unix-test cmd-null-error-override
  (is (string= (fmt "busted~%")
               (with-output-to-string (*standard-output*)
                 (some-user-function)))))

(unix-test private-stderr-stream-regression
  (let ((error
          (handler-case
              (foo "hello")
            (error (e) e))))
    (is (string*= "busted" (princ-to-string error)))))

(test pathname-with-space
  (with-working-directory ((asdf:system-relative-pathname "cmd" "test/"))
    (is (uiop:directory-exists-p "foo bar"))
    (is (string= "a file" ($cmd "ls" #p"foo bar")))
    (locally (declare (notinline $cmd))
      (is (string= "a file" ($cmd "ls" #p"foo bar"))))))
