(uiop:define-package :cmd/test
  (:use :cl :cmd/cmd :fiveam :alexandria :serapeum)
  (:import-from
   :cmd/cmd
   :expand-keyword-abbrevs
   :split-cmd
   :flatten-string-tokens
   :kill-process-group)
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
          ($cmd "cat" "/usr/share/dict/words"
                :pipeline "sort"
                :pipeline "uniq -c"
                :pipeline "sort -nr"
                :pipeline "head -3"))
        (string2
          ($cmd "cat /usr/share/dict/words | sort | uniq -c | sort -nr | head -3")))
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
  (let ((*standard-output* (make-broadcast-stream)))
    (signals subprocess-error
      (cmd "bash -c 'echo hello; exit 1'"))
    ;; TODO This doesn't work on CCL or SBCL. The problem is that the
    ;; exit code actually gets set to zero.
    ;; (signals subprocess-error
    ;;   (cmd "bash -c 'echo hello; exit 1' | rev"))
    ))

(unix-test tokenize-regression
  (is-true (cmd? "echo \"sleep 5000\" | grep -qo -e 'sleep 5000'")))
