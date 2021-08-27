(defpackage :cmd/test
  (:use :cl :cmd/cmd :fiveam :alexandria :serapeum)
  (:import-from :uiop :os-unix-p)
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
  (let ((string
          (with-output-to-string (*standard-output*)
            (nest
             (cmd "cat" "/usr/share/dict/words" :>)
             (cmdq "sort" :>)
             (cmdq "uniq -c" :>)
             (cmdq "sort -nr" :>)
             (cmdq "head -3")))))
    (is (length= 3 (lines string)))))
