(defpackage :cmd/test
  (:use :cl :cmd/cmd :fiveam :alexandria :serapeum)
  (:export :run-tests))
(in-package :cmd/test)

(def-suite cmd)
(in-suite cmd)

(defun run-tests ()
  (run! 'cmd))

(test filename-starts-with-dash
  (signals error
    (eval '(cmd "ls" #p"-file"))))

(test unix-cmd
  (if (uiop:os-unix-p)
      (progn
        (is (equal* "hello"
                    ($cmd "echo hello")
                    ($cmd '("echo" "hello"))
                    ($cmd "echo" #p"hello")
                    ($cmd '("echo" #p "hello"))))
        (let ((file (asdf:system-relative-pathname :cmd "test/literal.txt")))
          (is (equal (chomp (read-file-into-string file))
                     ($cmd "cat" file)))))
      (skip "Not on Unix")))
