(defpackage :cmd/hooks
  (:use :cl :alexandria :serapeum)
  (:export :*message-hook* *proc-hook*))
(in-package :cmd/hooks)

(defvar *message-hook* '())

(defvar *proc-hook* '())
