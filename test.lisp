#!/usr/local/bin/sbcl --script
;;;; -*- lisp -*-
;;;;============================================================================
;;;; Fibers API for Common Lisp
;;;; Copyright (c) 2011 Valentin Pavlov <x.pavlov@gmail.com>
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;; 
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.
;;;;============================================================================
(require :asdf)
(setf asdf:*asdf-verbose* nil)
(require :cl-fiber)
(use-package :cl-fiber)

(defun/cc task-a ()
  (dotimes (i 3000000)
    (when (= (mod i 1000000) 0)
      (format t "~&[TASK A: ~a]~%" i)
      (fiber-yield)))
  100)

(defun/cc task-b (fiber)
  (format t "~&[TASK B: task-a returned ~a]~%" (join-fiber fiber)))

(defun/cc simple-repl ()
  (loop
     (fresh-line)
     (write-string ">> ")
     (force-output)
     (fiber-wait (ch (read-char-no-hang))
		 (unread-char ch)
		 (princ (eval (read))))))

(defun test ()
  (let ((fiber-a (make-fiber "task a" #'task-a)))
    (make-fiber "task b" #'task-b fiber-a)
    (run-fiber-scheduler)))

(test)
