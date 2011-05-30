#!/usr/local/bin/sbcl --script
;;;; -*- lisp -*-
;;;;============================================================================
;;;; Fiber API for Common Lisp
;;;; Copyright (C) 2011 Valentin Pavlov <vpavlov@rila.bg>
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;;;----------------------------------------------------------------------------
;;;; $Id: test.lisp,v 1.2 2011/05/30 08:03:34 vpavlov Exp $
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
