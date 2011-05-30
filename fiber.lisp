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
(in-package :cl-fiber)

;;; Conditions

(define-condition fiber-error (error)
  ((fiber :reader fiber-error-fiber :initarg :fiber))
  (:documentation
   "Conditions of type FIBER-ERROR are signalled when fiber operations fail.
The offending fiber is initialized by the :FIBER initialization argument and
read by the function FIBER-ERROR-FIBER."))

(setf
 (documentation 'fiber-error-fiber 'function)
 "Return the offending fiber that the FIBER-ERROR pertains to.")

(define-condition join-fiber-error (fiber-error) ()
  (:report (lambda (c s)
             (format s "Joining fiber failed: fiber ~A ~
                        did not return normally."
                     (fiber-error-fiber c))))
  (:documentation
   "Signalled when joining a fiber fails due to abnormal exit of the fiber
to be joined. The offending fiber can be accessed using
FIBER-ERROR-FIBER."))

(deftype fiber-name () 'simple-string)

(defstruct (fiber (:constructor %make-fiber))
  "Fiber type."
  (name      nil :type (or fiber-name null))
  (%alive-p  nil :type boolean)
  (cc        nil :type (or function null))
  (result    nil :type list))

(defvar *current-fiber* nil
  "The one fiber that is currently running.")

(setf
 (documentation 'fiber-name 'function)
 "Name of the fiber. Can be assigned to using SETF. Fiber names can be
arbitrary printable objects, and need to be unique.")

(defmethod print-object ((fiber fiber) stream)
  (print-unreadable-object (fiber stream :type t :identity t)
    (let* ((cookie (list fiber))
           (info (if (fiber-alive-p fiber)
                     :running
                     (multiple-value-list
                      (join-fiber fiber :default cookie))))
           (state (if (eq :running info)
                      info
                      (if (eq cookie (car info))
                          :aborted
                          :finished)))
           (values (when (eq :finished state) info)))
      (format stream
              "~@[~S ~]~:[~A~;~A~:[ no values~; values: ~:*~{~S~^, ~}~]~]"
              (fiber-name fiber)
              (eq :finished state)
              state
              values))))

(defun fiber-alive-p (fiber)
  "Return T if FIBER is still alive."
  (fiber-%alive-p fiber))

;; A fiber is eligible for gc iff it has finished and there are no
;; more references to it. This list is supposed to keep a reference to
;; all running fibers.
(defvar *all-fibers* ())

;; These are used internally by the scheduler.
(defvar *ready-fibers* ())  ; A list of fibers waiting to be scheduled
(defvar *halt* (gensym))    ; Thrown when there are no more fibers to run
(defvar *yield* (gensym))   ; Thrown on yield, to back up the stack

(defun list-all-fibers ()
  "Return a list of the live fibers."
  (copy-list *all-fibers*))

;; Called when a fiber exits
(defun handle-fiber-exit (fiber)
  (setf (fiber-%alive-p fiber) nil)
  (setf (fiber-cc fiber) nil)
  (setq *all-fibers* (delete fiber *all-fibers*)))

;; Advance the scheduler, selecting a new fiber to run, if any. This is called
;; when a fiber yields or when a fiber function completes.
(defun fiber-sched-step ()
  (if *ready-fibers*
      (let* ((fiber (car *ready-fibers*))
	     (cc (fiber-cc fiber)))
	(setf *ready-fibers* (cdr *ready-fibers*))
	(setf *current-fiber* fiber)
	(funcall cc))
      (throw *halt* nil)))

(defun terminate-fiber (fiber)
  "Terminate the fiber identified by FIBER."
  (handle-fiber-exit fiber)
  (fiber-sched-step))

(defun/cc make-fiber (name function &rest args)
  "Create a new fiber of NAME that runs FUNCTION with ARGS. When the function
returns the fiber exits. The return values of FUNCTION are kept
around and can be retrieved by JOIN-FIBER."
  (let* ((fiber (%make-fiber :name name)))
    (setf (fiber-cc fiber) #'(lambda ()
			       (setf (fiber-result fiber)
				     (cons t
					   (multiple-value-list
					    (apply function args))))
			       (terminate-fiber fiber)
			       (values))
	  (fiber-%alive-p fiber) t)
    (push fiber *all-fibers*)
    (setf *ready-fibers*
	  (append *ready-fibers* (list fiber)))
    fiber))

(defun/cc fiber-yield ()
  "Yield the processor to other fibers."
  (when *ready-fibers*
    (call/cc
     (lambda (k)
       (let ((fiber *current-fiber*))
	 (when fiber
	   (setf (fiber-cc fiber) k)
	   (setf *ready-fibers*
		 (append *ready-fibers* (list fiber)))
	   (throw *yield* nil)))))))

(defmacro fiber-wait ((param test) &body body)
  "Yield the processor to other fibers while waiting on TEST. When TEST is
satisfied, evaluate BODY in a context where PARAM is bound to the result of
TEST"
  `(loop (let ((,param ,test))
	   (if ,param
	       (return
		 (progn ,@body))
	       (fiber-yield)))))

(defun/cc join-fiber (fiber &key (default nil defaultp))
  "Suspend current fiber until FIBER exits. Returns the result
values of the fiber function. If the fiber does not exit normally,
return DEFAULT if given or else signal JOIN-FIBER-ERROR."
  (fiber-wait (terminated (not (fiber-alive-p fiber)))
	(cond ((car (fiber-result fiber))
	       (return-from join-fiber
		 (values-list (cdr (fiber-result fiber)))))
	      (defaultp
	       (return-from join-fiber default)))
	(error 'join-fiber-error :fiber fiber)))

(defun run-fiber-scheduler ()
  "Run the fiber scheduler. A call to this starts the cooperative multitasking
machine."
  (catch *halt*
    (loop (catch *yield*
	    (fiber-sched-step))))
  (setf *current-fiber* nil))
