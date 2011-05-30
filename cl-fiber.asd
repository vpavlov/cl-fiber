;;;; -*- lisp -*-
;;;;============================================================================
;;;; Fibers API for Common Lisp
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
;;;; $Id: cl-fiber.asd,v 1.2 2011/05/30 08:03:34 vpavlov Exp $
;;;;============================================================================
(in-package :cl-user)

(defpackage :cl-fiber.system
  (:use :cl :asdf))

(in-package :cl-fiber.system)

(defsystem :cl-fiber
  :description "Fiber API for Common Lisp"
  :version "1.0"
  :author "Valentin Pavlov <vpavlov@rila.bg>"
  :license "GNU General Public License Version 3 (GPLv3)"
  :serial t
  :depends-on (:cl-cont)
  :components (
	       (:file "defpackage")
	       (:file "fiber")))

