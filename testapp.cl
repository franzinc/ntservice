;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the Franz
;; preamble to the LGPL found in
;; http://opensource.franz.com/preamble.html.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License can be
;; found at http://opensource.franz.com/license.html.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple
;; Place, Suite 330, Boston, MA  02111-1307  USA
;;
;; $Id: testapp.cl,v 1.2 2002/09/19 20:21:32 dancy Exp $

(in-package :user)

;; To use:

;; 1) Customize these per your needs.
(defparameter *servicename* "MyService")
(defparameter *displayname* "My Common Lisp Service")
(defparameter *commandline* 
    "c:\\devel\\nfs\\ntservice\\testapp\\testapp.exe arg1 arg2 arg3")

;; 2) Save 
;; 3) Load this file into Allegro CL and evaluate (build)
;; 4) Start up a cmd.exe and run 
;;      c:\devel\ntservice\testapp\testapp.exe /install
;;    to install the service.  [Adjust the actual pathname per your needs]
;; 5) Start the Services control panel applet and verify that the service
;;    is listed.
;; 6) Start the service and verify that it works.
;; 7) Stop the service.
;; 8) Start up a cmd.exe and run
;;      c:\devel\ntservice\testapp\testapp.exe /remove
;;    to remove the service.  [Adjust the actual pathname per your needs]


(eval-when (load compile eval)
  (load "ntservice.fasl"))

(defun main (&rest args)
  (format t "args are ~S~%" args)
  (if (member "/install" args :test #'equalp)
      (progn
	(add-service)
	(return-from main)))
  (if (member "/remove" args :test #'equalp)
      (progn
	(remove-service)
	(return-from main)))
  (ntservice:start-service 'real-main :init 'init)
  t)

(defun init (args) 
  (format t "Start parameters: ~S~%" args)
  t)

(defun real-main ()
  (loop
    (format t "sleeping...~%")
    (sleep 5)))

(defun build ()
  (compile-file-if-needed "testapp.cl")
  (generate-executable "testapp" '("testapp.fasl" "ntservice.fasl")))

(defun add-service ()
  (format t "Installing service...~%")
  (ntservice:create-service 
   *servicename*
   *displayname*
   *commandline*)
  (format t "Installation successful.~%"))

(defun remove-service ()
  (format t "Removing service...~%")
  (ntservice:delete-service *servicename*)
  (format t "Removal successful.~%"))
