;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
;; Copyright (C) 2002-2006 Franz Inc, Oakland, CA.  All rights reserved.
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
;; $Id: testapp.cl,v 1.7 2006/06/14 15:28:09 layer Exp $

(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To use:

;; 1) Customize these per your needs.
(defparameter *servicename* "MyService")
(defparameter *displayname* "My Common Lisp Service")
(defparameter *commandline* 
    "arg1 arg2 arg3") ;; the executable path will be prepended.

;; 2) Save 
;; 3) Load this file into Allegro CL and evaluate (build)
;; 4) Start up a cmd.exe and run 
;;      ...\testapp\testapp.exe /install
;;    to install the service.  [Adjust the actual ... per your needs]
;; 5) Start the Services control panel applet and verify that the service
;;    is listed
;; 6) Start the service and verify that it works.  You can start it my
;;    doing this from a cmd.exe prompt:
;;      net start MyService
;; 7) Stop the service:
;;      net stop MyService
;; 8) From a cmd.exe:
;;      ...\testapp\testapp.exe /remove
;;    to remove the service.  [Adjust the actual pathname per your needs]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)
  (require :ntservice
	   ;; makes for easier testing:
	   (probe-file "./ntservice.fasl")))

(defvar *log-stream* nil)

(defun main (&rest args)
  (setq ntservice::*debug* t)
  (setq *log-stream* t)
  (logit "args are ~S" args)
  (let ((exepath
	 ;; ignore executable image argument:
	 (pop args)))

    (when (member "/install" args :test #'equalp)
      (add-service exepath)
      (return-from main))

    (when (member "/remove" args :test #'equalp)
      (remove-service)
      (return-from main))

    (ntservice:execute-service *servicename* #'service-main
			       :init #'service-init
			       :stop #'service-stop)
    
    (logit "reached end of app main")
    t))

(defvar *shutting-down* nil)

(defun service-main ()
  (logit "main: starting")
  ;; Note that the application is not shutting down:
  (setq *shutting-down* (mp:make-gate nil))
  (let ((app
	 (mp:process-run-function "testapp"
	   (lambda ()
	     (logit "main: running testapp")
	     ;; The real work of the application goes here
	     (loop (sleep 1))))))
    (mp:process-wait "waiting for shutdown" #'mp:gate-open-p *shutting-down*)
    (mp:process-kill app)
    (logit "main: returning")))

(defun service-init (args)
  (tpl:do-command "proc")
  (logit "init: start parameters: ~S" args)
  ;; Must return `t' to signify the service started OK;
  t)

(defun service-stop ()
  (logit "stop: closing down service.")
  (mp:open-gate *shutting-down*))

(defun build ()
  (compile-file-if-needed "testapp.cl")
  (generate-executable
   "testapp"
   '("testapp.fasl" :ntservice #+(version>= 7) :proc2common)
   :allow-existing-directory t))

(defun add-service (exepath)
  (logit "Installing service")
  (multiple-value-bind (success errcode)
      (ntservice:create-service 
       *servicename*
       *displayname*
       (concatenate 'string exepath " " *commandline*))
    (if success
	(logit "Installation successful.")
      (error "ntservice:create-service error: ~A"
	     (ntservice:winstrerror errcode)))))

(defun remove-service ()
  (logit "Removing service")
  (multiple-value-bind (success errcode errfunc)
      (ntservice:delete-service *servicename*)
    (if success
	(logit "Removal successful.")
      (error "ntservice:delete-service error in ~A: ~A"
	     errfunc (ntservice:winstrerror errcode)))))

(defun logit (format-string &rest format-args)
  (when *log-stream*
    (format *log-stream* "~&[~x] ~?~&"
	    (mp::process-os-id mp:*current-process*)
	    format-string
	    format-args)
    (force-output *log-stream*)))
