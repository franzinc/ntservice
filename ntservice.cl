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
;; $Id: ntservice.cl,v 1.8 2003/01/20 22:41:35 dancy Exp $

(defpackage :ntservice 
  (:use :excl :ff :common-lisp)
  (:export #:start-service
	   #:create-service
	   #:delete-service
	   #:winstrerror))

(in-package :ntservice)

;; foreign types

(eval-when (compile load eval)
  (require :foreign)

(def-foreign-type SERVICE_TABLE_ENTRY 
    (:struct
     (lpServiceName :int)
     (lpServiceProc :int)))

(def-foreign-type SERVICE_STATUS
    (:struct
     (dwServiceType :int)
     (dwCurrentState :int)
     (dwControlsAccepted :int)
     (dwWin32ExitCode :int)
     (dwServiceSpecificExitCode :int)
     (dwCheckPoint :int)
     (dwWaitHint :int)))

(def-foreign-type ENUM_SERVICE_STATUS
    (:struct
     (lpServiceName (* :string))
     (lpDisplayName (* :string))
     (ServiceStatus SERVICE_STATUS)))
)

;; foreign calls

(def-foreign-call (StartServiceCtrlDispatcher "StartServiceCtrlDispatcherA")
    ()
  :strings-convert t
  :returning :int
  :release-heap :always)

(def-foreign-call (RegisterServiceCtrlHandler "RegisterServiceCtrlHandlerA") ()
    :strings-convert t
    :returning :int)

(def-foreign-call (SetServiceStatus "SetServiceStatus") () 
  :returning :int :strings-convert t)

(def-foreign-call (GetLastError "GetLastError") () 
  :returning :int :strings-convert t)

(def-foreign-call (DebugBreak "DebugBreak") () :strings-convert t)

(def-foreign-call (OutputDebugString "OutputDebugStringA") () 
  :strings-convert t)

(def-foreign-call (OpenSCManager "OpenSCManagerA") () 
  :strings-convert t
  :returning :int)

(def-foreign-call (CloseServiceHandle "CloseServiceHandle") ((hSCObject :int)) 
  :strings-convert t
  :returning :int)

(def-foreign-call (OpenService "OpenServiceA") () 
  :strings-convert t
  :error-value :os-specific
  :returning :int)

(def-foreign-call (EnumServicesStatus "EnumServicesStatusA") ((hSCManager :int) (dwServiceType :int) (dwServiceState :int) (lpServices (* ENUM_SERVICE_STATUS)) (cbBufSize :int) (pcbBytesNeeded (* :int)) (lpServicesReturned (* :int)) (lpResumeHandle (* :int)))
  :strings-convert t
  :returning :int)

(def-foreign-call (CreateService "CreateServiceA") () 
  :returning :int 
  :error-value :os-specific
  :strings-convert t)

(def-foreign-call (StartService "StartServiceA") ()
  :returning :int
  :strings-convert t)

(def-foreign-call (DeleteService "DeleteService") () 
  :returning :int 
  :error-value :os-specific
  :strings-convert t)

(def-foreign-call (FormatMessage "FormatMessageA") ()
  :returning :int :strings-convert nil)

(def-foreign-call LocalFree () :returning :int :strings-convert nil)

#+(version>= 6 2 :pre-beta 13)
(def-foreign-call (start_tray_icon_watcher "start_tray_icon_watcher") ()
  :returning :int
  :strings-convert nil)

;;; constants

(defparameter STANDARD_RIGHTS_REQUIRED #x000F0000)
(defparameter SC_MANAGER_CONNECT             #x0001)
(defparameter SC_MANAGER_CREATE_SERVICE      #x0002)
(defparameter SC_MANAGER_ENUMERATE_SERVICE   #x0004)
(defparameter SC_MANAGER_LOCK                #x0008)
(defparameter SC_MANAGER_QUERY_LOCK_STATUS   #x0010)
(defparameter SC_MANAGER_MODIFY_BOOT_CONFIG  #x0020)

(defparameter SC_MANAGER_ALL_ACCESS          
    (logior STANDARD_RIGHTS_REQUIRED  
	    SC_MANAGER_CONNECT     
	    SC_MANAGER_CREATE_SERVICE    
	    SC_MANAGER_ENUMERATE_SERVICE 
	    SC_MANAGER_LOCK              
	    SC_MANAGER_QUERY_LOCK_STATUS 
	    SC_MANAGER_MODIFY_BOOT_CONFIG))


(defparameter SERVICE_WIN32_OWN_PROCESS      #x00000010)
(defparameter SERVICE_WIN32_SHARE_PROCESS    #x00000020)
(defparameter SERVICE_WIN32
    (logior SERVICE_WIN32_OWN_PROCESS SERVICE_WIN32_SHARE_PROCESS))
(defparameter SERVICE_INTERACTIVE_PROCESS    #x00000100)

(defparameter SERVICE_ACTIVE                 #x00000001)
(defparameter SERVICE_INACTIVE               #x00000002)
(defparameter SERVICE_STATE_ALL         
    (logior SERVICE_ACTIVE SERVICE_INACTIVE))

(defparameter SERVICE_BOOT_START             #x00000000)
(defparameter SERVICE_SYSTEM_START           #x00000001)
(defparameter SERVICE_AUTO_START             #x00000002)
(defparameter SERVICE_DEMAND_START           #x00000003)
(defparameter SERVICE_DISABLED               #x00000004)

(defparameter SERVICE_ERROR_IGNORE           #x00000000)
(defparameter SERVICE_ERROR_NORMAL           #x00000001)
(defparameter SERVICE_ERROR_SEVERE           #x00000002)
(defparameter SERVICE_ERROR_CRITICAL         #x00000003)


;;
;; Controls
;;
(eval-when (compile load eval)
(defparameter SERVICE_CONTROL_STOP           #x00000001)
(defparameter SERVICE_CONTROL_PAUSE          #x00000002)
(defparameter SERVICE_CONTROL_CONTINUE       #x00000003)
(defparameter SERVICE_CONTROL_INTERROGATE    #x00000004)
(defparameter SERVICE_CONTROL_SHUTDOWN       #x00000005)
(defparameter SERVICE_CONTROL_PARAMCHANGE    #x00000006)
(defparameter SERVICE_CONTROL_NETBINDADD     #x00000007)
(defparameter SERVICE_CONTROL_NETBINDREMOVE  #x00000008)
(defparameter SERVICE_CONTROL_NETBINDENABLE  #x00000009)
(defparameter SERVICE_CONTROL_NETBINDDISABLE #x0000000A)
)

;;
;; Service State -- for CurrentState
;;
(defparameter SERVICE_STOPPED                #x00000001)
(defparameter SERVICE_START_PENDING          #x00000002)
(defparameter SERVICE_STOP_PENDING           #x00000003)
(defparameter SERVICE_RUNNING                #x00000004)
(defparameter SERVICE_CONTINUE_PENDING       #x00000005)
(defparameter SERVICE_PAUSE_PENDING          #x00000006)
(defparameter SERVICE_PAUSED                 #x00000007)

;;
;; Controls Accepted  (Bit Mask)
;;
(defparameter SERVICE_ACCEPT_STOP            #x00000001)
(defparameter SERVICE_ACCEPT_PAUSE_CONTINUE  #x00000002)
(defparameter SERVICE_ACCEPT_SHUTDOWN        #x00000004)
(defparameter SERVICE_ACCEPT_PARAMCHANGE     #x00000008)
(defparameter SERVICE_ACCEPT_NETBINDCHANGE   #x00000010)

;;; error codes

(defparameter NO_ERROR 0)
(defparameter ERROR_MORE_DATA 234)
(defparameter ERROR_SERVICE_SPECIFIC_ERROR 1066)

;; FormatMessage stuff
(defparameter FORMAT_MESSAGE_ALLOCATE_BUFFER #x00000100)
(defparameter FORMAT_MESSAGE_IGNORE_INSERTS  #x00000200)
(defparameter FORMAT_MESSAGE_FROM_STRING     #x00000400)
(defparameter FORMAT_MESSAGE_FROM_HMODULE    #x00000800)
(defparameter FORMAT_MESSAGE_FROM_SYSTEM     #x00001000)
(defparameter FORMAT_MESSAGE_ARGUMENT_ARRAY  #x00002000)
(defparameter FORMAT_MESSAGE_MAX_WIDTH_MASK  #x000000FF)

;; globals

(defparameter service-status (allocate-fobject 'SERVICE_STATUS :c))
(defparameter service-status-handle nil)

(defparameter service-init-func nil)
(defparameter service-main-func nil)
(defparameter service-stop-func nil)


;; macros
(defmacro ss-slot (slot) 
  `(fslot-value-typed 'SERVICE_STATUS :c service-status ,slot))


;; code

(defun-foreign-callable ServiceMain (argc argv)
  (declare (:convention :stdcall))
  ;;(in-package :ntservice)
  (let ((argv-type `(:array (* :string) ,argc))
	(service-control-handler-addr (register-foreign-callable 'service-control-handler))
	err
	args)
    (dotimes (i argc)
      (push (native-to-string (fslot-value-typed argv-type :c argv i)) args))
    (setf args (rest (reverse args))) ;; drop the service name from the list
    
    (without-interrupts 
     (setf service-status-handle (RegisterServiceCtrlHandler "Unused" service-control-handler-addr))
     (setf err (GetLastError)))
    (if* (= service-status-handle 0)
       then
	    (debug-msg (format nil "RegisterServiceCtrlHandler failed w/ error code ~D" err))
	    (return-from ServiceMain))

    (setf (ss-slot 'dwServiceType) (logior SERVICE_WIN32_OWN_PROCESS SERVICE_INTERACTIVE_PROCESS))
    (setf (ss-slot 'dwControlsAccepted) SERVICE_ACCEPT_STOP)
    (setf (ss-slot 'dwWin32ExitCode) NO_ERROR)
    (setf (ss-slot 'dwCheckPoint) 0)
    (setf (ss-slot 'dwWaitHint) 0)
    
    (if* service-init-func
       then
	    (setf (ss-slot 'dwCurrentState) SERVICE_START_PENDING)
	    (set-service-status)
	    
	    (if* (null (funcall service-init-func args))
	       then
		    (setf (ss-slot 'dwWin32ExitCode) ERROR_SERVICE_SPECIFIC_ERROR)
		    (setf (ss-slot 'dwServiceSpecificExitCode) 1)
		    (set-service-status)
		    (return-from ServiceMain)))
    
    (setf (ss-slot 'dwCurrentState) SERVICE_RUNNING)
    (set-service-status)
    
    (funcall service-main-func)

    (setf (ss-slot 'dwCurrentState) SERVICE_STOPPED)
    (set-service-status)
    
    ))

(defun set-service-status ()
  (let (res err)
    (without-interrupts
      (setf res (SetServiceStatus service-status-handle service-status))
      (setf err (GetLastError)))
    (if* (= 0 res)
       then
	    (debug-msg (format nil "SetServiceStatus failed w/ error code ~D" err))
	    (big-exit))))
  
(defun big-exit ()
  (exit 0 :no-unwind t :quiet t))
  
(defun-foreign-callable service-control-handler (fdwControl)
  (declare (:convention :stdcall))
  (debug-msg (format nil "service-control-handler got control code ~D~%" fdwControl))
  (case fdwControl
    (#.SERVICE_CONTROL_STOP
     (if* service-stop-func
	then
	     (setf (ss-slot 'dwCurrentState) SERVICE_STOP_PENDING)
	     (set-service-status)
	     
	     (funcall service-stop-func))

     (setf (ss-slot 'dwCurrentState) SERVICE_STOPPED)
     (set-service-status))
    (t
     (debug-msg "That control code is not handled.
"))))

(defun start-service (main &key init stop)

  (setf service-main-func main)
  (setf service-init-func init)
  (setf service-stop-func stop)
  
  (let* ((ServiceMainAddr (register-foreign-callable 'ServiceMain))
	 (service-name (string-to-native "Unused"))
	 (service-table-type '(:array SERVICE_TABLE_ENTRY 2))
	 (service-table (allocate-fobject service-table-type :c))
	 err)
    (macrolet ((st-slot (index slot) `(fslot-value-typed service-table-type :c service-table ,index ,slot)))

      (mp:start-customs) ;; rfr recommendation.

      #+(version>= 6 2 :pre-beta 13)
      (start_tray_icon_watcher) ; ensure that tray icon visible after login
      
      (setf (st-slot 0 'lpServiceName) service-name)  ;; unused
      (setf (st-slot 0 'lpServiceProc) ServiceMainAddr)
      ;; the null terminating entry
      (setf (st-slot 1 'lpServiceName) 0)
      (setf (st-slot 1 'lpServiceProc) 0)

      (if* (= 0 (StartServiceCtrlDispatcher service-table))
	 then
	      (setf err (GetLastError))
	      (debug-msg (format nil "StartServiceCtrlDispatcher got error code ~D~%" err)))
      
      ;; some cleanup
      (aclfree service-name)
      (free-fobject service-table)
      
      ;; The following is necessary to avoid a complaint about not being
      ;; able to kill a foreign thread.
      (big-exit))))

;;;;;;;;;;

(defun debug-msg (msg)
  (OutputDebugString msg))


;;;;;;;;  Other stuff.  Not used for normal service operation.  Some of it
;;;;;;;;  is just testing.   Some of it is code to install a service.

(defun open-sc-manager (machine database desired-access)
  (if (null machine)
      (setf machine 0))
  (if (null database)
      (setf database 0))
  (without-interrupts
    (let ((res (OpenSCManager machine database desired-access)))
      (if (= res 0)
	  (error "OpenSCManager error ~D" (GetLastError))
	res))))

(defun close-sc-manager (handle)
  (CloseServiceHandle handle))

(defmacro with-sc-manager ((handle machine database desired-access) &body body)
  `(let ((,handle (open-sc-manager ,machine ,database ,desired-access)))
     (unwind-protect 
	 (progn ,@body)
       (close-sc-manager ,handle))))

(defun open-service (smhandle name desired-access)
  (multiple-value-bind (shandle err)
      (OpenService smhandle name desired-access)
    (values (if (= 0 shandle) nil shandle) err)))

(defmacro with-open-service ((handle errvar smhandle name desired-access) 
			     &body body)
  `(multiple-value-bind (,handle ,errvar)
       (open-service ,smhandle ,name ,desired-access)
     (unwind-protect
	 (progn ,@body)
       (if ,handle (CloseServiceHandle ,handle)))))

;;; just a test function.
(defun enum-services ()
  (with-sc-manager (schandle nil nil SC_MANAGER_ALL_ACCESS)
    (let ((bytes-needed (allocate-fobject :int :c))
	  (services-returned (allocate-fobject :int :c))
	  (resume-handle (allocate-fobject :int :c))
	  (buf 0)
	  (bufsize 0)
	  (errcode ERROR_MORE_DATA)
	  res)
      (while (= errcode ERROR_MORE_DATA)
	     (setf (fslot-value-typed :int :c resume-handle) 0)
	     (without-interrupts
	       (setf res (EnumServicesStatus schandle SERVICE_WIN32 SERVICE_STATE_ALL buf bufsize bytes-needed services-returned resume-handle))
	       (setf errcode (GetLastError)))
	     (if (= 0 res)
		 (progn
		   (if (not (= errcode ERROR_MORE_DATA))
		       (error "EnumServicesStatus error code ~D" errcode))
		   (setf bufsize (fslot-value-typed :int :c bytes-needed))
		   (setf buf (aclmalloc bufsize)))
	       (setf errcode 0)))
      
      (let ((count (fslot-value-typed :int :c services-returned)))
	(dotimes (i count)
	  (format t "~A -> ~A~%" 
		  (native-to-string (fslot-value-typed `(:array ENUM_SERVICE_STATUS ,count) :c buf i 'lpServiceName)) 
		  (native-to-string (fslot-value-typed `(:array ENUM_SERVICE_STATUS ,count) :c buf i 'lpDisplayName)))))
      
      (aclfree buf)
      (free-fobject bytes-needed)
      (free-fobject services-returned)
      (free-fobject resume-handle))))

(defun create-service (name displaystring cmdline)
  (with-sc-manager (schandle nil nil SC_MANAGER_ALL_ACCESS)
    (multiple-value-bind (res err)
	(CreateService 
	 schandle 
	 name
	 displaystring
	 STANDARD_RIGHTS_REQUIRED 
	 (logior SERVICE_WIN32_OWN_PROCESS SERVICE_INTERACTIVE_PROCESS) 
	 SERVICE_DEMAND_START
	 SERVICE_ERROR_NORMAL
	 cmdline
	 0 ;; no load order group
	 0 ;; no tag identifier
	 0 ;; no dependencies 
	 0 ;; use LocalSystem account
	 0) ;; no password
      (if (/= res 0)
	  (CloseServiceHandle res))
      (if (= res 0)
	  (values nil err)
	(values t res)))))


(defun delete-service (name)
  (with-sc-manager (sc nil nil SC_MANAGER_ALL_ACCESS)
    (with-open-service (handle err sc name STANDARD_RIGHTS_REQUIRED)
      (if* handle
	 then
	      (multiple-value-bind (res err)
		  (DeleteService handle)
		(if (= res 0)
		    (values nil err "DeleteService")
		  (values t res)))
	 else
	      (values nil err "OpenService")))))

;;; Error message stuff

(defun winstrerror (code)
  (let ((stringptr (ff:allocate-fobject '(* :char) :foreign-static-gc))
	res)
    (FormatMessage
     (logior FORMAT_MESSAGE_FROM_SYSTEM 
	     FORMAT_MESSAGE_IGNORE_INSERTS
	     FORMAT_MESSAGE_ALLOCATE_BUFFER) ;; dwFlags
     0 ;; lpSource
     code ;; dwMessageId
     0 ;; dwLanguageId
     stringptr ;; lpBuffer
     0  ;; nSize
     0) ;; Arguments
    (setf res (native-to-string (ff:fslot-value stringptr)))
    (LocalFree (ff:fslot-value stringptr))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


