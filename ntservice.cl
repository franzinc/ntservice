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
;; $Id: ntservice.cl,v 1.12 2004/02/13 17:53:22 dancy Exp $

(defpackage :ntservice 
  (:use :excl :ff :common-lisp)
  (:export #:start-service
	   #:stop-service
	   #:execute-service ;; used to be start-service
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
  :error-value :os-specific
  :returning :int
  :release-heap :always)

(def-foreign-call (RegisterServiceCtrlHandler "RegisterServiceCtrlHandlerA") ()
  :strings-convert t
  :error-value :os-specific
  :returning :int)

(def-foreign-call (SetServiceStatus "SetServiceStatus") () 
  :returning :int 
  :error-value :os-specific
  :strings-convert t)

(def-foreign-call (GetLastError "GetLastError") () 
  :returning :int :strings-convert t)

(def-foreign-call (DebugBreak "DebugBreak") () :strings-convert t)

(def-foreign-call (OutputDebugString "OutputDebugStringA") () 
  :strings-convert t)

(def-foreign-call (OpenSCManager "OpenSCManagerA") () 
  :strings-convert t
  :error-value :os-specific
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
  :error-value :os-specific
  :returning :int)

(def-foreign-call (CreateService "CreateServiceA") () 
  :returning :int 
  :error-value :os-specific
  :strings-convert t)

(def-foreign-call (StartService "StartServiceA") ()
  :returning :int
  :error-value :os-specific
  :strings-convert t)

(def-foreign-call (DeleteService "DeleteService") () 
  :returning :int 
  :error-value :os-specific
  :strings-convert t)

(def-foreign-call (QueryServiceStatus "QueryServiceStatus") ()
  :returning :int
  :error-value :os-specific
  :strings-convert t)

(def-foreign-call (ControlService "ControlService") ()
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

(defconstant STANDARD_RIGHTS_REQUIRED #x000F0000)

(defconstant SC_MANAGER_CONNECT             #x0001)
(defconstant SC_MANAGER_CREATE_SERVICE      #x0002)
(defconstant SC_MANAGER_ENUMERATE_SERVICE   #x0004)
(defconstant SC_MANAGER_LOCK                #x0008)
(defconstant SC_MANAGER_QUERY_LOCK_STATUS   #x0010)
(defconstant SC_MANAGER_MODIFY_BOOT_CONFIG  #x0020)

(defconstant SC_MANAGER_ALL_ACCESS          
    (logior STANDARD_RIGHTS_REQUIRED  
	    SC_MANAGER_CONNECT     
	    SC_MANAGER_CREATE_SERVICE    
	    SC_MANAGER_ENUMERATE_SERVICE 
	    SC_MANAGER_LOCK              
	    SC_MANAGER_QUERY_LOCK_STATUS 
	    SC_MANAGER_MODIFY_BOOT_CONFIG))


(defconstant SERVICE_QUERY_CONFIG           #x0001)
(defconstant SERVICE_CHANGE_CONFIG          #x0002)
(defconstant SERVICE_QUERY_STATUS           #x0004)
(defconstant SERVICE_ENUMERATE_DEPENDENTS   #x0008)
(defconstant SERVICE_START                  #x0010)
(defconstant SERVICE_STOP                   #x0020)
(defconstant SERVICE_PAUSE_CONTINUE         #x0040)
(defconstant SERVICE_INTERROGATE            #x0080)
(defconstant SERVICE_USER_DEFINED_CONTROL   #x0100)

(defconstant SERVICE_ALL_ACCESS 
    (logior
     STANDARD_RIGHTS_REQUIRED   
     SERVICE_QUERY_CONFIG         
     SERVICE_CHANGE_CONFIG        
     SERVICE_QUERY_STATUS         
     SERVICE_ENUMERATE_DEPENDENTS 
     SERVICE_START                
     SERVICE_STOP                 
     SERVICE_PAUSE_CONTINUE       
     SERVICE_INTERROGATE          
     SERVICE_USER_DEFINED_CONTROL))




(defconstant SERVICE_WIN32_OWN_PROCESS      #x00000010)
(defconstant SERVICE_WIN32_SHARE_PROCESS    #x00000020)
(defconstant SERVICE_WIN32
    (logior SERVICE_WIN32_OWN_PROCESS SERVICE_WIN32_SHARE_PROCESS))
(defconstant SERVICE_INTERACTIVE_PROCESS    #x00000100)

(defconstant SERVICE_ACTIVE                 #x00000001)
(defconstant SERVICE_INACTIVE               #x00000002)
(defconstant SERVICE_STATE_ALL         
    (logior SERVICE_ACTIVE SERVICE_INACTIVE))

(defconstant SERVICE_BOOT_START             #x00000000)
(defconstant SERVICE_SYSTEM_START           #x00000001)
(defconstant SERVICE_AUTO_START             #x00000002)
(defconstant SERVICE_DEMAND_START           #x00000003)
(defconstant SERVICE_DISABLED               #x00000004)

(defconstant SERVICE_ERROR_IGNORE           #x00000000)
(defconstant SERVICE_ERROR_NORMAL           #x00000001)
(defconstant SERVICE_ERROR_SEVERE           #x00000002)
(defconstant SERVICE_ERROR_CRITICAL         #x00000003)


;;
;; Controls
;;
(eval-when (compile load eval)
(defconstant SERVICE_CONTROL_STOP           #x00000001)
(defconstant SERVICE_CONTROL_PAUSE          #x00000002)
(defconstant SERVICE_CONTROL_CONTINUE       #x00000003)
(defconstant SERVICE_CONTROL_INTERROGATE    #x00000004)
(defconstant SERVICE_CONTROL_SHUTDOWN       #x00000005)
(defconstant SERVICE_CONTROL_PARAMCHANGE    #x00000006)
(defconstant SERVICE_CONTROL_NETBINDADD     #x00000007)
(defconstant SERVICE_CONTROL_NETBINDREMOVE  #x00000008)
(defconstant SERVICE_CONTROL_NETBINDENABLE  #x00000009)
(defconstant SERVICE_CONTROL_NETBINDDISABLE #x0000000A)
)

;;
;; Service State -- for CurrentState
;;
(defconstant SERVICE_STOPPED                #x00000001)
(defconstant SERVICE_START_PENDING          #x00000002)
(defconstant SERVICE_STOP_PENDING           #x00000003)
(defconstant SERVICE_RUNNING                #x00000004)
(defconstant SERVICE_CONTINUE_PENDING       #x00000005)
(defconstant SERVICE_PAUSE_PENDING          #x00000006)
(defconstant SERVICE_PAUSED                 #x00000007)

;;
;; Controls Accepted  (Bit Mask)
;;
(defconstant SERVICE_ACCEPT_STOP            #x00000001)
(defconstant SERVICE_ACCEPT_PAUSE_CONTINUE  #x00000002)
(defconstant SERVICE_ACCEPT_SHUTDOWN        #x00000004)
(defconstant SERVICE_ACCEPT_PARAMCHANGE     #x00000008)
(defconstant SERVICE_ACCEPT_NETBINDCHANGE   #x00000010)

;;; error codes

(defconstant NO_ERROR 0)
(defconstant ERROR_MORE_DATA 234)
(defconstant ERROR_SERVICE_SPECIFIC_ERROR 1066)

;; FormatMessage stuff
(defconstant FORMAT_MESSAGE_ALLOCATE_BUFFER #x00000100)
(defconstant FORMAT_MESSAGE_IGNORE_INSERTS  #x00000200)
(defconstant FORMAT_MESSAGE_FROM_STRING     #x00000400)
(defconstant FORMAT_MESSAGE_FROM_HMODULE    #x00000800)
(defconstant FORMAT_MESSAGE_FROM_SYSTEM     #x00001000)
(defconstant FORMAT_MESSAGE_ARGUMENT_ARRAY  #x00002000)
(defconstant FORMAT_MESSAGE_MAX_WIDTH_MASK  #x000000FF)

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

    (multiple-value-setq (service-status-handle err)
      (RegisterServiceCtrlHandler "Unused" service-control-handler-addr))
    (when (zerop service-status-handle)
      (debug-msg (format nil "RegisterServiceCtrlHandler failed~A"
			 (winstrerror err)))
      (return-from ServiceMain))

    (setf (ss-slot 'dwServiceType) 
      (logior SERVICE_WIN32_OWN_PROCESS SERVICE_INTERACTIVE_PROCESS))
    (setf (ss-slot 'dwControlsAccepted) SERVICE_ACCEPT_STOP)
    (setf (ss-slot 'dwWin32ExitCode) NO_ERROR)
    (setf (ss-slot 'dwCheckPoint) 0)
    (setf (ss-slot 'dwWaitHint) 10000) ;; 10 seconds
    
    (when service-init-func
      (when (null (funcall service-init-func args))
	(setf (ss-slot 'dwWin32ExitCode) ERROR_SERVICE_SPECIFIC_ERROR)
	(setf (ss-slot 'dwServiceSpecificExitCode) 1)
	(set-service-status)
	(return-from ServiceMain)))
    
    (setf (ss-slot 'dwCurrentState) SERVICE_RUNNING)
    (setf (ss-slot 'dwCheckPoint) 0)
    (setf (ss-slot 'dwWaitHint) 0)
    (set-service-status)
    
    (funcall service-main-func)

    (setf (ss-slot 'dwCurrentState) SERVICE_STOPPED)
    (set-service-status)))

(defun set-service-status ()
  (multiple-value-bind (res err)
      (SetServiceStatus service-status-handle service-status)
    (when (zerop res)
      (debug-msg (format nil "SetServiceStatus failed: ~A" 
			 (winstrerror err)))
      (big-exit))))
  
(defun big-exit ()
  (exit 0 :no-unwind t :quiet t))
  
(defun-foreign-callable service-control-handler (fdwControl)
  (declare (:convention :stdcall))
  (debug-msg 
   (format nil "service-control-handler got control code ~D~%" fdwControl))
  (case fdwControl
    (#.SERVICE_CONTROL_STOP
     (when service-stop-func
       (setf (ss-slot 'dwCurrentState) SERVICE_STOP_PENDING)
       (set-service-status)
	     
       (funcall service-stop-func))

     (setf (ss-slot 'dwCurrentState) SERVICE_STOPPED)
     (set-service-status))
    (t
     (debug-msg "That control code is not handled.
"))))

(defun execute-service (main &key init stop)
  
  (setf service-main-func main)
  (setf service-init-func init)
  (setf service-stop-func stop)
  
  (let* ((ServiceMainAddr (register-foreign-callable 'ServiceMain))
	 (service-name (string-to-native "Unused"))
	 (service-table-type '(:array SERVICE_TABLE_ENTRY 2))
	 (service-table (allocate-fobject service-table-type :c)))
    (macrolet ((st-slot (index slot) `(fslot-value-typed service-table-type :c service-table ,index ,slot)))

      (mp:start-scheduler)
      #-(version>= 7)(mp:start-customs) ;; rfr recommendation.

      (start_tray_icon_watcher) ; ensure that tray icon is visible after login

      (setf (st-slot 0 'lpServiceName) service-name)  ;; unused
      (setf (st-slot 0 'lpServiceProc) ServiceMainAddr)
      ;; the null terminating entry
      (setf (st-slot 1 'lpServiceName) 0)
      (setf (st-slot 1 'lpServiceProc) 0)

      (multiple-value-bind (res err)
	  (StartServiceCtrlDispatcher service-table)
	(when (zerop res)
	  (debug-msg 
	   (format nil "StartServiceCtrlDispatcher failed:  ~D~%" 
		   (winstrerror err)))))
      
      ;; some cleanup
      (aclfree service-name)
      (free-fobject service-table)
      
      ;; The following is necessary to avoid a complaint about not being
      ;; able to kill a foreign thread.
      (big-exit))))

;;;;;;;;;;

(defun debug-msg (msg)
  (OutputDebugString msg))


(defun open-sc-manager (machine database desired-access)
  (if (null machine)
      (setf machine 0))
  (if (null database)
      (setf database 0))
  (multiple-value-bind (res err)
      (OpenSCManager machine database desired-access)
    (if (zerop res)
	(error "OpenSCManager failed: ~A" (winstrerror err))
      res)))
      

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
	
	(multiple-value-setq (res errcode)
	  (EnumServicesStatus schandle SERVICE_WIN32 SERVICE_STATE_ALL buf bufsize bytes-needed services-returned resume-handle))
	(if* (zerop res)
	   then
		(if (not (= errcode ERROR_MORE_DATA))
		    (error "EnumServicesStatus error: ~A"
			   (winstrerror errcode)))
		(setf bufsize (fslot-value-typed :int :c bytes-needed))
		(setf buf (aclmalloc bufsize))
	   else
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

(defun create-service (name displaystring cmdline &key (start :manual))
  (with-sc-manager (schandle nil nil SC_MANAGER_ALL_ACCESS)
    (multiple-value-bind (res err)
	(CreateService 
	 schandle 
	 name
	 displaystring
	 STANDARD_RIGHTS_REQUIRED 
	 (logior SERVICE_WIN32_OWN_PROCESS SERVICE_INTERACTIVE_PROCESS) 
	 (case start
	   (:auto SERVICE_AUTO_START)
	   (:manual SERVICE_DEMAND_START)
	   (t (error "create-service: Unrecognized start type: ~S" start)))
	 SERVICE_ERROR_NORMAL
	 cmdline
	 0 ;; no load order group
	 0 ;; no tag identifier
	 0 ;; no dependencies 
	 0 ;; use LocalSystem account
	 0) ;; no password
      (if (/= res 0)
	  (CloseServiceHandle res))
      (if (zerop res)
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

(defmacro with-service-status ((handle var) &body body)
  `(let ((,var (allocate-fobject 'SERVICE_STATUS :c)))
     (unwind-protect
	 (progn 
	   (query-service-status ,handle ,var)
	   ,@body)
       (free-fobject ,var))))


(defun query-service-status (handle ss)
  (multiple-value-bind (res err) (QueryServiceStatus handle ss)
    (if (zerop res)
	(error "QueryServiceStatus: ~A" (winstrerror err)))))

(defmacro get-service-state (ss)
  `(fslot-value-typed 'SERVICE_STATUS :c ,ss 'dwCurrentState))

(defmacro service-status-eq (ss status)
  `(= (get-service-state ,ss) ,status))

(defmacro service-running-p (ss)
  `(service-status-eq ,ss SERVICE_RUNNING))

(defmacro service-start-pending-p (ss)
  `(service-status-eq ,ss SERVICE_START_PENDING))

(defmacro service-stopped-p (ss)
  `(service-status-eq ,ss SERVICE_STOPPED))

(defmacro service-stop-pending-p (ss)
  `(service-status-eq ,ss SERVICE_STOP_PENDING))

(defmacro get-service-wait-hint-in-seconds (ss)
  `(/ (fslot-value-typed 'SERVICE_STATUS :c ,ss 'dwWaitHint) 1000.0))

(defmacro sleep-wait-hint-time (ss)
  `(sleep (get-service-wait-hint-in-seconds ,ss)))

(defmacro get-service-checkpoint (ss)
  `(fslot-value-typed 'SERVICE_STATUS :c ,ss 'dwCheckPoint))

(defun wait-for-service-to-stop (handle ss timeout)
  (let ((give-up-at (+ timeout (get-universal-time))))
    (while (not (service-stopped-p ss))
      (if (>= (get-universal-time) give-up-at)
	  (return-from wait-for-service-to-stop
	    (values nil :timeout)))
      (sleep-wait-hint-time ss)
      (query-service-status handle ss))
    t))

(defun start-service (name &key (wait t))
  (block nil
    (with-sc-manager (sc nil nil SC_MANAGER_ALL_ACCESS)
      (with-open-service (handle err sc name SERVICE_ALL_ACCESS)
	(if (null handle)
	    (return (values nil err "OpenService")))
	
	(multiple-value-bind (res err)
	    (StartService handle 0 0)
	  (if (zerop res)
	      (return (values nil err "StartService")))
	  
	  (if (not wait)
	      (return t)))
	      
	;; need to wait.
	(multiple-value-bind (success err)
	    (wait-for-service-to-start handle)
	  (if success
	      t
	    (values nil err)))))))

(defun stop-service (name &key (timeout 30))
  (block nil
    (with-sc-manager (sc nil nil SC_MANAGER_ALL_ACCESS)
      (with-open-service (handle err sc name SERVICE_ALL_ACCESS)
	(if (null handle)
	    (return (values nil err "OpenService")))
	(with-service-status (handle ss)
	  (if (service-stopped-p ss)
	      (return t))
	  (if (service-stop-pending-p ss)
	      (return (wait-for-service-to-stop handle ss timeout)))
	  
	  ;; XXXX -- need option to stop dependencies.
	  
	  (multiple-value-bind (res err)
	      (ControlService handle SERVICE_CONTROL_STOP ss)
	    (if (zerop res)
		(return (values nil err "ControlService"))))
	  
	  (wait-for-service-to-stop handle ss timeout))))))



(defun wait-for-service-to-start (handle)
  (with-service-status (handle ss)
    (let ((start-tick-count (get-universal-time))
	  (old-checkpoint (get-service-checkpoint ss)))
      (while (service-start-pending-p ss)
	(let ((wait-time  (/ (get-service-wait-hint-in-seconds ss) 10.0)))
	  (if (< wait-time 1)
	      (setf wait-time 1))
	  (if (> wait-time 10)
	      (setf wait-time 10))
	  (sleep wait-time))
	
	;; check again
	(query-service-status handle ss)
	(if (not (service-start-pending-p ss))
	    (return))

	(if* (> (get-service-checkpoint ss) old-checkpoint)
	   then 
		;; progress is being made.
		(setf old-checkpoint (get-service-checkpoint ss))
		(setf start-tick-count (get-universal-time))
	   else
		;; no progress has been made.. see if we have exceeded
		;; the wait time.
		(if (> (- (get-universal-time) start-tick-count)
		       (get-service-wait-hint-in-seconds ss))
		    (return-from wait-for-service-to-start 
		      (values nil :timeout)))))
      
      (if (service-running-p ss)
	  t
	(error "wait-for-service-to-start: Unexpected service state: ~A"
	       (get-service-state ss))))))


  

;;; Error message stuff

(defun winstrerror (code)
  (if (not (numberp code))
      (error "Argument to winsterror should be a number, not ~S" code))
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


