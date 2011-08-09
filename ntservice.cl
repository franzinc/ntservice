#+(version= 8 1)
(sys:defpatch "ntservice" 2
  "v1: new: set-service-description
v2: fix error code detection in execute-service."
  :type :system
  :post-loadable t)

#+(version= 8 0)
(sys:defpatch "ntservice" 2
  "v1: major revision for clean exiting.
v2: Reduced delays during service termination."
  :type :system
  :post-loadable t)

#+(version= 7 0)
(sys:defpatch "ntservice" 2
  "v0: Initial release;
v1: major revision for clean exiting.
v2: Reduced delays during service termination."
  :type :system
  :post-loadable t)

;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
;; Copyright (C) 2001-2007 Franz Inc, Oakland, CA.  All rights reserved.
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
;; $Id: ntservice.cl,v 1.24 2008/04/10 19:04:02 layer Exp $

(defpackage :ntservice 
  (:use :excl :ff :common-lisp)
  (:export #:start-service
	   #:stop-service
	   #:execute-service ;; used to be start-service
	   #:create-service
	   #:delete-service
	   #:set-service-description
	   #:winstrerror))

(in-package :ntservice)

(provide :ntservice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See readme.txt for user-level information on the NT service module.
;;
;; ** How service startup and shutdown works **
;;
;; The function execute-service is given three functional arguments: main,
;; init, and stop.  The `main' routine does the work of the application,
;; the `init' routine initializes it, and `stop' routine shuts it down.
;;
;; execute-service does the work.  It calls StartServiceCtrlDispatcher()
;; after setting up a Lisp function to get called back when the service
;; starts and stops.  StartServiceCtrlDispatcher() returns when the service
;; has stopped.  Here's what the MSDN says about this function [my comments
;; are in brackets]:
;;
;;    When the service control manager starts a service process [the Lisp
;;    application], it waits for the process to call the
;;    StartServiceCtrlDispatcher function. The main thread of a service
;;    process should make this call as soon as possible after it starts
;;    up. If StartServiceCtrlDispatcher succeeds, it connects the calling
;;    thread to the service control manager and does not return until all
;;    running services in the process have terminated. The service control
;;    manager uses this connection to send control and service start
;;    requests to the main thread of the service process. The main thread
;;    acts as a dispatcher by invoking the appropriate HandlerEx function
;;    to handle control requests, or by creating a new thread to execute
;;    the appropriate ServiceMain function when a new service is started.
;;
;; The function that StartServiceCtrlDispatcher calls is foreign-callable
;; and is called `service-main'.  This function:
;;
;;  1. set the status of the service
;;  2. calls the `init' function
;;  3. calls the `main' function
;;  4. sets the state to `stopped' when the `main' function returns
;;
;; In June 06, the code was reworked to make sure exiting a service from
;; the tray icon (or close button, if that hasn't been disabled), would
;; work.  Here's what Bob found during the investigation as to why a double
;; exit was needed:
;;
;;    The problem here is that clicking on the close button causes a
;;    cascade of actions that eventually tries to exit by calling
;;    mp:exit-from-initial-listener.
;;
;;    mp:exit-from-initial-listener looks for an initial listener process
;;    and if it finds one, it does a process-interrupt to make -that-
;;    process call exit.
;;
;;    Unfortunately, the way ntservice is working, the initial lisp
;;    listener has made the call to execute-service, which in turn called
;;    StartServiceCtrlDispatcher, which doesn't return until the service
;;    shuts down.  We can't actually interrupt a process in a foreign call,
;;    so the interrupt is a pending request that never gets to run.
;;    So the first thing we have to do is ensure that the Initial Lisp
;;    Listener doesn't make that call. Then the existing mechanism will get
;;    exit called.
;;
;;    This isn't quite enough, though. The exit code does a process-kill
;;    and waits for the processes to go away. The process stuck in a
;;    foreign call won't go away until the foreign call returns, i.e., the
;;    service shuts down.
;;
;;    So it looks to me like ntservice needs to put something on the
;;    sys:*exit-cleanup-forms* that will shut down the service in a nice
;;    way so that the StartServiceCtrlDispatcher call returns and we can do
;;    a proper cleanup of things.
;;
;; In summary: the Initial Lisp Listener process can't call
;; StartServiceCtrlDispatcher and the service has to be shut down and the
;; callback into lisp has to return, for there to be a clean exit.  That's
;; exactly what we now do.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; foreign types

(eval-when (compile load eval)
  (require :winapi)
  (require :foreign))

(eval-when (compile load eval)
(def-foreign-type SERVICE_TABLE_ENTRY 
    (:struct
     (lpServiceName win:lptstr)
     (lpServiceProc win::lp)))

(def-foreign-type SERVICE_STATUS
    (:struct
     (dwServiceType win:dword)
     (dwCurrentState win:dword)
     (dwControlsAccepted win:dword)
     (dwWin32ExitCode win:dword)
     (dwServiceSpecificExitCode win:dword)
     (dwCheckPoint win:dword)
     (dwWaitHint win:dword)))

(def-foreign-type ENUM_SERVICE_STATUS
    (:struct
     (lpServiceName win:lptstr)
     (lpDisplayName win:lptstr)
     (ServiceStatus SERVICE_STATUS)))
)

(def-foreign-type SERVICE_DESCRIPTION 
    (:struct
     (lpDescription win:lptstr)))

;; foreign calls

(def-foreign-call (StartServiceCtrlDispatcher "StartServiceCtrlDispatcherA")
    ((lpServiceTable (* SERVICE_TABLE_ENTRY)))
  :strings-convert t
  :error-value :os-specific
  :returning win:bool
  #+smp :release-heap-implies-allow-gc #+smp t
  :release-heap :always)

(def-foreign-call (RegisterServiceCtrlHandler "RegisterServiceCtrlHandlerA")
    ((lpServiceName win:lpctstr)
     (lpHandlerProc win::lp))
  :strings-convert t
  :returning win::lp			; SERVICE_STATUS_HANDLE
  :error-value :os-specific)

(def-foreign-call (SetServiceStatus "SetServiceStatus")
    ((lpServiceHandle win::lp)		; SERVICE_STATUS_HANDLE
     (lpServiceStatus (* SERVICE_STATUS)))
  :returning win:bool 
  :error-value :os-specific
  :strings-convert t
  #+smp :release-heap-implies-allow-gc #+smp t
  :release-heap :always)

(def-foreign-call (OutputDebugString "OutputDebugStringA")
    ((string win:lpctstr))
  :strings-convert t)

(def-foreign-call (OpenSCManager "OpenSCManagerA")
    ((lpMachineName win:lpctstr)
     (lpDatabaseName win:lpctstr)
     (wDesiredAccess win:dword))
  :strings-convert t
  :returning win::lp
  :error-value :os-specific)

(def-foreign-call (CloseServiceHandle "CloseServiceHandle")
    ((hSCObject win::lp)			; SC_HANDLE
     ) 
  :strings-convert t
  :returning win:bool)

(def-foreign-call (OpenService "OpenServiceA")
    ((handle win::lp)			; SC_HANDLE
     (lpServiceName win:lpctstr)
     (dwDesiredAccess win:dword))
  :strings-convert t
  :returning win::lp			; SC_HANDLE
  :error-value :os-specific)

(def-foreign-call (EnumServicesStatus "EnumServicesStatusA") 
    ((hSCManager win::lp)		; SC_HANDLE
     (dwServiceType win:dword) 
     (dwServiceState win:dword) 
     (lpServices (* ENUM_SERVICE_STATUS)) 
     (cbBufSize win:dword) 
     (pcbBytesNeeded win:lpdword) 
     (lpServicesReturned win:lpdword) 
     (lpResumeHandle win:lpdword))
  :strings-convert t
  :returning win:bool
  :error-value :os-specific)

(def-foreign-call (CreateService "CreateServiceA")
    ((hSCManager win::lp)		; SC_HANDLE
     (lpServiceName win:lpctstr)
     (lpDisplayName win:lpctstr)
     (dwDesiredAccess win:dword)
     (dwServiceType win:dword)
     (dwStartType win:dword)
     (dwErrorControl win:dword)
     (lpBinaryPathName win:lpctstr)
     (lpLoadOrderGroup win:lpctstr)
     (lpdwTagId win:lpdword)
     (lpDependencies win:lpctstr)
     (lpServiceStartName win:lpctstr)
     (lpPassword win:lpctstr))
  :returning win::lp			; SC_HANDLE 
  :error-value :os-specific
  :strings-convert t)

(def-foreign-call (ChangeServiceConfig2 "ChangeServiceConfig2A")
    ((hService win::lp)
     (dwInfoLevel win:dword)
     (lpInfo win::lp))
  :returning win:bool
  :error-value :os-specific)

(def-foreign-call (StartService "StartServiceA")
    ((hService win::lp)			; SC_HANDLE
     (dwNumServiceArgs win:dword)
     (lpServiceArgVectors (* win:lpctstr)))
  :returning win:bool
  :error-value :os-specific
  :strings-convert t)

(def-foreign-call (DeleteService "DeleteService")
    ((hService win::lp)			; SC_HANDLE
     )
  :returning win:bool
  :error-value :os-specific
  :strings-convert t)

(def-foreign-call (QueryServiceStatus "QueryServiceStatus")
    ((hService win::lp)			; SC_HANDLE
     (lpServiceStatus (* SERVICE_STATUS)))
  :returning win:bool
  :error-value :os-specific
  :strings-convert t)

(def-foreign-call (ControlService "ControlService")
    ((hService win::lp)			; SC_HANDLE
     (dwControl win:dword)
     (lpServiceStatus (* SERVICE_STATUS)))
  :returning win:bool
  :error-value :os-specific
  :strings-convert t
  #+smp :release-heap-implies-allow-gc #+smp t
  :release-heap :always)

(def-foreign-call (start_tray_icon_watcher "start_tray_icon_watcher") ()
  :returning :int
  :strings-convert nil)

;;; constants

(eval-when (compile eval)
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

;; ChangeServiceConfig2 options

(defconstant SERVICE_CONFIG_DESCRIPTION     1)
(defconstant SERVICE_CONFIG_FAILURE_ACTIONS 2)

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
)

;; globals

(defparameter *service-status* (allocate-fobject 'SERVICE_STATUS :c))
(defparameter *service-status-handle* nil)

(defparameter *service-init-func* nil)
(defparameter *service-main-func* nil)
(defparameter *service-stop-func* nil)
(defparameter *service-shutdown-func* nil)

(defparameter *main-process* nil)
(defparameter *main-process-gate* nil)

(eval-when (compile eval)
(defmacro ss-slot (slot) 
  `(fslot-value-typed 'SERVICE_STATUS :c *service-status* ,slot))
)

;; This operates as an immigrant thread.
(defun-foreign-callable service-main (argc argv)
  (declare (:convention :stdcall))
  (let ((argv-type `(:array (* :string) ,argc))
	(service-control-handler-addr 
	 (register-foreign-callable 'service-control-handler))
	err
	args
	service-name)
    (dotimes (i argc)
      (push (native-to-string (fslot-value-typed argv-type :c argv i)) args))
    
    (setf args (nreverse args))
    (setf service-name (pop args))

    ;; The name of the our process is "Immigrant Process".  Make it more
    ;; obvious what it's really doing:
    (setf (mp:process-name mp:*current-process*)
      (format nil "Immigrant Process for service ~s" service-name))

    (multiple-value-setq (*service-status-handle* err)
      (with-native-string (name service-name)
	(RegisterServiceCtrlHandler name service-control-handler-addr)))
    (when (zerop *service-status-handle*)
      (debug-msg "RegisterServiceCtrlHandler failed: ~A" (winstrerror err))
      (return-from service-main))

    (setf (ss-slot 'dwServiceType) 
      #.(logior SERVICE_WIN32_OWN_PROCESS SERVICE_INTERACTIVE_PROCESS))
    (setf (ss-slot 'dwControlsAccepted) #.SERVICE_ACCEPT_STOP)
    (setf (ss-slot 'dwWin32ExitCode) #.NO_ERROR)
    
    (when *service-init-func*
      (debug-msg "service-main: calling service init func")
      (setf (ss-slot 'dwCurrentState) #.SERVICE_START_PENDING)
      (setf (ss-slot 'dwCheckPoint) 0)
      (setf (ss-slot 'dwWaitHint) 10000) ;; 10 seconds
      (set-service-status)
      
      (when (null (funcall *service-init-func* args))
	(debug-msg "service-main: init returned error")
	(setf (ss-slot 'dwCurrentState) #.SERVICE_STOPPED)
	(setf (ss-slot 'dwWin32ExitCode) #.ERROR_SERVICE_SPECIFIC_ERROR)
	(setf (ss-slot 'dwServiceSpecificExitCode) 1)
	(set-service-status)
	(debug-msg "service-main: init: returning")
	(return-from service-main)))
    
    (setf (ss-slot 'dwCurrentState) #.SERVICE_RUNNING)
    (setf (ss-slot 'dwCheckPoint) 0)
    (setf (ss-slot 'dwWaitHint) 0)
    (set-service-status)

    (debug-msg "service-main: starting service main func")
    (setf *main-process-gate* (mp:make-gate nil))
    (setf *main-process*
      (mp:process-run-function (format nil "service main for ~a" service-name)
	#'(lambda ()
	    (funcall *service-main-func*)
	    (mp:open-gate *main-process-gate*))))
    (mp:process-wait 
     (format nil "waiting for service main for ~a to complete" service-name)
     #'mp:gate-open-p *main-process-gate*)
    (debug-msg "service-main: *main-process-gate* opened.")
    (setf *main-process* nil)
    
    (setf (ss-slot 'dwCurrentState) #.SERVICE_STOPPED)
    (set-service-status
     ;; This call, for some reason, gets an error when the user explicitly
     ;; exits the service app.   That's probably because the service
     ;; status handle has become invalid because the service control
     ;; dispatcher has terminated.
     :ignore-error t)))

(defun set-service-status (&key ignore-error)
  (multiple-value-bind (res err)
      (SetServiceStatus *service-status-handle* *service-status*)
    (when (and (null res) (null ignore-error))
      (debug-msg "SetServiceStatus failed: ~A" (winstrerror err))
      (big-exit))))
  
(defun-foreign-callable service-control-handler (fdwControl)
  (declare (:convention :stdcall))
  (case fdwControl
    (#.SERVICE_CONTROL_STOP
     (debug-msg "service-control-handler: got STOP")
     
     (setf (ss-slot 'dwCurrentState) #.SERVICE_STOP_PENDING)
     (setf (ss-slot 'dwCheckPoint) 0)
     ;; Request that programs querying our status check back every
     ;; second.
     (setf (ss-slot 'dwWaitHint) 1000) 
     (set-service-status)

     (when *service-stop-func*
       (debug-msg "service-control-handler: Calling service stop function")
       (funcall *service-stop-func*)
       (debug-msg "service-control-handler: service stop function returned"))

     (when *main-process*
       (debug-msg "service-control-handler: Terminating service-main")
       (mp:process-kill *main-process* :wait t)
       (debug-msg "service-control-handler: Terminated.")
       (debug-msg "service-control-handler: opening *main-process-gate*")
       (mp:open-gate *main-process-gate*)))
     
    (#.SERVICE_CONTROL_SHUTDOWN
     (debug-msg "service-control-handler: got SHUTDOWN")
     (when *service-shutdown-func*
       (setf (ss-slot 'dwCurrentState) #.SERVICE_STOP_PENDING)
       (set-service-status)
       (funcall *service-shutdown-func*))

     (setf (ss-slot 'dwCurrentState) #.SERVICE_STOPPED)
     (set-service-status))
    
    (#.SERVICE_CONTROL_INTERROGATE
     (debug-msg "service-control-handler: got INTERROGATE")
     (set-service-status))
    
    (t (debug-msg "service-control-handler: control code ~A is not handled"
		  fdwControl)))
  (values))

;; It is only safe to exit when this gate is closed
(defparameter *service-stopped-gate* nil)

(defun execute-service (service-name main &key init stop shutdown)
  (setf *service-stopped-gate* (mp:make-gate nil))

  ;; This is so the close button on the console will stop the service.
  (push 
   `(when (not (mp:gate-open-p *service-stopped-gate*))
      (debug-msg "Stopping service (~s) from *exit-cleanup-forms*"
		 ,service-name)
      (ntservice:stop-service ,service-name)
      (debug-msg "stop-service returned.  Waiting for *service-stopped-gate* for confirmation")
      (mp:process-wait "waiting for service to complete"
		       #'mp:gate-open-p *service-stopped-gate*)
      (debug-msg "Done stopping service from *exit-cleanup-forms*"))
   sys:*exit-cleanup-forms*)
  
  (mp:process-run-function "executing service"
    (lambda (service-name main init stop shutdown)
      (execute-service-1 service-name main init stop shutdown)
      (debug-msg "execute-service: service returned")
      (mp:open-gate *service-stopped-gate*))
    service-name main init stop shutdown)
  
  (mp:process-wait "waiting for service to complete"
		   #'mp:gate-open-p *service-stopped-gate*)
  ;; Only reached during normal service shutdown.
  (big-exit))

;; Does not return until the service is stopped.
(defun execute-service-1 (service-name main init stop shutdown)
  (setf *service-main-func* main)
  (setf *service-init-func* init)
  (setf *service-stop-func* stop)
  (setf *service-shutdown-func* shutdown)
  
  (let* ((service-main-addr (register-foreign-callable 'service-main))
	 (service-name (string-to-native service-name))
	 (service-table-type '(:array SERVICE_TABLE_ENTRY 2))
	 (service-table (allocate-fobject service-table-type :c)))
    (macrolet ((st-slot (index slot)
		 `(fslot-value-typed service-table-type
				     :c service-table ,index ,slot)))

      (start_tray_icon_watcher) ;; ensure that tray icon is visible after login

      (setf (st-slot 0 'lpServiceName) service-name)  ;; unused
      (setf (st-slot 0 'lpServiceProc) service-main-addr)
      ;; the null terminating entry
      (setf (st-slot 1 'lpServiceName) 0)
      (setf (st-slot 1 'lpServiceProc) 0)

      (debug-msg "calling StartServiceCtrlDispatcher()")
      (multiple-value-bind (res err)
	  (StartServiceCtrlDispatcher service-table)
	(when (null res)
	  (debug-msg "StartServiceCtrlDispatcher failed: ~D"
		     (winstrerror err))))
      (debug-msg "returned from StartServiceCtlDispatcher()")
      
      ;; some cleanup
      (aclfree service-name)
      (free-fobject service-table))))

;;;;;;;;;;

;; This is turned on to debug the ntservice module itself.  It is quite
;; verbose and the output won't likely be interesting to anyone but
;; someone working on this module.
(defvar *debug* nil)

(defun debug-msg (format-string &rest format-args)
  (when *debug*
    (format excl::*initial-terminal-io*
	    "~&[~x] ~?~&" (mp::process-os-id mp:*current-process*)
	    format-string format-args))
  (OutputDebugString (apply #'format nil format-string  format-args)))

(defun open-sc-manager (machine database desired-access)
  (when (null machine) (setf machine 0))
  (when (null database) (setf database 0))
  (multiple-value-bind (res err)
      (OpenSCManager machine database desired-access)
    (if* (zerop res)
       then (error "OpenSCManager failed: ~A" (winstrerror err))
       else res)))

(defun close-sc-manager (handle)
  (CloseServiceHandle handle))

(defmacro with-sc-manager ((handle machine database desired-access) &body body)
  `(let ((,handle (open-sc-manager ,machine ,database ,desired-access)))
     (unwind-protect (progn ,@body)
       (close-sc-manager ,handle))))

(defun open-service (smhandle name desired-access)
  (multiple-value-bind (shandle err)
      (OpenService smhandle name desired-access)
    (values (if (= 0 shandle) nil shandle) err)))

(defmacro with-open-service ((handle errvar smhandle name desired-access) 
			     &body body)
  `(multiple-value-bind (,handle ,errvar)
       (open-service ,smhandle ,name ,desired-access)
     (unwind-protect (progn ,@body)
       (if ,handle (CloseServiceHandle ,handle)))))

;;; just a test function.
(defun enum-services ()
  (with-sc-manager (schandle nil nil #.SC_MANAGER_ALL_ACCESS)
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
	  (EnumServicesStatus schandle #.SERVICE_WIN32 #.SERVICE_STATE_ALL
			      buf bufsize bytes-needed services-returned
			      resume-handle))
	(if* (null res)
	   then (if (not (= errcode #.ERROR_MORE_DATA))
		    (error "EnumServicesStatus error: ~A"
			   (winstrerror errcode)))
		(setf bufsize (fslot-value-typed :int :c bytes-needed))
		(setf buf (aclmalloc bufsize))
	   else (setf errcode 0)))
      
      (let ((count (fslot-value-typed :int :c services-returned)))
	(dotimes (i count)
	  (format t "~A -> ~A~%" 
		  (native-to-string
		   (fslot-value-typed `(:array ENUM_SERVICE_STATUS ,count)
				      :c buf i 'lpServiceName)) 
		  (native-to-string
		   (fslot-value-typed `(:array ENUM_SERVICE_STATUS ,count)
				      :c buf i 'lpDisplayName)))))
      
      (aclfree buf)
      (free-fobject bytes-needed)
      (free-fobject services-returned)
      (free-fobject resume-handle))))

(defun create-service (name displaystring cmdline 
		       &key (start :manual)
			    (interact-with-desktop t)
			    description
			    username ;; LocalSystem
			    (password ""))
  (with-sc-manager (schandle nil nil #.SC_MANAGER_ALL_ACCESS)
    (multiple-value-bind (handle err)
	(CreateService 
	 schandle 
	 name
	 displaystring
	 #.SERVICE_ALL_ACCESS
	 (logior #.SERVICE_WIN32_OWN_PROCESS 
		 (if interact-with-desktop #.SERVICE_INTERACTIVE_PROCESS 0))
	 (case start
	   (:auto #.SERVICE_AUTO_START)
	   (:manual #.SERVICE_DEMAND_START)
	   (t (error "create-service: Unrecognized start type: ~S" start)))
	 #.SERVICE_ERROR_NORMAL
	 cmdline
	 0 ;; no load order group
	 0 ;; no tag identifier
	 0 ;; no dependencies 
	 (or username 0) ;; 0 means LocalSystem
	 password)
      (if* (zerop handle)
	 then ;; error
	      (values nil err)
	 else (if description
		  (set-service-description-1 handle name description))
	      (CloseServiceHandle handle)
	      t))))
      

(defun delete-service (name)
  (with-sc-manager (sc nil nil #.SC_MANAGER_ALL_ACCESS)
    (with-open-service (handle err sc name #.STANDARD_RIGHTS_REQUIRED)
      (if* handle
	 then
	      (multiple-value-bind (res err)
		  (DeleteService handle)
		(if (null res)
		    (values nil err "DeleteService")
		  (values t res)))
	 else
	      (values nil err "OpenService")))))

(defun set-service-description (name description)
  (with-sc-manager (sm nil nil #.SC_MANAGER_ALL_ACCESS) 
    (with-open-service (h err sm name #.SERVICE_ALL_ACCESS)
      (if (null h)
	  (error "Failed to open service ~s: ~a"
		 name (winstrerror err)))
      (set-service-description-1 h name description))))

(defun set-service-description-1 (handle name description)
  (if (null description)
      (setf description ""))
  (let ((sd (ff:allocate-fobject 'SERVICE_DESCRIPTION :foreign-static-gc)))
    (with-native-string (description description)
      (setf (ff:fslot-value sd 'lpDescription) description)
      (multiple-value-bind (res err)
	  (ChangeServiceConfig2 handle #.SERVICE_CONFIG_DESCRIPTION
				sd)
	(if (zerop res)
	    (error "Failed to change description for service ~s: ~a"
		   name (winstrerror err)))))))

(defmacro with-service-status ((handle var) &body body)
  `(let ((,var (allocate-fobject 'SERVICE_STATUS :c)))
     (unwind-protect
	 (progn 
	   (query-service-status ,handle ,var)
	   ,@body)
       (free-fobject ,var))))

(defun query-service-status (handle ss)
  (multiple-value-bind (res err) (QueryServiceStatus handle ss)
    (when (null res) (error "QueryServiceStatus: ~A" (winstrerror err)))))

(defmacro get-service-state (ss)
  `(fslot-value-typed 'SERVICE_STATUS :c ,ss 'dwCurrentState))

(defmacro service-status-eql (ss status)
  `(eql (get-service-state ,ss) ,status))

(defmacro service-running-p (ss)
  `(service-status-eql ,ss #.SERVICE_RUNNING))

(defmacro service-start-pending-p (ss)
  `(service-status-eql ,ss #.SERVICE_START_PENDING))

(defmacro service-stopped-p (ss)
  `(service-status-eql ,ss #.SERVICE_STOPPED))

(defmacro service-stop-pending-p (ss)
  `(service-status-eql ,ss #.SERVICE_STOP_PENDING))

(defmacro get-service-wait-hint-in-seconds (ss)
  `(/ (fslot-value-typed 'SERVICE_STATUS :c ,ss 'dwWaitHint) 1000.0))

(defmacro sleep-wait-hint-time (ss)
  (let ((secs (gensym)))
    `(let ((,secs (get-service-wait-hint-in-seconds ,ss)))
       (if* (zerop ,secs)
	  then (sleep 0.5)
	  else (sleep ,secs)))))

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
    (with-sc-manager (sc nil nil #.SC_MANAGER_ALL_ACCESS)
      (with-open-service (handle err sc name #.SERVICE_ALL_ACCESS)
	(if (null handle)
	    (return (values nil err "OpenService")))
	
	(multiple-value-bind (res err)
	    (StartService handle 0 0)
	  (when (null res) (return (values nil err "StartService")))
	  (when (not wait) (return t)))
	      
	;; need to wait.
	(multiple-value-bind (success err)
	    (wait-for-service-to-start handle)
	  (if success t (values nil err)))))))

(defun stop-service (name &key (timeout 30))
  (debug-msg "stop-service: stopping ~a" name)
  (with-sc-manager (sc nil nil #.SC_MANAGER_ALL_ACCESS)
    (with-open-service (handle err sc name #.SERVICE_ALL_ACCESS)
      (when (null handle)
	(debug-msg "stop-service: handle is null")
	(return-from stop-service (values nil err "OpenService")))
      (with-service-status (handle ss)
	(when (service-stopped-p ss)
	  (debug-msg "stop-service: service stopped")
	  (return-from stop-service t))
	(when (service-stop-pending-p ss)
	  (debug-msg "stop-service: service stop pending")
	  (return-from stop-service
	    (wait-for-service-to-stop handle ss timeout)))
	  
	;; XXXX -- need option to stop dependencies.
	  
	(multiple-value-bind (res err)
	    (ControlService handle #.SERVICE_CONTROL_STOP ss)
	  (when (null res)
	    (debug-msg "stop-service: ControlService returned zero")
	    (return-from stop-service (values nil err "ControlService"))))

	(wait-for-service-to-stop handle ss timeout)))))

(defun wait-for-service-to-start (handle)
  (with-service-status (handle ss)
    (let ((start-tick-count (get-universal-time))
	  (old-checkpoint (get-service-checkpoint ss)))
      (while (service-start-pending-p ss)
	(let ((wait-time  (/ (get-service-wait-hint-in-seconds ss) 10.0)))
	  (when (< wait-time 1) (setf wait-time 1))
	  (when (> wait-time 10) (setf wait-time 10))
	  (debug-msg "wait-for-service-to-start: waiting ~d seconds"
		     wait-time)
	  (sleep wait-time))
	
	;; check again
	(query-service-status handle ss)
	(when (not (service-start-pending-p ss))
	  (return))

	(if* (> (get-service-checkpoint ss) old-checkpoint)
	   then ;; progress is being made.
		(setf old-checkpoint (get-service-checkpoint ss))
		(setf start-tick-count (get-universal-time))
	   else ;; no progress has been made.. see if we have exceeded
		;; the wait time.
		(if (> (- (get-universal-time) start-tick-count)
		       (get-service-wait-hint-in-seconds ss))
		    (return-from wait-for-service-to-start 
		      (values nil :timeout)))))
      
      (if* (service-running-p ss)
	 then t
	 else (error "wait-for-service-to-start: Unexpected service state: ~A"
		     (get-service-state ss))))))

(defun big-exit ()
  ;; If anything other than `0' is used, then stopping the service will
  ;; cause the console window to require closing.
  (exit 0 :no-unwind t :quiet t))

(defun winstrerror (code) (excl::get-winapi-error-string code))
