Turn your lisp application into a Windows NT/2000 service with the
ntservice package.

If you have a lisp application that you'd like to start up
automatically when the system starts... and shut down cleanly when the
system is shutting down, then this package is for you.

Follow these steps and you'll be on the road to servicedom. 

1) Compile ntservice.cl. 

2) Write/test/debug your application.  Make sure you load
ntservice.fasl along with it.

3) The main function in your application should call
ntservice:start-service.  The definition for ntservice:start-service
is as follows:

(defun start-service (main &key init stop)
  ...)

'main' should be a function (or a symbol naming function) which
constitutes the main loop of your program.  This function will be
called when the service starts running.

The keyword arguments 'init' and 'stop' are optional.  

'init' specifies a function (or a symbol naming a function) that
should be executed before the main loop is executed.  Such a function
might load in configuration settings or verify the system environment.
If the 'init' function returns 'nil', the service will not be started
and an error will be logged and/or reported.  Make sure 'init' returns
non-nil under normal circumstances.

'stop' specifies a function (or a symbol naming a function) that
should be executed when the service is to be stopped.  Such a function
might do work to ensure that the service stops cleanly.

4) Make your application into a standalone executable (check the
Allegro CL product documentation on generate-executable [Allegro CL
v6.1] or generate-application).

5) Call ntservice:create-service to add your program to the list of
Windows services.  The definition for ntservice:create-service is as
follows:

(defun create-service (name displaystring cmdline)
  ...)

'name' should be a string that identifies your service.  The maximum
string length is 256 characters. The service control manager database
preserves the case of the characters, but service name comparisons are
always case insensitive.  Forward-slash (/) and back-slash (\) are
invalid service name characters.

'displaystring' should be a string that contains the display name to
be used by user interface programs to identify the service. This
string has a maximum length of 256 characters. The name is
case-preserved in the service control manager. display name
comparisons are always case-insensitive.

'cmdline' should be a string that contains the command line for
executing your service program.  The first word in the string should
be the fully-qualified pathname to the executable.

Example:

(ntservice:create-service 
	"MyService" 
	"My lisp program service" 
	"c:\\devel\\program\\program.exe /runfast /dontcrash")

ntservice:create-service returns '1' if all is well.

Your service will be created w/ the following properties:
Manual start
Run as LocalSystem account
Allow program to interact with desktop

6) Try it out!  Your service should now be listed in the Services
control panel applet.  Try starting and stopping your service.  If it
works as planned, you can use the Services control panel applet to
make the service start automatically instead of manually.

Other notes:
-
If you want to remove your program from the list of services, load
ntservice.fasl and evaluate:

(ntservice:delete-service name)

where 'name' is the name you gave the service in your call to
ntservice:create-service.
-
The LocalSystem account is very powerful!  Be careful of what you
allow your program to do.  

Also note that the LocalSystem account usually does not have access to
network filesystems.  This may lead to confusion if your service tries
to access drive letters that are mapped to network drives, or if it
tries to access remote filesystems via UNC names (\\host\\share\file).

You can use the Services control panel applet to change who the
service runs as.  Note that no account but LocalSystem will be able to
interact w/ the desktop (i.e., your program's window will be invisible
if you don't run as LocalSystem).
-













