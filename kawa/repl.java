package kawa;
import kawa.lang.*;
import kawa.standard.*;
import java.io.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.SourceMessages;
import gnu.lists.*;

/** Start a "Read-Eval-Print-Loop" for the Kawa Scheme evaluator. */

public class repl extends Procedure0or1
{
  public static String compilationDirectory = null;
  public static String compilationTopname = null;
  public static String compilationPrefix = null;

  Interpreter interp;

  public repl(Interpreter interp)
  {
    this.interp = interp;
  }

  public Object apply0 ()
  {
    Shell.run(interp, Environment.getCurrent());
    return Scheme.voidObject;
  }

  public Object apply1(Object env)
  {
    Shell.run(interp, (Environment) env);
    return Scheme.voidObject;
  }

  static void bad_option (String str)
  {
    System.err.println ("kawa: bad option `" + str + "'");
    System.exit (-1);
  }

  public static FVector commandLineArguments;

  public static String homeDirectory;

  static void checkInitFile ()
  {
    /* Set homeDirectory;  if first time called, run ~/.kawarc.scm. */
    if (homeDirectory == null)
      {
	File initFile = null;
	homeDirectory = System.getProperty ("user.home");
	Object scmHomeDirectory;
	if (homeDirectory != null)
	  {
	    scmHomeDirectory = new FString (homeDirectory);
	    String file_separator = System.getProperty("file.separator");
	    String kawarc_name =
	      "/".equals(file_separator) ? ".kawarc.scm"
	      : "kawarc.scm";
	    initFile = new File(homeDirectory, kawarc_name);
	  }
	else
	  scmHomeDirectory = Scheme.falseObject;
	Environment.define_global("home-directory", scmHomeDirectory);
	if (initFile != null && initFile.exists())
	  Shell.runFile(initFile.getPath());
      }
  }

  public static void setArgs (String[] args, int arg_start)
  {
    Object[] array = new Object[args.length - arg_start];
    for (int i = arg_start;  i < args.length;  i++)
      array[i - arg_start] = new FString (args[i]);
    commandLineArguments = new FVector (array);  // FIXME scsh has list
    // FIXME scsh also has command-line proc
    Environment.define_global ("command-line-arguments",
			       commandLineArguments);
  }

  public static Interpreter getInterpreter()
  {
    if (Interpreter.defaultInterpreter == null)
      {
	Interpreter.defaultInterpreter = Interpreter.getInstance(null);
	Environment.setCurrent(Interpreter.defaultInterpreter.getEnvironment());
      }
    return Interpreter.defaultInterpreter;
  }

  public static void main(String args[])
  {
    boolean shutdownRegistered =
      gnu.text.WriterManager.instance.registerShutdownHook();
    boolean windowStarted = false;
    try
      {
	int iArg = 0;
	boolean something_done = false;
	for ( ;  iArg < args.length;  iArg++)
	  {
	    String arg = args[iArg];
	    if (arg.equals ("-c") || arg.equals ("-e"))
	      {
		iArg++;
		if (iArg == args.length)
		  bad_option (arg);
		getInterpreter();
		setArgs (args, iArg+1);
		if (arg.equals ("-c"))
		  checkInitFile();
		Shell.runString(args[iArg], Interpreter.defaultInterpreter, Environment.getCurrent());
		something_done = true;
	      }
	    else if (arg.equals ("-f"))
	      {
		iArg++;
		if (iArg == args.length)
		  bad_option (arg);
		getInterpreter();
		setArgs (args, iArg+1);
		checkInitFile();
		Shell.runFile (args[iArg]);
		something_done = true;
	      }
	    else if (arg.equals ("-s") || arg.equals ("--"))
	      {
		iArg++;
		getInterpreter();
		setArgs (args, iArg);
		checkInitFile();
		Shell.run(Interpreter.defaultInterpreter, Environment.getCurrent());
		return;
	      }
	    else if (arg.equals ("-w"))
	      {
		getInterpreter();
		setArgs (args, iArg);
		checkInitFile();
		// Do this instead of just new GuiConsole in case we have
		// configured --without-awt.
		try
		  {
		    Class.forName("kawa.GuiConsole").newInstance();
		    windowStarted = true;
		  }
		catch (Exception ex)
		  {
		    System.err.println("failed to create Kawa window: "+ex);
		    System.exit (-1);
		  }
		something_done = true;
	      }
	    else if (arg.equals ("-d"))
	      {
		iArg++;
		if (iArg == args.length)
		  bad_option (arg);
		compilationDirectory = args[iArg];
	      }
	    else if (arg.equals ("-P"))
	      {
		iArg++;
		if (iArg == args.length)
		  bad_option (arg);
		compilationPrefix = args[iArg];
	      }
	    else if (arg.equals ("-T"))
	      {
		iArg++;
		if (iArg == args.length)
		  bad_option (arg);
		compilationTopname = args[iArg];
	      }
	    else if (arg.equals ("-C"))
	      {
		++iArg;
		getInterpreter();
		if (iArg == args.length)
		  bad_option (arg);
		for ( ; iArg < args.length;  iArg++)
		  {
		    arg = args[iArg];
		    try
		      {
			System.err.println("(compiling "+arg+")");
			SourceMessages messages = new SourceMessages();

			CompileFile.compile_to_files(arg,
						     compilationDirectory,
						     compilationPrefix,
						     compilationTopname,
						     messages);
			boolean sawErrors = messages.seenErrors();
			messages.checkErrors(System.err, 50);
			if (sawErrors)
			  System.exit(-1);
		      }
		    catch (Throwable ex)
		      {
			System.err.println("Internal error while compiling "+arg);
			ex.printStackTrace(System.err);
			System.exit(-1);
		      }
		  }
		return;
	      }
	    else if (arg.equals("--connect"))
	      {
		getInterpreter();
		++iArg;
		if (iArg == args.length)
		  bad_option (arg);
		int port;
		if (args[iArg].equals("-"))
		  port = 0;
		else
		  {
		    try
		      {
			port = Integer.parseInt(args[iArg]);
		      }
		    catch (NumberFormatException ex)
		      {
			bad_option ("--connect port#");
			port = -1; // never seen.
		      }
		  }
		try
		  {
		    java.net.Socket socket = new java.net.Socket("localhost",port);
		    Telnet conn = new Telnet(socket, true);
		    java.io.InputStream sin = conn.getInputStream();
		    java.io.OutputStream sout = conn.getOutputStream();
		    java.io.PrintStream pout = new PrintStream (sout, true);
		    System.setIn(sin);
		    System.setOut(pout);
		    System.setErr(pout);
		  }
		catch (java.io.IOException ex)
		  {
		    ex.printStackTrace(System.err);
		    throw new Error(ex.toString());
		  }
	      }
	    else if (arg.equals("--server"))
	      {
		getInterpreter();
		++iArg;
		if (iArg == args.length)
		  bad_option (arg);
		int port;
		if (args[iArg].equals("-"))
		  port = 0;
		else
		  {
		    try
		      {
			port = Integer.parseInt(args[iArg]);
		      }
		    catch (NumberFormatException ex)
		      {
			bad_option ("--server port#");
			port = -1; // never seen.
		      }
		  }
		try
		  {
		    java.net.ServerSocket ssocket
		      = new java.net.ServerSocket(port);
		    port = ssocket.getLocalPort();
		    System.err.println("Listening on port "+port);
		    for (;;)
		      {
			System.err.print("waiting ... ");  System.err.flush();
			java.net.Socket client = ssocket.accept();
			System.err.println("got connection from "
					   +client.getInetAddress()
					   +" port:"+client.getPort());
			serveTelnet(Interpreter.defaultInterpreter, client);
		      }
		  }
		catch (java.io.IOException ex)
		  {
		    throw new Error(ex.toString());
		  }
	      }
	    else if (arg.equals("--main"))
	      {
		Compilation.generateMainDefault = true;
	      }
	    else if (arg.equals("--applet"))
	      {
		Compilation.generateAppletDefault = true;
	      }
	    else if (arg.equals("--debug-dump-zip"))
	      {
		gnu.expr.ModuleExp.dumpZipPrefix = "kawa-zip-dump-";
	      }
	    else if (arg.equals("--debug-print-expr"))
	      {
		gnu.expr.ModuleExp.debugPrintExpr = true;
	      }
	    else if (arg.equals("--debug-print-final-expr"))
	      {
		Compilation.debugPrintFinalExpr = true;
	      }
	    else if (arg.equals("--module-static"))
	      {
		gnu.expr.Compilation.moduleStatic = 1;
	      }
	    else if (arg.equals("--fewer-classes"))
	      {
		gnu.expr.Compilation.fewerClasses = true;
	      }
	    else if (arg.equals("--cps"))
	      {
		gnu.expr.Compilation.fewerClasses = true;
		gnu.expr.Compilation.usingTailCalls = true;
		gnu.expr.Compilation.usingCPStyle = true;
	      }
	    else if (arg.equals("--full-tailcalls"))
	      {
		gnu.expr.Compilation.usingTailCalls = true;
	      }
	    else if (arg.equals("--no-full-tailcalls"))
	      {
		gnu.expr.Compilation.usingTailCalls = false;
	      }
	    else if (arg.equals("--version"))
	      {
		System.out.print("Kawa ");
		System.out.print(Version.getVersion());
		System.out.println();
		System.out.println("Copyright (C) 2001 Per Bothner");
		something_done = true;
	      }
	    else if (arg.length () > 0 && arg.charAt(0) == '-')
	      { // Check if arg is a known language name.
		Interpreter previous = Interpreter.defaultInterpreter;
		String name = arg;
		if (name.length() > 2 && name.charAt(0) == '-')
		  name = name.substring(name.charAt(1) == '-' ? 2 :1);
		Interpreter interpreter = Interpreter.getInstance(name);
		if (interpreter == null)
		  bad_option(arg);
		else
		  {
		    Interpreter.defaultInterpreter = interpreter;
		    if (previous == null)
		      Environment.setCurrent(interpreter.getEnvironment());
		  }
	      }
	    else
	      break;
	  }
	if (something_done)
	  return;
	getInterpreter();
	if (iArg < args.length)
	  {
	    setArgs (args, iArg+1);
	    checkInitFile();
	    Shell.runFile (args[iArg]);
	  }
	else
	  {
	    setArgs (args, iArg);
	    checkInitFile();
	    Shell.run(Interpreter.defaultInterpreter);
	  }
      }
    finally
      {
	if (! shutdownRegistered && ! windowStarted)
	  {
	    // Redundant if registerShutdownHook succeeded (e.g on JDK 1.3).
	    gnu.mapping.OutPort.runCleanups();
	  }
      }
   }

  /** Run a Kawa repl as a telnet server.
      @param client A client that has connected to us,
      and that wants to use the telnet protocol to talk to a
      Scheme read-eval-print-loop. */
  static void serveTelnet (Interpreter interp, java.net.Socket client)
    throws java.io.IOException
  {
    Telnet conn = new Telnet(client, true);
    java.io.OutputStream sout = conn.getOutputStream();
    java.io.InputStream sin = conn.getInputStream();
    OutPort out = new OutPort(sout);
    TtyInPort in = new TtyInPort(sin, "<stdin>", out);
    /*
    conn.request(Telnet.DO, Telnet.EOF);
    conn.request(Telnet.DO, Telnet.NAWS);
    conn.request(Telnet.DO, Telnet.TTYPE);
    conn.request(Telnet.DO, Telnet.LINEMODE);
    */

    Thread thread = new Future(new SocketRepl(interp, client),
			       interp.getEnvironment(),
			       in, out, out);
    thread.start();
  }
}

class SocketRepl extends Procedure0
{
  // close when finished.
  java.net.Socket socket;

  Interpreter interp;

  public SocketRepl(Interpreter interp, java.net.Socket socket)
  {
    this.interp = interp;
    this.socket = socket;
  }

  public Object apply0 ()
  {
    try
      {
	Shell.run(interp, Environment.getCurrent());
	return Scheme.voidObject;
      }
    finally
      {
	try
	  {
	    socket.close();
	  }
	catch (java.io.IOException ex)
	  {
	  }
      }
  }
}

