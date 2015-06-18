package gnu.expr;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.kawa.reflect.ClassMemberLocation;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.WriterManager;
import gnu.kawa.util.ExitCalled;

/**
 * Class for the dummy top-level function of a module.
 */

public abstract class ModuleBody implements RunnableModule
{
  protected boolean runDone;

  public void run (CallContext ctx)  throws Throwable
  {
  }

  public void run ()
  {
    synchronized (this)
      {
        if (runDone)
          return;
        runDone = true;
      }
    run (VoidConsumer.instance);
  }

    public static void runToVoid (RunnableModule mod)
    {run(mod, VoidConsumer.instance); }

  public void run (Consumer out) { run(this, out); }
  public static void run (RunnableModule mod, Consumer out)
  {
    // This should match the "run" method generated in Compilation.
    CallContext ctx = CallContext.getInstance();
    Consumer save = ctx.consumer;
    ctx.consumer = out;
    Throwable th;
    try
      {
	mod.run(ctx);
	th = null;
      }
    catch (Throwable ex)
      {
	th = ex;
      }
    runCleanup(ctx, th, save);
  }

  public static void runCleanup (CallContext ctx, Throwable th, Consumer save)
  {
    if (th == null)
      {
	try
	  {
	    ctx.runUntilDone();
	  }
	catch (Throwable ex)
	  {
	    th = ex;
	  }
      }
    ctx.consumer = save;
    if (th != null)
      {
        WrappedException.rethrow(th);
      }
  }

  private static boolean mainPrintValues;

  /** True if runAsMain should print values (in top-level expressions). */
  public static boolean getMainPrintValues()
  {
    return mainPrintValues;
  }

  public static void setMainPrintValues(boolean value)
  {
    mainPrintValues = value;
  }

  /** Number of times exitDecrement calls before we exit. */
  private static int exitCounter;
  /** See exitDecrement. */
  public static synchronized void exitIncrement()
  {
    if (exitCounter == 0)
      exitCounter++;
    exitCounter++;
  }

  /** Work around an AWT bug, where AWT threads are non-daemon.
   * Thus if you start up AWT, the JVM will wait for the AWT to finish,
   * even if there are no other non-daemon threads.
   * So call exitIncrement() each time a Freme is created,
   * and call exitDecrement() when a Frame is closed. */
  public static synchronized void exitDecrement()
  {
    int counter = exitCounter;
    if (counter > 0)
      {
	counter--;
	if (counter == 0)
	  {
	    System.exit(0);
	  }
	else
	  exitCounter = counter;
      }
  }

  /** This is invoked by main when ModuleBody is compiled with --main. */
  public final void runAsMain ()
  {
    runAsMain(this);
  }

  /** This is invoked by main when ModuleBody is compiled with --main. */
  public static void runAsMain (RunnableModule module)
  {
    boolean registered = WriterManager.instance.registerShutdownHook();
    try
      {
        ExitCalled.push();
	CallContext ctx = CallContext.getInstance();
	if (getMainPrintValues())
	  {
	    OutPort out = OutPort.outDefault();
	    ctx.consumer = kawa.Shell.getOutputConsumer(out);
	    module.run(ctx);
	    ctx.runUntilDone();
	    out.freshLine();
	  }
	else
	  {
            ctx.consumer = VoidConsumer.instance;
            module.run(ctx);
	    ctx.runUntilDone();
	  }
        if (! registered)
          gnu.kawa.io.OutPort.runCleanups();
	exitDecrement();
      }
    catch (ExitCalled ex)
      {
         throw ex; // handled by ExitCalled.pop below.
      }
    catch (Throwable ex)
      {
	ex.printStackTrace();
	gnu.kawa.io.OutPort.runCleanups();
	System.exit(-1);
      }
    finally
      {
        ExitCalled.pop();
      }
  }

}
