package gnu.mapping;

public class Future extends Thread
{
  public RunnableClosure closure;

  public Future (Procedure action, CallContext parentContext)
  {
    closure = new RunnableClosure(action, parentContext);
  }

  public Future (Procedure action,
                 CallContext parentContext, Environment penvironment)
  {
    closure = new RunnableClosure (action, parentContext, penvironment);
    closure.environment.setName(getName());
  }

  public Future (Procedure action, Environment penvironment,
		 InPort in, OutPort out, OutPort err)
  {
    closure = new RunnableClosure (action, penvironment, in, out, err);
  }

  public Future (Procedure action)
  {
    closure = new RunnableClosure(action);
  }

  /** Get the CallContext we use for this Thread. */
  public final CallContext getCallContext() {
    return closure.getCallContext();
  }

  public Environment getEnvironment() {
    return closure.environment;
  }

  public void run() {
    closure.run();
  }

  public Object waitForResult ()
  {
    try
      {
	join ();
      }
    catch (InterruptedException ex)
      {
	throw new RuntimeException ("thread join [force] was interrupted");
      }
    Throwable ex = closure.exception;
    if (ex != null)
      throw WrappedException.wrapIfNeeded(ex);
    return closure.result;
  }

  public String toString() {
    StringBuffer buf = new StringBuffer();
    buf.append ("#<future ");
    buf.append(getName());
    buf.append(">");
    return buf.toString();
  }
}
