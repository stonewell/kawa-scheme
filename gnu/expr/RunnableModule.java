package gnu.expr;
import gnu.mapping.*;

public interface RunnableModule
{
    public void run (CallContext ctx)  throws Throwable;
    /** Check if run has been invoked.
     * @param value - should be true - probably pointless.
     */
    public boolean checkRunDone(boolean value);
}
