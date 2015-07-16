package gnu.expr;
import gnu.mapping.*;

public interface RunnableModule
{
    public void run (CallContext ctx)  throws Throwable;
    public boolean checkRunDone(boolean value);
}
