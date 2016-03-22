// Copyright (c) 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
/* #ifdef JAVA2 */
import java.util.Iterator;
import java.util.List;
/* #endif */

/** A function that maps a List into the sequence of its elements. */

public class ListItems extends MethodProc
{
  public static ListItems listItems = new ListItems();

    private ListItems() {
        applyToConsumerMethod =
            Procedure.lookupApplyHandle(ListItems.class, "applyToConsumer");
    }

    public static Object applyToConsumer(Procedure proc, CallContext ctx) throws Throwable {
    Consumer out = ctx.consumer;
    Object arg = ctx.getNextArg();
    ctx.lastArg();

    List list = (List) arg;
    if (arg instanceof AbstractSequence)
      {
	((AbstractSequence) arg).consumePosRange(0, -1, out);
	return null;
      }
    Iterator iter = list.iterator();
    while (iter.hasNext())
      {
	Object val = iter.next();
	Values.writeValues(val, out);
      }
    return null;
  }
}
