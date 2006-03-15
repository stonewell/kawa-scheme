package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.xml.*;
import gnu.kawa.xml.KNode;
import gnu.lists.*;
import java.math.BigDecimal;

public class StringValue extends Procedure1
{
  public static final StringValue stringValue
  = new StringValue("string-value");
  public static final StringValue string = new StringValue("string");

  public StringValue(String name)
  {
    super(name);
  }

  public Object apply1 (Object node)
  {
    StringBuffer sbuf = new StringBuffer(100);
    if (node instanceof Values)
      {
	TreeList tlist = (TreeList) node;
	int index = 0;
	for (;;)
	  {
	    int kind = tlist.getNextKind(index);
	    if (kind == Sequence.EOF_VALUE)
	      break;
	    if (kind == Sequence.OBJECT_VALUE)
	      stringValue(tlist.getPosNext(index), sbuf);
	    else
	      tlist.stringValue(tlist.posToDataIndex(index), sbuf);
	    index = tlist.nextPos(index);
	  }
      }
    else
      stringValue(node, sbuf);
    return sbuf.toString();
  }

  public static String stringValue (Object node)
  {
    StringBuffer sbuf = new StringBuffer();
    stringValue(node, sbuf);
    return sbuf.toString();
  }

  public static void stringValue (Object node, StringBuffer sbuf)
  {
    if (node instanceof KNode)
      {
	KNode pos = (KNode) node;
	NodeTree tlist = (NodeTree) pos.sequence;
	tlist.stringValue(tlist.posToDataIndex(pos.ipos), sbuf);
	return;
      }
    if (node instanceof BigDecimal)
      node = XMLPrinter.formatDecimal((BigDecimal) node);
    else if (node instanceof Double || node instanceof gnu.math.DFloNum)
      node = XMLPrinter.formatDouble(((Number) node).doubleValue());
    else if (node instanceof Float)
      node = XMLPrinter.formatFloat(((Number) node).floatValue());
    if (node != null && node != Values.empty)
      sbuf.append(node);
  }
}