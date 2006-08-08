package gnu.kawa.lispexpr;
import gnu.mapping.*;
import java.io.*;
import gnu.kawa.xml.ElementType; // FIXME

public class XmlNamespace extends Namespace
  implements Externalizable
{
  public static final XmlNamespace HTML =
    getInstance("html", "http://www.w3.org/1999/xhtml");

  public static XmlNamespace getInstance (String prefix, String uri)
  {
    String xname = prefix + " -> "+ uri;
    synchronized (nsTable)
      {
	Object old = nsTable.get(xname);
	if (old instanceof XmlNamespace)
	  return (XmlNamespace) old;
	XmlNamespace ns = new XmlNamespace();
        ns.setName(uri);
        ns.prefix = prefix;
	nsTable.put(xname, ns);
	return ns;
      }
  }

  public Object get (String name)
  {
    return ElementType.make(getSymbol(name));
  }

  public boolean isConstant (String key)
  {
    return true;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(getName());
    out.writeObject(prefix);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    setName((String) in.readObject());
    prefix = (String) in.readObject();
  }

  public Object readResolve() throws ObjectStreamException
  {
    String xname = prefix + " -> "+ getName();
    Namespace ns = (Namespace) nsTable.get(xname);
    if (ns instanceof ClassNamespace)
      return ns;
    nsTable.put(xname, this);
    return this;
  }
}