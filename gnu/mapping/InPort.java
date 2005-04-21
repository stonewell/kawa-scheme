package gnu.mapping;
import java.io.*;

public class InPort extends gnu.text.LineBufferedReader implements Printable
{
  public InPort (Reader in)
  {
    super (in);
  }

  public InPort (Reader in, String name)
  {
    this (in);
    setName(name);
  }

  public InPort (InputStream in)
  {
    super (in);
  }

  public InPort (InputStream in, String name)
  {
    this (in);
    setName(name);
  }

  public static Reader convertToReader (InputStream in, Object conv)
  {
    if (conv != null && conv != Boolean.TRUE)
      {
	String enc = (conv == Boolean.FALSE ? "8859_1" : conv.toString());
	try
	  {
	    return new java.io.InputStreamReader(in, enc);
	  }
	catch (java.io.UnsupportedEncodingException ex)
	  {
	    throw new RuntimeException("unknown character encoding: "+enc);
	  }
      }
    return new java.io.InputStreamReader(in);
  }

  public InPort (InputStream in, String name, Object conv)
    throws java.io.UnsupportedEncodingException
  {
    this (convertToReader(in, conv), name);
    if (conv == Boolean.FALSE)
      {
	// Use a fixed-size buffer.  This prevents really-long "lines"
	// from causing the buffer to grow to accomodate them.
	try
	  {
	    setBuffer(new char[2048]);
	  }
	catch (java.io.IOException ex) { /* ignored */ }
      }
    else
      setConvertCR(true);
  }

  private static InPort systemInPort
    = new TtyInPort (System.in, "<stdin>", OutPort.outInitial);
  public static final ThreadLocation inLocation
    = new ThreadLocation(new Symbol("in-default"));
  static { inLocation.setGlobal(systemInPort); }

  static public InPort inDefault ()
  {
    return (InPort) inLocation.get();
  }

  static public void setInDefault (InPort in)
  {
    inLocation.set(in);
  }

  public static InPort openFile(String fname)
    throws java.io.UnsupportedEncodingException,
           java.io.FileNotFoundException
  {
    java.io.InputStream strm = new java.io.FileInputStream(fname);
    strm = new java.io.BufferedInputStream(strm);
    return openFile(strm, fname);
  }

  public static InPort openFile(InputStream strm, String fname)
    throws java.io.UnsupportedEncodingException
  {
    return new InPort(strm, fname,
		      Environment.user().get("port-char-encoding"));
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<input-port");
    String name = getName();
    if (name != null)
      {
	ps.print (' ');
	ps.print (name);
      }
    ps.print ('>');
  }
}
