package gnu.text;

/** Represents an error message from processing a "source" file. */

public class SourceError
{
  /** Used to chain to the "next" message. */
  public SourceError next;

  /** The seriousness of the error - one of 'w' (for warning),
   * 'e' (for error), or 'f' (for fatal error). */
  public char severity;

  /* The name or URL of the file containing the error. */
  public String filename;

  /** The line number of the error, with 1 being the top line.
   * The value 0 means unknown or not applicable (such as the entire file). */
  /** The (1-origin) location of the error. */
  public int line;

  /** The column number of the error, with 1 being the left-most column.
   * The value 0 means unknown or not applicable (such as the entire line). */
  public int column;

  /** The actual error message.
   * This is post-localization and -formatting.
   * It can contain multiple lines, separated by '\n'.*/
  public String message;

  public SourceError(char severity, String filename, int line, int column, 
		     String message)
  {
    this.severity = severity;
    this.filename = filename;
    this.line = line;
    this.column = column;
    this.message = message;
  }

  /** Create a new SourceError using the current line/column from
   * a <code>LineBufferedReader</code>. */
  public SourceError(LineBufferedReader port, char severity, String message)
  {
    this(severity, port.getName(),
	 port.getLineNumber() + 1, port.getColumnNumber(),
	 message);
    if (column >= 0)
      column++;
  }

  /** Convert the error to a String.
   * The String starts with filename, line and option column,
   * followed by the message.  Warning messages are indicated as such. */
  public String toString()
  {
    StringBuffer buffer = new StringBuffer ();
    buffer.append (filename == null ? "<unknown>" : filename);
    if (line != 0 || column != 0)
      {
	buffer.append (':');
	buffer.append (line);
	if (column > 0)
	  {
	    buffer.append (':');
	    buffer.append (column);
	  }
      }
    buffer.append (": ");
    if (severity == 'w')
      buffer.append("warning - ");
    buffer.append (message);
    return buffer.toString ();
  }

  public void print(java.io.PrintWriter out)
  {
    out.print(this);
  }

  public void println(java.io.PrintWriter out)
  {
    String line = toString();
    for (;;)
      {
        int nl = line.indexOf('\n');
        if (nl < 0)
          break;
        out.println(line.substring(0, nl));
        line = line.substring(nl+1);
      }
    out.println(line);
  }

  public void println(java.io.PrintStream out)
  {
    String line = toString();
    for (;;)
      {
        int nl = line.indexOf('\n');
        if (nl < 0)
          break;
        out.println(line.substring(0, nl));
        line = line.substring(nl+1);
      }
    out.println(line);
  }
}
