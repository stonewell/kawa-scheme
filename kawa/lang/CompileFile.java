package kawa.lang;
import java.io.*;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.SourceMessages;
import gnu.lists.*;
import gnu.kawa.lispexpr.LispReader;

/** Procedure to read and compile and entire file.
 * Creates a .zip archive containing the resulting classes.
 * @author	Per Bothner
 */

public class CompileFile extends Procedure2
{
  public CompileFile ()
  {
    super ("compile-file");
  }

  public static final Compilation read (String name, SourceMessages messages)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    try
      {
	InPort fstream = InPort.openFile(name);
	Compilation result = read(fstream, messages);
	fstream.close();
	return result;
      }
    catch (java.io.FileNotFoundException e)
      {
	throw new GenericError ("compile-file: file not found: " + name);
      }
    catch (java.io.IOException e)
      {
	throw new GenericError(name.toString());
      }
  }

  public static final Compilation read (InPort port, SourceMessages messages)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    return Interpreter.getInterpreter().parse(port, messages, 0);
  }

  public final Object apply2 (Object arg1, Object arg2)
    throws Throwable
  {
    if (! (arg1 instanceof FString))
      throw new WrongType (this.getName(), 1, "file name");
    SourceMessages messages = new SourceMessages();
    Compilation comp = read (arg1.toString (), messages);
    comp.compileToArchive(comp.getModule(), arg2.toString());
    if (messages.seenErrors())
      throw new gnu.text.SyntaxException(messages);
    return Values.empty;
  }

  /** Compile a Scheme source file to one or more .class file.
   * @param inname name of the Scheme source file
   * @param directory where to place the .class files
   * @param topname name for the class of the .class for the top-level code.
   *  If null, topname is derived from prefix and inname.
   * @param prefix to prepend classnames for functions
   * @return true iff there were syntax errors
   */
  public static void compile_to_files (String inname, String directory,
                                       String prefix, String topname,
                                       SourceMessages messages)
  {
    if (topname == null)
      {
	File infile = new File (inname);
	String short_name = infile.getName ();
        int dotIndex = short_name.lastIndexOf('.');
        if (dotIndex > 0)
          short_name = short_name.substring (0, dotIndex);
	short_name = Compilation.mangleNameIfNeeded(short_name);
	topname = short_name;
	if (prefix != null)
	  topname = prefix + short_name;
      }
    try
      {
	Compilation comp = read (inname, messages);
	if (messages.seenErrors())
	  return;
	try
	  {
	    comp.compileToFiles(comp.getModule(), topname, directory, prefix);
	  }
	catch (Throwable ex)
	  {
	    StringBuffer sbuf = new StringBuffer();
	    sbuf.append("Internal error while compiling ");
	    sbuf.append(inname);
	    int line = comp.getLine();
	    if (line > 0)
	      {
		sbuf.append(':');
		sbuf.append(line);
	      }
	    System.err.println(sbuf);
	    ex.printStackTrace(System.err);
	    System.exit(-1);
	  }
	if (messages.seenErrors())
	  return;
      }
    catch (gnu.text.SyntaxException ex)
      {
        // Got a fatal error.
        if (ex.getMessages() != messages)
          throw new RuntimeException ("confussing syntax error: "+ex);
        // otherwise ignore it - it's already been recorded in messages.
      }
    catch (IOException ex)
      {
        throw new RuntimeException (ex.toString ());
      }
  }
}
