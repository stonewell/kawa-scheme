// Copyright (c) 2001, 2002, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.math.RatNum;
import gnu.math.IntNum;
import java.io.*;
import gnu.text.Char;
import gnu.expr.Keyword;
/* #ifdef enable:XML */
import gnu.kawa.xml.KNode;
import gnu.xml.XMLPrinter;
/* #endif */
import gnu.kawa.io.CheckConsole;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.PrettyWriter;
import gnu.kawa.xml.XmlNamespace;
import gnu.kawa.lispexpr.LispLanguage;
import gnu.text.Printable;
import gnu.kawa.models.Picture;
import gnu.kawa.models.SVGUtils;
import java.util.List;
/* #ifdef use:java.util.regex */
import java.util.regex.*;
/* #endif */

/** Handle formatted output for Lisp-like languages. */

public class DisplayFormat extends AbstractFormat
{
  /** Fluid parameter to specify default output base for printing rationals. */
  public static final ThreadLocation outBase
    = new ThreadLocation("out-base");
  static { outBase.setGlobal(IntNum.ten()); }
  /** True if we should print a radix indicator when printing rationals.
   * The default is no; otherwise we follow Common Lisp conventions. */
  public static final ThreadLocation outRadix
    = new ThreadLocation("out-radix");

    public static final DisplayFormat schemeDisplayFormat
        = new DisplayFormat(false, 'S');

    public static final DisplayFormat schemeWriteSimpleFormat
        = new DisplayFormat(true, 'S');
    public static final DisplayFormat schemeWriteFormat
        = new DisplayFormat(true, 'S');
    public static final DisplayFormat schemeWriteSharedFormat
        = new DisplayFormat(true, 'S');
    static {
        schemeWriteFormat.checkSharing = 0;
        schemeWriteSharedFormat.checkSharing = 1;
    }

    /** Controls whether we check for sharing and cycles.
     * 1: check for sharing; 0: check for cycles: -1: no checking. */
    public int checkSharing = -1;

  /** Create a new instance.
   * @param readable if output should be formatted so it could be read
   *   back in again, for example strings shoudl be quoted.
   * @param language the programming language style to use, where
   *   'S' is Scheme, 'C' is Common Lisp, and 'E' is Emacs Lisp.
   */
  public DisplayFormat(boolean readable, char language)
  {
    this.readable = readable;
    this.language = language;
  }

  public static DisplayFormat getEmacsLispFormat(boolean readable)
  {
    return new DisplayFormat(readable, 'E');
  }

  public static DisplayFormat getCommonLispFormat(boolean readable)
  {
    return new DisplayFormat(readable, 'C');
  }

  public static DisplayFormat getSchemeFormat(boolean readable)
  {
    return new DisplayFormat(readable, 'S');
  }

  boolean readable;

  /** 'S' is Scheme-style; 'C' is CommonLisp-style;  'E' is Emacs-style.
   * Note Emacs has its own sub-class gnu.jemacs.lang.Print. */
  char language;

  public boolean getReadableOutput () { return readable; }

    @Override
    public boolean textIsCopied() { return ! readable; }

  @Override
  public void writeBoolean(boolean v, Consumer out)
  {
    write (language == 'S' ? (v ? "#t" : "#f") : (v ? "t" : "nil"), out);
  }

  @Override
  public void write (int v, Consumer out)
  {
    if (! getReadableOutput ())
      Char.print(v, out);
    else
      {
	if (language == 'E'
	    && v > ' ')
	  {
	    out.write('?');
            Char.print(v, out);
	  }
	// else if (language == 'E') ...
	else
	  write(Char.toScmReadableString(v), out);
      }
  }

  /**
   * Format a list.
   * 
   * Try to find shared structures in a list. To accomplish this, each subobject
   * is hashed to the idhash, which is used later to determine whether we've seen
   * a subobject before. There is an added complication when you consider cases
   * like this:
   * 
   * '((b . #1=(a . z)) 3)
   * 
   * It is not known in advance that the printer will have to emit an extra
   * ')' after the '(a . z) pair. Every time we CDR further into the list, we
   * push a position marker onto a stack. Once we've examined the tail of this
   * sublist, we pop all the posn markers off and tell the pretty printer that
   * it might have to emit an extra ')' if the corresponding posn marker 
   * becomes active.
   *
   * @param value The list on which the method CDR's, termination occurs when
   * this becomes a non-pair or the empty list
   * @param out The output port that is responsible for the pretty printing
   */
  public void writeList(LList value, Consumer out)
  {
    PrettyWriter pout =
        out instanceof PrintConsumer ? ((PrintConsumer) out).getPrettyWriter()
        : null;
    // The stack of position markers, populated by CDR'ing further into the list.
    int[] posnStack = null;
    Object[] tailStack = null;
    int stackTail = 0;
    Object list = value;
    PrintConsumer.startLogicalBlock("(", false, ")", out);
    
    while (list instanceof Pair)
      {
	Pair pair = (Pair) list;
	writeObject(pair.getCar(), out);
        list = pair.getCdr();
        if (! getReadableOutput())
          list = Promise.force(list);
        if (list == LList.Empty)
          break;
        PrintConsumer.writeSpaceFill(out);
        if (! (list instanceof Pair))
	  {
	    out.write(". ");
	    writeObject(LList.checkNonList(list), (Consumer) out);
	    break;
	  }
	if (pout != null && checkSharing >= 0)
	  {
	    
	    int hashIndex = pout.IDHashLookup(list);
	    int posn = pout.IDHashGetFromIndex(hashIndex);
	    if (posn == -1)
	      // Then this is a new (sub)object to be marked
	      {
		// writePositionMarker will return the index to which is was enqueued
		posn = pout.writePositionMarker(true);
		if (posnStack == null) {
		  posnStack = new int[128]; // should be plently for most cases.
                  tailStack = new Object[128];
                }
		else if (stackTail >= posnStack.length)
		  {
		    int[] newPStack = new int[posnStack.length << 1];
		    System.arraycopy(posnStack, 0, newPStack, 0, stackTail);
		    posnStack = newPStack;
                    Object[] newTStack = new Object[posnStack.length << 1];
		    System.arraycopy(tailStack, 0, newTStack, 0, stackTail);
		    tailStack = newTStack;
		  }
		posnStack[stackTail] = posn;
                tailStack[stackTail++] = list;
		// Mark (hash) this object
		pout.IDHashPutAtIndex(list, posn, hashIndex);
	      }
	    else
	      {
		out.write(". ");
		pout.writeBreak(PrettyWriter.NEWLINE_FILL);
		pout.writeBackReference(posn);
		list = LList.Empty;
		break;
	      }
	  }
      }
    for (;--stackTail >= 0;) {
      pout.writePairEnd(posnStack[stackTail]);
      if (checkSharing == 0)
          pout.IDHashRemove(tailStack[stackTail]);
    }

    PrintConsumer.endLogicalBlock(")", out);
  }

    public void writeObject(Object obj, Consumer out) { 
        PrettyWriter pout = out instanceof PrintConsumer
            ? ((PrintConsumer) out).getPrettyWriter()
            : null;
        boolean popIDHashNeeded = false;
        boolean space = false;
        boolean skip = false;
        if (out instanceof PrintConsumer
            && ! (obj instanceof gnu.kawa.xml.UntypedAtomic)
            && ! (obj instanceof Values)
            && (getReadableOutput()
                || ! (obj instanceof Char || obj instanceof Character
                      || obj instanceof CharSequence))) {
            ((PrintConsumer) out).writeWordStart();
            space = true;
        }
        boolean removeNeeded = false;
        try {
            if (pout != null && checkSharing >= 0 && isInteresting(obj)) {
                popIDHashNeeded = pout.initialiseIDHash();
                pout.setSharing(true);
                // The value returned from this hash is the respective index in the
                // queueInts[] from PrettyWriter to which this object should reference.
                int hashIndex = pout.IDHashLookup(obj);
                int posn = pout.IDHashGetFromIndex(hashIndex);
                if (posn == -1) {
                    // Find the position in the queueInts that
                    // future (if any) backreferences should reference
                    int nposn = pout.writePositionMarker(false);
                    // Mark (hash) this object
                    pout.IDHashPutAtIndex(obj, nposn, hashIndex);
                    removeNeeded = checkSharing == 0;
                    // Print the object, instead of emitting print-circle notation
                    skip = false;
                } else {
                    // This object is referring to another part of the expression.
                    // Activate the referenced position marker
                    pout.writeBackReference(posn);
                    // This object is referring to the structure, we shall not
                    // print it and instead we shall emit print-circle notation.
                    skip = true;
                    // Format a fill space after the #N# token
                    space = true;
                }
            }
            if (!skip)
                writeObjectRaw(obj, out);
        } finally {
            if (removeNeeded)
                pout.IDHashRemove(obj);
            if (space)
                ((PrintConsumer) out).writeWordEnd();
            if (popIDHashNeeded) {
                pout.setSharing(true);
                pout.finishIDHash();
            }
        }
    }

  public void writeObjectRaw(Object obj, Consumer out)
  {
    if (! readable)
      {
        obj = Promise.force(obj);
      }
    if (obj instanceof Boolean)
      writeBoolean(((Boolean)obj).booleanValue(), out);
    else if (obj instanceof Char)
      write(((Char) obj).intValue(), out);
    else if (obj instanceof Character)
      write(((Character) obj).charValue(), out);
    else if (obj instanceof Symbol)
      {
        Symbol sym = (Symbol) obj;
        Namespace ns = sym.getNamespace();
        if (ns == XmlNamespace.HTML)
          {
            write("html:", out);
            write(sym.getLocalPart(), out);
          }
        else if (ns == LispLanguage.entityNamespace
                 || ns == LispLanguage.constructNamespace)
          {
            write(ns.getPrefix(), out);
            write(":", out);
            write(sym.getLocalPart(), out);
          }
        else
          writeSymbol(sym, out, readable);
      }
    /* #ifdef use:java.net.URI */
    /* #ifdef use:java.lang.CharSequence */
    else if (obj instanceof java.net.URI && getReadableOutput()
             && out instanceof PrintWriter)
      {
        write("#,(URI ", out);
        Strings.printQuoted(obj.toString(), (PrintWriter) out, 1);
        out.write(')');
      }
    /* #endif */
    /* #endif */
    else if
      /* #ifdef use:java.lang.CharSequence */
      (obj instanceof CharSequence) 
      /* #else */
      // (obj instanceof CharSeq || obj instanceof String) 
      /* #endif */
      {
        /* #ifdef use:java.lang.CharSequence */
	CharSequence str = (CharSequence) obj;
        /* #else */
	// String str = obj.toString();
        /* #endif */
	if (getReadableOutput () && out instanceof PrintWriter)
	  Strings.printQuoted(str, (PrintWriter) out, 1);
        else if (obj instanceof String)
          {
            out.write((String) obj);
          }
	else if (obj instanceof CharSeq)
          {
            CharSeq seq = (CharSeq) obj;
            seq.consume(0, seq.size(), out);
          }
        else
          {
            int len = str.length();
            for (int i = 0; i < len;  i++)
              out.write(str.charAt(i));
          }
      }
    else if (obj instanceof LList)
      writeList((LList) obj, out);
    else if (obj instanceof Range && getReadableOutput()) {
        Range range = (Range) obj;
        PrintConsumer.startLogicalBlock("[", false, "]", out);
        Object rstart = range.getStart();
        Object rstep = range.getStep();
        writeObject(rstart, out);
        IntNum istart = IntNum.asIntNumOrNull(rstart);
        IntNum istep = IntNum.asIntNumOrNull(rstep);
        int rsize = range.size();
        if (! range.isUnbounded() && istart != null && istep != null
            && (istep.isOne() || istep.isMinusOne())) {
            if (istep.isOne()) {
                out.write(" <: ");
                writeObject(IntNum.add(istart, rsize), out);
            } else {
                out.write(" >: ");
                writeObject(IntNum.add(istart, -rsize), out);
            }
        } else {
            out.write(" by: ");
            writeObject(rstep, out);
            if (! range.isUnbounded()) {
                out.write(" size: ");
                out.writeInt(rsize);
            }
        }
	PrintConsumer.endLogicalBlock("]", out);
    }
    else if (obj instanceof List)
      {
	List vec = (List) obj;
	String tag =
            vec instanceof SimpleVector ? ((SimpleVector) vec).getTag() : null;
	String start, end;
	if (language == 'E')
	  {
	    start = "[";
	    end = "]";
	  }
        else if ("b".equals(tag))
          {
            start = "#*";
            end = "";
          }
	else
	  {
	    start = tag == null ? "#(" : ("#" + tag + "(");
	    end = ")";
	  }
        PrintConsumer.startLogicalBlock(start, false, end, out);
        // Using consumeNext for primtives avoids boxing.
        // However, for objects we want to recurse
        if ("b".equals(tag)) {
            SimpleVector bvec = (SimpleVector) vec;
            int blen = vec.size();
            for (int i = 0; i < blen; i++) {
                boolean b = bvec.getBooleanRaw(bvec.effectiveIndex(i));
                out.write(b ? '1' : '0');
            }
        } else if (vec instanceof SimpleVector && tag != null) {
            int endpos = vec.size() << 1;
            for (int ipos = 0;  ipos < endpos;  ipos += 2) {
                if (ipos > 0)
                    PrintConsumer.writeSpaceFill(out);
                if (! ((SimpleVector) vec).consumeNext(ipos, out))
                    break;
                }
        } else {
            boolean first = true;
            for (Object el : vec) {
                if (first)
                    first = false;
                else
                    PrintConsumer.writeSpaceFill(out);
                writeObject(el, out);
            }
        }
	PrintConsumer.endLogicalBlock(end, out);
      }
    else if (obj instanceof Array)
      {
          if (!getReadableOutput () && out instanceof OutPort // FIXME PrintConsumer?
            && ((OutPort) out).atLineStart()
            && ((OutPort) out).isPrettyPrinting())
            write(ArrayPrint.print(obj, null), out);
        else
            write((Array) obj, 0, 0, out);
      }
    /* #ifdef enable:XML */
    else if (obj instanceof KNode)
      {
        boolean escapeForDomTerm = false;
        if (getReadableOutput())
          write("#", out);
        else if (CheckConsole.forDomTerm(out))
          {
            write("\033]72;", out);
            escapeForDomTerm = true;
          }
        new XMLPrinter(out).writeObject(obj);
        if (escapeForDomTerm)
          write("\007", out);
      }
    /* #endif */
    else if (obj instanceof Picture && ! getReadableOutput()
             && CheckConsole.forDomTerm(out))
    {
        write("\033]72;"+SVGUtils.toSVG((Picture) obj)+"\007", out);
    }
    else if (obj == Values.empty && getReadableOutput())
      write("#!void", out);
    else if (obj instanceof Consumable
             && (! readable || ! (obj instanceof Printable)))
      ((Consumable) obj).consume(out);
    else if (obj instanceof Printable)
      ((Printable) obj).print(out);
    else if (obj instanceof RatNum
             || (obj instanceof Number
                 && (obj instanceof Long
                     || obj instanceof Integer
                     || obj instanceof Short
                     || obj instanceof Byte
                     || obj instanceof java.math.BigInteger)))
      {
        int b = 10;
        boolean showRadix = false;
        Object base = outBase.get(null);
        Object printRadix = outRadix.get(null);
        if (printRadix != null
            && (printRadix == Boolean.TRUE
                || "yes".equals(printRadix.toString())))
          showRadix = true;
        if (base instanceof Number)
          b = ((IntNum) base).intValue();
        else if (base != null)
          b = Integer.parseInt(base.toString());
        String asString = Arithmetic.asRatNum(obj).toString(b);
        if (showRadix)
          {
            if (b == 16)
              write("#x", out);
            else if (b == 8)
              write("#o", out);
            else if (b == 2)
              write("#b", out);
            else if (b != 10 || ! (obj instanceof IntNum))
              write("#"+base+"r", out);
          }
        write(asString, out);
        if (showRadix && b == 10 && obj instanceof IntNum)
          write(".", out);
      }
    else if (obj instanceof java.lang.Enum && getReadableOutput())
      {
        write(obj.getClass().getName(), out);
        write(":", out);
        write(((java.lang.Enum) obj).name(), out);
      }
    else
      {
        String asString;
        if (obj == null)
          asString = null;
        else
          {
            Class cl = obj.getClass();
            if (cl.isArray())
              {
                int len = java.lang.reflect.Array.getLength(obj);
                PrintConsumer.startLogicalBlock("[", false, "]", out);
                for (int i = 0;  i < len;  i++)
                  {
                    if (i > 0)
                        PrintConsumer.writeSpaceFill(out);
                    writeObject(java.lang.reflect.Array.get(obj, i), out);
                  }
                PrintConsumer.endLogicalBlock("]", out);
                return;
              }
            asString = obj.toString();
          }
	if (asString == null)
	  write("#!null", out);
        else
          write(asString, out);
      }
  }

  /** Recursive helper method for writing out Array (sub-) objects.
   * @param array the Array to write out (part of).
   * @param index the row-major index to start
   * @param level the recurssion level, from 0 to array.rank()-1.
   * @param out the destination
   */
  int write(Array array, int index, int level, Consumer out)
  {
    int rank = array.rank();
    int count = 0;
    String start;
    if (level > 0)
        start = "(";
    else {
        boolean printDims = false;
        int i = rank;
        while (--i >= 0) {
            if (array.getLowBound(i) != 0 || array.getSize(i) == 0)
                break;
        }
        StringBuilder sbuf = new StringBuilder();
        sbuf.append('#');
        sbuf.append(rank);
        String tag = array instanceof GeneralArray
            ? ((GeneralArray) array).getTag()
            : null;
        sbuf.append(tag == null ? 'a' : tag);
        if (i >= 0) {
            for (i = 0; i < rank; i++) {
                int low = array.getLowBound(i);
                if (low != 0) {
                    sbuf.append('@');
                    sbuf.append(low);
                }
                sbuf.append(':');
                sbuf.append(array.getSize(i));
            }
        }
        sbuf.append(rank == 0 ? ' ' : '(');
        start = sbuf.toString();
    }
    String end = rank == 0 ? "" : ")";
    PrintConsumer.startLogicalBlock(start, false, end, out);
    if (rank > 0)
      {
	int size = array.getSize(level);
	level++;
	for (int i = 0;  i < size;  i++)
	  {
	    if (i > 0)
                PrintConsumer.writeSpaceFill(out);
	    int step;
	    if (level == rank)
	      {
		writeObject(array.getRowMajor(index), out);
		step = 1;
	      }
	    else
	      step = write(array, index, level, out);
	    index += step;
	    count += step;
	  }
      }
    else
      writeObject(array.getRowMajor(index), out);
    PrintConsumer.endLogicalBlock(end, out);
    return count;
  }

  /* #ifdef use:java.util.regex */
  static Pattern r5rsIdentifierMinusInteriorColons =
    Pattern.compile("(([a-zA-Z]|[!$%&*/:<=>?^_~])"
                    + "([a-zA-Z]|[!$%&*/<=>?^_~]|[0-9]|([-+.@]))*[:]?)"
                    + "|([-+]|[.][.][.])");
  /* #endif */

  void writeSymbol (Symbol sym, Consumer out, boolean readable)
  {
    String prefix = sym.getPrefix();
    Namespace namespace = sym.getNamespace();
    String uri = namespace == null ? null : namespace.getName();
    boolean hasUri = uri != null && uri.length() > 0;
    boolean hasPrefix = prefix != null && prefix.length() > 0;
    boolean suffixColon = false;
    if (namespace == Keyword.keywordNamespace)
      {
        if (language == 'C' || language == 'E')
          out.write(':');
        else
          suffixColon = true;
      }
    else if (hasPrefix || hasUri)
      {
        if (hasPrefix)
          writeSymbol(prefix, out, readable);
        if (hasUri && (readable || ! hasPrefix))
          {
            out.write('{');
            out.write(uri);
            out.write('}');
          }
        out.write(':');
      }
    writeSymbol(sym.getName(), out, readable);
    if (suffixColon)
      out.write(':');
  }

    void writeSymbol (String sym, Consumer out, boolean readable) {
        /* #ifdef use:java.util.regex */
        /* Use |...| if symbol doesn't follow R5RS conventions
           for identifiers or has a colon in the interior. */
        if (readable
            && ! r5rsIdentifierMinusInteriorColons.matcher(sym).matches()) {
            int len = sym.length();
            boolean r7rsStyle = true; // FIXME
            if (r7rsStyle) {
                out.write('|');
                for (int i = 0;  i < len;  i++) {
                    int ch = sym.charAt(i);
                    if (ch >= 0xD800 && ch <= 0xDBFF) { // surrogates
                        char next = sym.charAt(++i);
                        if (next >= 0xDC00 && next <= 0xDFFF)
                           ch = ((ch - 0xD800) << 10)
                               + (next - 0xDC00) + 0x10000;
                    }
                    if (ch == '|' || ch == '\\' || ch < ' ' || ch == 127) {
                        out.write('\\');
                        switch(ch) {
                        case '\007': out.write('a'); break;
                        case '\b': out.write('b'); break;
                        case '\n': out.write('n'); break;
                        case '\r': out.write('r'); break;
                        case '\t': out.write('t'); break;
                        case '\\': out.write('\\'); break;
                        case '|': out.write('|'); break;
                        default:
                            out.write('x');
                            writeHexDigits(ch, out);
                            out.write(';');
                        }
                    }
                    else
                        Char.print(ch, out);
                }
                out.write('|');
            } else if (len == 0) {
                write("||", out);
            } else {
                boolean inVerticalBars = false;
                for (int i = 0;  i < len;  i++) {
                    char ch = sym.charAt(i);
                    if (ch == '|') {
                        write(inVerticalBars ? "|\\" : "\\", out);
                        inVerticalBars = false;
                    } else if (! inVerticalBars) {
                        out.write('|');
                        inVerticalBars = true;
                    }
                    out.write(ch);
                }
                if (inVerticalBars)
                    out.write('|');
            }
            return;
        }
        /* #endif */
        write(sym, out);
    }

    static void writeHexDigits(int i, Consumer out) {
        int high = i >>> 4;
        if (high != 0) {	  
            writeHexDigits(high, out);
            i &= 15;
	}
        out.write("0123456789ABCDEF".charAt(i));
    }
  
  /**
   * An "interesting" object is one where object identity is significant.
   *
   * Examples are vectors and lists.
   * No immutable values (for example symbols or numbers) are interesting
   * @param obj The object under test
   * @return true if this object is considered interesting
   */
  private boolean isInteresting (Object obj)
  {
    // FIXME Should probably consider empty lists and vectors, as well as FStrings
    return obj instanceof Pair || obj instanceof SimpleVector;
  }
}
