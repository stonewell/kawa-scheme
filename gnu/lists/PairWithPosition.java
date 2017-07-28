package gnu.lists;
import java.io.*;
import gnu.text.SourceLocator;
import gnu.text.SourceMapper;

/** A <code>Pair</code> with the file name and position it was read from.
 * Note the position (start/end range) is actually that of the car part. */

public class PairWithPosition extends ImmutablePair
  implements gnu.text.SourceLocator
{
    String filename; // Future: union(String,SourceMapper)
    long position; // See SourceMapper#simpleEncode

  public final void setFile (String filename)
  {
    this.filename = filename;
  }

    public final void setLine(int lineno, int colno) {
        position = SourceMapper.simpleEncode(lineno, colno);
    }

    public final void setEndLine(int endline, int endcolumn) {
        position = SourceMapper.simpleEncode(getStartLine(), getStartColumn(),
                                             endline, endcolumn);
    }

  public final void setLine (int lineno)
  {
    setLine (lineno, 0);
  }

  public final String getFileName ()
  {
    return filename;
  }

  public String getPublicId ()
  {
    return null;
  }

  public String getSystemId ()
  {
    return filename;
  }

    public final int getLineNumber() {
        return SourceMapper.simpleStartLine(position);
    }

    public final int getColumnNumber() {
        return SourceMapper.simpleStartColumn(position);
    }

    public final int getStartLine() {
        return SourceMapper.simpleStartLine(position);
    }

    public final int getStartColumn() {
        return SourceMapper.simpleStartColumn(position);
    }

    public final int getEndLine() {
        return SourceMapper.simpleEndLine(position);
    }

    public final int getEndColumn() {
        return SourceMapper.simpleEndColumn(position);
    }

  public boolean isStableSourceLocation() { return true; }

  /** Only for serialization. */
  public PairWithPosition ()
  {
  }

  public PairWithPosition (SourceLocator where,
                           Object car, Object cdr)
  {
    super (car, cdr);
    filename = where.getFileName();
    this.position = SourceMapper.simpleEncode(where.getStartLine(), where.getStartColumn(),
                              where.getEndLine(), where.getEndColumn());
  }

  public PairWithPosition (Object car, Object cdr)
  {
    super (car, cdr);
  }

  public static PairWithPosition make(Object car, Object cdr,
				      String filename, int line, int column)
  {
    PairWithPosition pair = new PairWithPosition(car, cdr);
    pair.filename = filename;
    pair.setLine(line, column);
    return pair;
  }

  public static PairWithPosition make(Object car, Object cdr,
				      String filename, long position)
  {
    PairWithPosition pair = new PairWithPosition(car, cdr);
    pair.filename = filename;
    pair.position = position;
    return pair;
  }

    /** Should only be used when initializing a PairWithPosition instance. */
    public void init(Object car, Object cdr,
                     String filename, long position) {
        this.car = car;
        this.cdr = cdr;
        this.filename = filename;
        this.position = position;
    }

  /**
   * @serialData Write the car followed by the cdr,
   *   followed by filename (as an {@code Object}, so it can be shared),
   *   followed by position (see SourceMapper#simpleEncode}.
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(car);
    out.writeObject(cdr);
    out.writeObject(filename);
    out.writeLong(position);
  }

    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        Object car = in.readObject();
        Object cdr = in.readObject();
        String filename = (String) in.readObject();
        long position = in.readLong();
        init(car, cdr, filename, position);
    }
}
