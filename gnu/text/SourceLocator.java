package gnu.text;

public interface SourceLocator
  /* #ifdef SAX2 */
  extends
  /* #ifdef use:javax.xml.transform */
  javax.xml.transform.SourceLocator, 
  /* #endif */
  org.xml.sax.Locator
  /* #endif */
{
    /** Return current line number.
     * Normally the same as {@code getStartLine()}.
     * The "first" line is line 1; unknown is -1. */
    public int getLineNumber();

    /** Return current column number.
     * Normally the same as {@code getStartColumn()}.
     * The "first" column is column 1; unknown is -1. */
    public int getColumnNumber();

    /** Line number (one-origin) of start of range; unknown/unspecified is -1. */
    public int getStartLine();
    /** Column (one-origin) of start of range; unknown/unspecified is -1. */
    public int getStartColumn();
    /** Line number (one-origin) of end of range; unknown/unspecified is -1. */
    public int getEndLine();
    /** Column (one-origin) of end of range; unknown/unspecified is -1. */
    public int getEndColumn();

  public String getPublicId();

  public String getSystemId();
  /** Normally same as getSystemId. */
  public String getFileName();

  /** True if position is unlikely to change.
   * True for an expression but not an input file. */
  public boolean isStableSourceLocation();

    public static class Simple implements SourceLocator {
        protected String filename;
        protected long position;

        public String getFileName() {
            return filename;
        }

        public String getPublicId() {
            return null;
        }

        public String getSystemId() {
            return filename;
        }

        public int getLineNumber() {
            return SourceMapper.simpleStartLine(position);
        }

        public int getColumnNumber() {
            return SourceMapper.simpleStartColumn(position);
        }

        public int getStartLine() {
            return SourceMapper.simpleStartLine(position);
        }

        public int getStartColumn() {
            return SourceMapper.simpleStartColumn(position);
        }

        public int getEndLine() {
            return SourceMapper.simpleEndLine(position);
        }

        public int getEndColumn() {
            return SourceMapper.simpleEndColumn(position);
        }

        public boolean isStableSourceLocation() { return true; }

        public void setFile(String filename) {
            this.filename = filename;
        }
        public void setLine(String filename, int line, int column) {
            setFile(filename);
            setLine(line, column);
        }

        public void setLine(int lineno, int colno) {
            position = SourceMapper.simpleEncode(lineno, colno);
        }

        public void setLine(int lineno) {
            setLine (lineno, 0);
        }

        public void setLocation(SourceLocator location) {
            this.filename = location.getFileName();
            this.position = SourceMapper.simpleEncode(location);
        }
    }
}
