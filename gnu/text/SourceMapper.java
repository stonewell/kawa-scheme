package gnu.text;

/** Helpers for managing source-code positions and ranges.
 * FUTURE: An object that decodes source positions, encoded as longs.
 * The default encoding gives start-line and end-line both 20 bits,
 * and start-column and end-column both 12 bits.
 */

public class SourceMapper {
    public static long simpleEncode(SourceLocator location) {
        return location instanceof SourceLocator.Simple
            ? ((SourceLocator.Simple) location).position
            : SourceMapper
            .simpleEncode(location.getStartLine(), location.getStartColumn(),
                          location.getEndLine(), location.getEndColumn());
    }

    public static long simpleEncode(int startLine, int startColumn,
                                    int endLine, int endColumn) {
        if (startLine < 0 || startLine > 0xfffff) startLine = 0;
        if (endLine < 0 || endLine > 0xfffff) endLine = 0;
        if (startColumn < 0 || startColumn > 0xfff) startColumn = 0;
        if (endColumn < 0 || endColumn > 0xfff) endColumn = 0;
        return ((long) endLine << 44) | ((long) endColumn << 32)
                 | ((long) startLine << 12) | ((long) startColumn);
    }

    public static long simpleEncode(int startLine, int startColumn) {
        if (startLine < 0 || startLine > 0xfffff) startLine = 0;
        if (startColumn < 0 || startColumn > 0xfff) startColumn = 0;
        return ((long) startLine << 12) | ((long) startColumn);
    }

    public static int simpleStartLine(long position) {
        int line = (int) (position >> 12) & 0xfffff;
        return  line == 0 ? -1 : line;
    }

    public static int simpleStartColumn(long position) {
        int column = (int) position & 0xfff;
        return column == 0 ? -1 : column;
    }

    public static int simpleEndLine(long position) {
        int line = (int) (position >> 44) & 0xfffff;
        return line == 0 ? simpleStartLine(position) : line;
    }

    public static int simpleEndColumn(long position) {
        int column = (int) (position >> 32) & 0xfff;
        return column == 0 ? simpleStartColumn(position) : column;
    }

    /* MAYBE FUTURE:
    private String filename;

    public static SourceMapper simpleMapper(String filename) {
        SourceMapper mapper = new SourceMapper();
        mapper.filename = filename;
        return mapper;
    }
    public String getFileName(long range) { return filename; }

    public int getStartLine(long range) { return simpleStartLine(range); }
    public int getStartColumn(long range) { return simpleStartColumn(range); }

    public int getEndLine(long range) { return simpleEndLine(range); }
    public int getEndColumn(long range) { return simpleEndColumn(range); }

    public int getLineNumber(long range) { return getStartLine(range); }
    public int getColumnNumber(long range) { return getStartColumn(range); }

    public static String getFileName(Object mapper, long range) {
        if (mapper instanceof String)
            return (String) mapper;
        else
            return ((SourceMapper) mapper).getFileName(range);  
    }
    public static int getLineNumber(Object mapper, long range) {
         if (mapper instanceof String)
             return getStartLine(mapper, range);
         else
             return ((SourceMapper) mapper).getLineNumber(range);
    }

    public static int getColumnNumber(Object mapper, long range) {
         if (mapper instanceof String)
             return getStartColumn(mapper, range);
         else
             return ((SourceMapper) mapper).getColumnNumber(range);
    }

    public static int getStartLine(Object mapper, long range) {
        return (mapper instanceof String ? simpleStartLine(range)
                : ((SourceMapper) mapper).getStartLine(range));
    }

    public static int getEndLine(Object mapper, long range) {
        if (mapper instanceof String)
            return getStartLine(mapper, range);
        return ((SourceMapper) mapper).getEndLine(range);
    }

    public static int getStartColumn(Object mapper, long range) {
        return mapper instanceof String ? simpleStartColumn(range)
                    : ((SourceMapper) mapper).getStartColumn(range);
    }

    public static int getEndColumn(Object mapper, long range) {
        return mapper instanceof String ? simpleEndColumn(range)
            : ((SourceMapper) mapper).getEndColumn(range);
    }
    */
}
