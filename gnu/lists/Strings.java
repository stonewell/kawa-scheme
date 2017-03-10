// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import gnu.text.Char;
import java.io.IOException;

/** Various static utility methods for general strings (CharSeqs). */

public class Strings
{
    /** Get character (code point) at a offset.
     * @param index offset measured in 16-bit code units
     */
    public static int characterAt(CharSequence cseq, int index) {
        return characterAt(cseq, 0, cseq.length(), index);
    }
    /** Get character (code point) at a offset.
     * @param index offset measured in 16-bit code units,
     * from begining of cseq, not frm start
     */
    public static int characterAt(CharSequence cseq, int start, int end,
                                  int index) {
        if (index < start || index >= end)
            throw new IndexOutOfBoundsException();
        char ch1 = cseq.charAt(index);
        if (ch1 >= 0xD800 && ch1 <= 0xDBFF) {
            if (index + 1 < end) {
                char ch2 = cseq.charAt(index+1);
                if (ch2 >= 0xDC00 && ch2 <= 0xDFFF)
                    return ((ch1 - 0xD800) << 10) + (ch2 - 0xDC00) + 0x10000;
            }
        } else if (ch1 >= 0xDC00 && ch1 <= 0xDFFF) {
            if (index > start) {
                char ch0 = cseq.charAt(index-1);
                if (ch0 >= 0xD800 && ch0 <= 0xDBFF)
                    return Char.IGNORABLE_CHAR;
            }
        }
        return ch1;
    }
    /** Get index'th character (code point).
     * @param index offset by code points
     */
    public static int indexByCodePoints(CharSequence str, int index) {
        if (str instanceof IString)
            return ((IString) str).indexByCodePoints(index);
        index = Character.offsetByCodePoints(str, 0, index);
        return Character.codePointAt(str, index);
    }

    /** Like offsetByCodePoints, but optimize if an IString.
     * @param offset number of code points beyond start index.
     * @param cuStart start index in code units (Java chars)
     * @param cpStart start index in Unicode code points
     */
    public static int offsetByCodePoints(CharSequence str, int offset,
                                         int cuStart, int cpStart) {
        if (str instanceof IString)
            return ((IString) str).offsetByCodePoints(cpStart+offset);
        return Character.offsetByCodePoints(str, cuStart, offset);
    }

    public static int sizeInCodePoints(CharSequence str) {
        if (str instanceof IString)
            return ((IString) str).lengthByCodePoints();
        int len = str.length();
        int nsurr = 0;
        for (int i = 0; i < len;  ) {
            char ch = str.charAt(i++);
            if (ch >= 0xD800 && ch <= 0xDBFF && i < len) {
                int next = str.charAt(i);
                if (next >= 0xDC00 && next <= 0xDFFF) {
                    i++;
                    nsurr++;
                }
            }
        }
        return len-nsurr;
    }

  /** Change every character to be uppercase. */
  public static void makeUpperCase(CharSeq str)
  {
    for (int i = str.length();  --i >= 0; )
      str.setCharAt(i, Character.toUpperCase(str.charAt(i)));
  }

  /** Change every character to be lowercase. */
  public static void makeLowerCase(CharSeq str)
  {
    for (int i = str.length();  --i >= 0; )
      str.setCharAt(i, Character.toLowerCase(str.charAt(i)));
  }

  /** Capitalize this string.
   * Change first character of each word to titlecase,
   * and change the other characters to lowercase. */
  public static void makeCapitalize(CharSeq str)
  {
    char prev = ' ';
    int len = str.length();
    for (int i = 0;  i < len;  i++)
      {
	char ch = str.charAt(i);
	if (! Character.isLetterOrDigit(prev))
	  ch = Character.toTitleCase(ch); 
        else 
          ch = Character.toLowerCase(ch);
	str.setCharAt(i, ch);
	prev = ch;
      }
  }

    public static String toJson(CharSequence str) {
        StringBuilder sbuf = new StringBuilder();
        printQuoted(str, sbuf, 3);
        return sbuf.toString();
    }

    public static void printJson(CharSequence str, Appendable ps) {
        printQuoted(str, ps, 3);
    }

    /** Print a string with quotes and escapes.
     * @param escapes The value 0 means only escape '"' and '\\';
     *   the value 1 means escape standard escape characters like '\\b';
     *   the value 2 means escape all non-ascii or control characters;
     *   the value 3 means follow the JSON standard.
     */
    public static void printQuoted(CharSequence str,
                                   Appendable ps, int escapes) {
        int len = str.length();
        try {
            ps.append('\"');
            for (int i = 0;  i < len; i++) {
                char ch = str.charAt(i);
                if ((ch == '\\' || ch == '\"'))
                    ps.append('\\');
                else if (escapes > 0) {
                    // These escapes are R6RS:
                    if (ch == '\n')
                    { ps.append("\\n"); continue; }
                    else if (ch == '\r')
                    { ps.append("\\r"); continue; }
                    else if (ch == '\t')
                    { ps.append("\\t"); continue; }
                    else if (ch == '\007' && escapes < 3)
                    { ps.append("\\a"); continue; }
                    else if (ch == '\b')
                    { ps.append("\\b"); continue; }
                    else if (ch == '\013' && escapes < 3)
                    { ps.append("\\v"); continue; }
                    else if (ch == '\f')
                    { ps.append("\\f"); continue; }
                    else if (escapes >= 3 && (ch < ' ' || ch >= 127))
                    {
                        ps.append("\\u");
                        int d = ch;
                        for (int k = 12; k >= 0; k -= 4) {
                            ps.append(Character.forDigit((d >> k) & 15, 16));
                        }
                        continue;
                    }
                    else if (ch < ' ' || (escapes > 1 && ch >= 127))
                    {
                        ps.append("\\x");
                        ps.append(Integer.toHexString(ch));
                        ps.append(';');
                        continue;
                    }
                }
                ps.append(ch);
            }
            ps.append('\"');
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    public static void copyInto(CharSequence src, int start, int end,
                                CharSeq dst, int at) {
        int dstLen = dst.length();
        int srcLen = src.length();
        if (at < 0 || at > dstLen || start < 0 || end > srcLen || end < start
            || dstLen - at < end - start)
            throw new StringIndexOutOfBoundsException();
        if (at < start) {
            int i = at;
            int j = start;
            for (; j < end; i++, j++) {
                dst.setCharAt(i, src.charAt(j));
            }
        }
        else {
            int i = at + end - start;
            int j = end;
            while (--j >= start) {
                dst.setCharAt(--i, src.charAt(j));
            }
        }
    }

    /** Make a read-only substring, generalized to arbitrary index sequences.
     * The indexes are in terms of code points (character) offsets.
     */
    public static IString indirectIndexed(CharSequence base,
                                               IntSequence indexes) {
        if (indexes instanceof Range.IntRange) {
            Range.IntRange range = (Range.IntRange) indexes;
            if (range.getStepInt() == 1) {
                int start = range.getStartInt();
                int end = base.length();
                if (start < 0 || start > end)
                    throw new IndexOutOfBoundsException();
                int size;
                if (! range.isUnbounded()) {
                    size = range.size();
                    if (start+size < 0 || start+size > end)
                        throw new IndexOutOfBoundsException();
                } else
                    size = end - start;
                return IString.valueOf(base, start, size);
            }
        }
        int len = indexes.size();
        StringBuilder sbuf = new StringBuilder(len);
        for (int i = 0; i < len; i++) {
            int ch = Strings.indexByCodePoints(base, indexes.getInt(i));
            if (ch >= 0x10000) {
                sbuf.append((char) (((ch - 0x10000) >> 10) + 0xD800));
                ch = (ch & 0x3FF) + 0xDC00;
            }
            sbuf.append((char) ch);
        }
        return new IString(sbuf.toString());
    }

    /** Make a read-only substring.
     * The start and end are in terms of code unit (16-bit char).
     */
    public static CharSequence substring(CharSequence base,
                                         int start, int end) {
        if (base instanceof FString) {
            FString fstr = (FString) base;
            if (fstr.isVerySimple() || fstr.isSubRange())
                return (CharSequence) Sequences.copySimple(fstr, start, end, false);
        }
        if (base instanceof String) {
            return ((String) base).substring(start, end);
        } else {
            int len = end - start;
            StringBuilder sbuf = new StringBuilder(len);
            if (base instanceof CharSeq) {
                try {
                    ((CharSeq) base).writeTo(start, len, sbuf);
                } catch (Throwable ex) {
                    throw new RuntimeException(ex);
                }
            } else {
                for (int i = start; i < end; i++)
                    sbuf.append(base.charAt(i));
            }
            return sbuf.toString();
        }
    }

    public static String toUtf8(byte[] bytes, int start, int length) {
        /* #ifdef JAVA7 */  
        return new String(bytes, start, length, java.nio.charset.StandardCharsets.UTF_8);
        /* #else */
        // try {
        //   return new String(bytes, start, length, "UTF-8");
        // } catch (java.io.UnsupportedEncodingException ex) {
        //     throw new RuntimeException(ex);
        // }
        /* #endif */
    }

    public static int compareTo(CharSequence str1, CharSequence str2) {
        int n1 = str1.length();
        int n2 = str2.length();
        int n = n1 > n2 ? n2 : n1;
        for (int i = 0; i < n; i++) {
            char c1 = str1.charAt(i);
            char c2 = str2.charAt(i);
            int d = c1 - c2;
            if (d != 0)
                return d;
        }
        return n1 - n2;
    }
}
