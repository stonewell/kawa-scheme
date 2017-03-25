// Copyright (c) 2001, 2004, 2017  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import gnu.text.Char;
import java.io.IOException;
import java.io.Writer;

/** Simple adjustable-length vector whose elements are 32-bit code points
 * Used for the Scheme string type.
 * This isn't a regular SimpleVector because character indexing
 * isn't a simple lookup.
 * "Sub-range mode" is not used (at least not for Kawa Scheme):
 * if you need an immutable sub-string, use an IString (or java.lang.String).
 * @author Per Bothner
 */

public class FString extends AbstractCharVector<Char>
    implements Appendable, CharSeq, Consumable
{
    public FString() {
        data = empty;
    }

    public FString(int num) {
        data = new char[num];
    }

    public FString(int num, int value) {
        data = new char[value < 0x10000 ? num : 2 * num];
        if (value != 0 && num != 0)
            insertRepeatedRaw(0, value, num);
    }

    /** Create an FString from a char[].
     * Note that this contructor does *not* copy the argument. */
    public FString(char[] values) {
        data = values;
    }

    /** This constructor makes a copy. */
    public FString(char[] buffer, int offset, int length) {
        data = new char[length];
        System.arraycopy(buffer, offset, data, 0, length);
    }

    public FString(CharSequence seq) {
        this(seq, 0, seq.length());
    }

    /** Copy a substring of a CharSequence.
     * @param offset - start offset in 16-bit char units
     * @param length - length in 16-bit char units
     */
    public FString(CharSequence seq, int offset, int length) {
        char[] data = new char[length];
        if (seq instanceof CharSeq)
            ((CharSeq) seq).getChars(offset, offset+length, data, 0);
        else if (seq instanceof String || seq instanceof IString) {
            seq.toString().getChars(offset, offset+length, data, 0);
            this.data = data;
        }
        else if (seq instanceof StringBuilder)
            ((StringBuilder) seq).getChars(offset, offset+length, data, 0);
        else if (seq instanceof StringBuffer)
            ((StringBuffer) seq).getChars(offset, offset+length, data, 0);
        else {
            for (int i = length; --i >= 0; )
                data[i] = seq.charAt(offset+i);
        }
        this.data = data;
    }

    @Override
    public int size() { return Strings.sizeInCodePoints(this); }

    /** @param index measured in Unicode code points (characters)
     */
    @Override
    public int effectiveIndex(int index) {
        return relativeIndex(0, index);
    }

    private int relativeIndex(int start, int offset) {
        if (isSubRange()) throw new InternalError();
        int eff = start;
        char prev = 0;
        int dlen = data.length;
        boolean beforeGap = ! isVerySimple();
        int limit = beforeGap ? getGapStart() : dlen;
        for (;;) {
            if (eff >= limit && beforeGap) {
                eff = getGapEnd();
                limit = dlen;
                beforeGap = false;
            }
            if (offset == 0)
                return eff;
            char ch = data[eff];
            eff++;
            if (! (prev >= 0xD800 && prev <= 0xDBFF
                   && ch >= 0xDC00 && ch <= 0xDFFF))
                offset--;
            prev = ch;
        }
    }

    @Override
    public int createPos (int index, boolean isAfter) {
        int eff = effectiveIndex(index);
        // createPos ignores buffer gap; effectiveIndex includes it.
        // (At least that is what we do for other SimpleVector types.)
        if (isGapBuffer() && eff >= getGapEnd())
            eff -= getGapSize();
        return (eff << 1) | (isAfter ? 1 : 0);
    }

    @Override
    protected int nextIndex(int ipos) {
        int dlen = data.length;
        int pos = ipos == -1 ? dlen : ipos >>> 1;
        if (isGapBuffer() && pos >= getGapStart())
            pos += getGapSize();
        int eff = 0;
        char prev = 0;
        int index = 0;
        boolean beforeGap = ! isVerySimple();
        int limit = beforeGap ? getGapStart() : dlen;
        for (;;) {
            if (eff >= limit && beforeGap) {
                eff = getGapEnd();
                limit = dlen;
                beforeGap = false;
            }
            if (eff == pos) {
                return index;
            }
            char ch = data[eff];
            eff++;
            if (! (prev >= 0xD800 && prev <= 0xDBFF
                   && ch >= 0xDC00 && ch <= 0xDFFF))
                index++;
            prev = ch;
        }
    }
    public int createRelativePos(int pos, int delta, boolean isAfter) {
        if (delta >= 0 && pos >= 0) {
            return (relativeIndex(pos >>> 1, delta) << 1) | (isAfter ? 1 : 0);
        }
        return super.createRelativePos(pos, delta, isAfter);
    }

    /** Create a empty string, but with a given initial buffer size. */
    public static FString alloc(int sz) {
        if (sz > MAX_GAP_SIZE)
            sz = MAX_GAP_SIZE;
        FString str = new FString(sz);
        str.setGapBounds(0, sz);
        return str;
    }

    public final Char getRaw(int index) {
        int ch1 = data[index];
        if (ch1 >= 0xD800 && ch1 <= 0xDBFF) {
            char ch2 = data[index+1];
            if (ch2 >= 0xDC00 && ch2 <= 0xDFFF)
                ch1 = ((ch1 - 0xD800) << 10) + (ch2 - 0xDC00) + 0x10000;
        }
        return Char.valueOf(ch1);
    }

    /** @param index offset in character units (Unicode code points) */
    public final Char get(int index) {
        return Char.valueOf(Strings.indexByCodePoints(this, index));
    }

    /** @param fromChar offset in 16-bit code units */
    public int indexOf(int ch, int fromChar) {
        char c1, c2;
        if (ch >= 0x10000) {
            c1 = (char) (((ch - 0x10000) >> 10) + 0xD800);
            c2 = (char) ((ch & 0x3FF) + 0xDC00);
        } else {
            c1 = 0;
            c2 = (char) ch;
        }
        int sz = length();
        char prev = 0;
        for (int i = fromChar;  i < sz; i++) {
            char cur = charAt(i);
            if (cur == c2) {
                if (c1 == 0)
                    return i;
                if (prev == c1)
                    return i-1;
            }
            prev = cur;
        }
        return -1;
    }

    /** @param fromChar offset in 16-bit code units */
    public int lastIndexOf(int ch, int fromChar) {
        char c1, c2;
        if (ch >= 0x10000) {
            c1 = (char) (((ch - 0x10000) >> 10) + 0xD800);
            c2 = (char) ((ch & 0x3FF) + 0xDC00);
        } else {
            c1 = 0;
            c2 = (char) ch;
        }
        for (int i = fromChar; --i >= 0; ) {
            if (charAt(i) == c2) {
                if (c1 == 0)
                    return i;
                if (i > 0 && charAt(i-1) == c1)
                    return i - 1;
            }
        }
        return -1;
    }

    /** @param index offset in character units (Unicode code points) */
    public Char set(int index, Char value) {
        checkCanWrite();
        index = Character.offsetByCodePoints(this, 0, index);
        Char old = Char.valueOf(characterAt(index));
        setCharacterAt(index, value.intValue());
        return old;
    }

    /** @param index offset in character units (Unicode code points) */
    @Override
    public final void setRaw(int index, Char value) {
        index = Character.offsetByCodePoints(this, 0, index);
        setCharacterAt(index, value.intValue());
    }

    /** @param index offset in 16-bit code units */
    public final int characterAt(int index) {
        // The following uses charAt, which handles adjusting for indexes.
        return Strings.characterAt(this, 0, length(), index);
    }

  /** Return a char[] contain the characters of this string.
   * It is unspecified if the result is a copy or shares with this FString.
   */
  public char[] toCharArray()
  {
    if (isVerySimple())
        return data;
    int seq_length = length();
    char[] arr = new char[seq_length];
    for (int i = 0;  i < seq_length;  i++)
        arr[i] = charAt(i);
    return arr;
  }

  public void shift(int srcStart, int dstStart, int count)
  {
    System.arraycopy(data, srcStart, data, dstStart, count);
  }

  public FString copy (int start, int end)
  {
    char[] copy = new char[end-start];
    char[] src = data; // Move to local to help optimizer.
    for (int i = start;  i < end;  i++)
      copy[i-start] = src[i];
    return new FString(copy);
  }

  public boolean addAll (CharSequence s)
  {
    int ssize = s.length();
    int sz = length();
    addSpace(sz, ssize); 
    if (s instanceof String)
      ((String) s).getChars(0, ssize, data, sz);
    else if (s instanceof CharSeq)
      ((CharSeq) s).getChars(0, ssize, data, sz);
    else
      for (int i = ssize; --i >= 0; )
        data[sz+i] = s.charAt(i);
    return ssize > 0;
  }

    public void insert(int where, int ch, boolean beforeMarkers) {
        int len;
        char c1, c2;
        if (ch >= 0x10000) {
            c1 = (char) (((ch - 0x10000) >> 10) + 0xD800);
            c2 = (char) ((ch & 0x3FF) + 0xDC00);
            len = 2;
        } else {
            c1 = (char) ch;
            c2 = 0;
            len = 1;
        }
        addSpace(where, len);
        data[where] = c1;
        if (c2 > 0)
            data[where+1] = c2;
    }

    public void insert(int where, String str, boolean beforeMarkers) {
        int len = str.length();
        addSpace(where, len);
        str.getChars(0, len, data, where);
    }

  /** Append arguments to this FString.
   * Used to implement Scheme's string-append.
   * @param args an array of FString value
   * @param startIndex index of first string in <code>args</code> to use
   */
  public void addAllStrings(Object[] args, int startIndex)
  {
    int sz = length();
    int count = 0;
    for (int i = startIndex; i < args.length; ++i)
      {
        Object arg = args[i];
        count += ((CharSequence) arg).length();
      }
    //if (data.length < total)      copyBuffer(total);
    gapReserve(sz, count);
    
    for (int i = startIndex; i < args.length; ++i)
      {
        addAll((CharSequence) args[i]);
      }
  }
  
    public String toString() {
        return substring(0, length());
    }

    public String substring(int start, int end) {
        if (isVerySimple())
            return new String(data, start, end - start);
        // FIXME aybe also optimize isSubRange() case
        else
            return new StringBuilder().append(this, start, end).toString();
    }

  public CharSeq subSequence(int start, int end)
  {
    // FIXME should maybe share?  Or use Strings.substring?
    return new FString(this, start, end-start);
  }

    public void setCharAt(int index, char ch) {
        checkCanWrite(); // FIXME maybe inline and fold into following
        data[super.effectiveIndex(index)] = ch;
    }

    public void setCharacterAt(int index, int ch) {
        int sz = length();
        if (index < 0 || index >= sz)
            throw new StringIndexOutOfBoundsException(index);
        char old1 = charAt(index);
        char old2;
        boolean oldIsSupp = old1 >= 0xD800 && old1 <= 0xDBFF
            && index+1 < sz
            && (old2 = charAt(index+1)) >= 0xDC00 && old2 <= 0xDFFF;
        if (ch <= 0xFFFF) {
            if (oldIsSupp)
                delete(index+1, index+2);
            setCharAt(index, (char) ch);
        } else {
            char c1 = (char) (((ch - 0x10000) >> 10) + 0xD800);
            char c2 = (char) ((ch & 0x3FF) + 0xDC00);
            setCharAt(index, c1);
            if (oldIsSupp) {
                setCharAt(index+1, c2);
            } else {
                insert(index+1, c2, true);
            }
        }
    }

    /** Replace a substring of this string with another.
     * The two strings may have different lengths, so this
     * generalizes insertion and deletion.
     * All indexes are code-unit (16-bit char) offsets.
     */
    public void replace(CharSequence src, int srcStart, int srcEnd,
                        int dstStart, int dstEnd) {
        if (dstStart < 0 || dstStart > dstEnd || dstEnd > length()
            || srcStart < 0 || srcStart > srcEnd || srcEnd > src.length())
            throw new StringIndexOutOfBoundsException();
        int srcLength = srcEnd - srcStart;
        int dstLength = dstEnd - dstStart;
        int grow = srcLength - dstLength;
        if (grow > 0) {
            gapReserve(dstEnd, grow);
        }
        if (src instanceof FString) {
            FString fsrc = (FString) src;
            int sstart = fsrc.getSegmentReadOnly(srcStart, srcLength);
            if (sstart >= 0) {
                System.arraycopy(fsrc.data, sstart, data, dstStart, srcLength);
                if (grow < 0)
                    delete(dstEnd+grow, dstEnd);
                else if (grow > 0)
                    setGapBounds(getGapStart() + grow, getGapEnd());
                return;
            }
        }
        if (! Sequences.copyInPlaceIsSafe(this, src)) {
            src = src.subSequence(srcStart, srcEnd).toString();
            srcEnd = srcLength;
            srcStart = 0;
        }
        if (grow < 0)
            delete(dstEnd+grow, dstEnd);
        else if (grow > 0)
            setGapBounds(getGapStart()+grow, getGapEnd());
        int i = dstStart;
        int j = srcStart;
        for (; j < srcEnd; i++, j++) {
            data[i] = src.charAt(j);
        }
    }

  public void setCharAtBuffer (int index, char ch)
  {
    data[index] = ch;
  }

    public void insertRepeated(int where, int value, int count) {
        addSpace(where, value < 0x10000 ? count : 2 * count);
        insertRepeatedRaw(where, value, count);
    }
    private void insertRepeatedRaw(int where, int value, int count) {
        char c1, c2;
        int len;
        if (value >= 0x10000) {
            c1 = (char) (((value - 0x10000) >> 10) + 0xD800);
            c2 = (char) ((value & 0x3FF) + 0xDC00);
            len = 2 * count;
        } else {
            c1 = (char) value;
            c2 = 0;
            len = count;
        }
        char[] array = data;
        int end = where + len;
        for (int i = where;  i < end;  ) {
            array[i++] = c1;
            if (c2 != 0)
                array[where + i++] = c2;
        }
    }

  public void replace(int where, char[] chars, int start, int count)
  {
    System.arraycopy(chars, start, data, where, count);
  }

  public void replace(int where, String string)
  {
    string.getChars(0, string.length(), data, where);
  }

    public boolean equals(Object obj) {
        return obj instanceof FString && equals(this, (FString) obj);
    }

    @Override
    protected FString newInstance(int newLength) {
        return new FString(newLength < 0 ? data : new char[newLength]);
    }

  public int getElementKind()
  {
    return CHAR_VALUE;
  }

    public String getTag() { return "c32"; }

    public void consumePosRange(int iposStart, int iposEnd, Consumer out) {
        if (out.ignoring())
            return;
        int i = iposStart >>> 3;
        int end = iposEnd == -1 ? length() : iposEnd >>> 1;
        while (i < end) {
            long result = getSegment(i);
            int where = (int) result;
            int size = (int) (result >> 32);
            out.write(data, where, size);
            i += size;
        }
    }

    public FString append(char c) {
        int sz = length();
        addSpace(sz, 1);
        char[] d = data;
        d[sz] = c;
        return this;
    }

    /** Append a Unicode code point. */
    public FString appendCharacter(int c) {
        int delta;
        if (c < 0x10000)
            delta = 1;
        else if (c == Char.IGNORABLE_CHAR)
            return this;
        else
            delta = 2;
        int sz = length();
        addSpace(sz, delta);
        char[] d = data;
        if (delta > 1) {
            d[sz++] = (char) (((c - 0x10000) >> 10) + 0xD800);
            c = (c & 0x3FF) + 0xDC00;
        }
        d[sz++] = (char) c;
        return this;
    }

    public FString prependCharacter(int c) {
        int delta;
        if (c < 0x10000)
            delta = 1;
        else if (c == Char.IGNORABLE_CHAR)
            return this;
        else
            delta = 2;
        int sz = length();
        gapReserve(0, delta);
        int p = getGapEnd()-delta;
        setGapBounds(getGapStart(), p);
        char[] d = data;
        if (delta > 1) {
            d[p++] = (char) (((c - 0x10000) >> 10) + 0xD800);
            c = (c & 0x3FF) + 0xDC00;
        }
        d[p++] = (char) c;
        return this;
    }

    public FString append(CharSequence csq) {
        if (csq == null)
            csq = "null";
        return append(csq, 0, csq.length());
    }

    public FString append(CharSequence csq, int start, int end) {
        if (csq == null)
            csq = "null";
        int len = end - start;
        int sz = length();
        addSpace(sz, len);
        char[] d = data;
        if (csq instanceof String)
            ((String) csq).getChars(start, end, d, sz);
        else if (csq instanceof CharSeq)
            ((CharSeq) csq).getChars(start, end, d, sz);
        else {
            int j = sz;
            for (int i = start; i < end;  i++)
                d[j++] = csq.charAt(i);;
        }
        return this;
    }

    public FString append(Object obj) {
        if (obj instanceof gnu.text.Char)
            appendCharacter(((gnu.text.Char) obj).intValue());
        else if (obj instanceof java.lang.Character)
            appendCharacter(((java.lang.Character) obj).charValue());
        else
            append(obj.toString());
        return this;
    }

    public void writeTo(int start, int count, Appendable dest)
        throws IOException {
        if (dest instanceof Writer) {
            Writer wr = (Writer) dest;
            while (count > 0) {
                long result = getSegment(start);
                int where = (int) result;
                int size = (int) (result >> 32);
                if (size > count)
                    size = count;
                wr.write(data, where, size);
                start += size;
                count -= size;
            }
        }
        else
            dest.append(this, start, start+count);
    }

    public void writeTo(Appendable dest) throws IOException {
        writeTo(0, length(), dest);
    }
}
