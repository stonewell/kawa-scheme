package gnu.lists;

import java.io.*;

// FIXME should also implement CharSeq, Consumable
// maybe also AVector<Char>, RandomAccess
// FIXME should extend AbstractSequence<gnu.text.Char>
//    of perhaps (unlikely) extend SimpleVector<gnu.text.Char>

/** A string implementation with contant-time codepoint indexing.
 * Suitable for SRFI-135 or SRFI-140.
 */
public class IString
    implements CharSequence, Externalizable, Comparable<IString> {
    String str;
    int cplength; // number of codepoints

    /* Index of every 16-th character.
     * Null if all characters are in the basic plane. */
    int[] offsets;
    private static int[] NO_OFFSETS = { };

    private static final int INDEX_STEP = 16;
    private static final int INDEX_STEP_LOG = 4;
    private static final int numSteps(int len) {
        return len >> INDEX_STEP_LOG;
    }
    private static final int restStep(int i) {
        return i % INDEX_STEP;
        //return i & (INDEX_STEP-1);
    }

    IString() {
    }

    public IString(String str) {
        init(str);
    }

    public static IString valueOf(CharSequence str) {
        if (str instanceof IString)
            return (IString) str;
        else
            return new IString(str.toString());
    }
    public static IString valueOf(CharSequence str, int start, int count) {
        int slen = str.length();
        if (start < 0 || count < 0 || start + count > slen)
            throw new IndexOutOfBoundsException();
        if (str instanceof IString) {
            IString istr = (IString) str;
            if (start == 0 && count == slen)
                return istr;
            return new SubString(istr, start, start+count);
        }
        int jlStart = Character.offsetByCodePoints(str, 0, start);
        int jlEnd = Character.offsetByCodePoints(str, jlStart, count);
        return new IString(str.subSequence(jlStart, jlEnd).toString());
    }

    private void init(String str) {
        this.str = str;
        cplength = Strings.sizeInCodePoints(str);
        if (cplength != str.length()) {
            int n = numSteps(cplength);
            offsets = n == 0 ? NO_OFFSETS : new int[n];
            int off = 0;
            for (int i = 0; i < n; i++) {
                off = str.offsetByCodePoints(off, INDEX_STEP);
                offsets[i] = off;
            }
        }
    }

    /** used for string-ref */
    public int indexByCodePoints(int i) {
        if (i < 0 || i >= cplength)
            throw new StringIndexOutOfBoundsException(); // FIXME add args
        return str.codePointAt(offsetByCodePoints(i)+jlStart());
    }

    /** Map character offset to char offset.
     * Caller is responsible for checking that {@code i >=0 && i <= cplength}.
     */
    public int offsetByCodePoints(int i) {
        if (offsets == null)
            return i;
        int jlOffset = jlStart();
        i += cpStart();
        int step = i>>INDEX_STEP_LOG;
        int ilo, ihi, clo, chi;
        // ilo <= i && i < ihi && ihi <= cplength
        // clo and chi are the indexes of ilo and ihi in str.
        ilo = i - restStep(i);
        clo = step == 0 ? 0 : offsets[step-1];
        if (ilo <= cplength - INDEX_STEP) {
            ihi = ilo + INDEX_STEP;
            chi = offsets[step];
        } else {
            ihi = cplength;
            chi = str.length();
        }
        // Optimization: all characters in [ilo..ihi)  are in BMP
        if (chi - clo == ihi - ilo)
            return clo + restStep(i) - jlOffset;
        // Optimization: no characters in range are in BMP
        if (chi - clo == 2 * (ihi - ilo))
            return clo + 2 * restStep(i) - jlOffset;
        // scan linearly from pos, at most INDEX_STEP-1 characters forward:
        return str.offsetByCodePoints(clo, restStep(i)) - jlOffset;
    }

    /* used for string-length */
    public int lengthByCodePoints() { return cplength; }

    /** To implement CharSequence */
    public char charAt(int i) { return str.charAt(i); }
    public String toString() { return str; }
    public int length() { return str.length(); }

    /** Substring using offsets in code-units (16-bit chars). */
    public IString subSequence(int from, int to) {
        return new IString(str.substring(from, to));
    }

    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeObject(str);
    }

    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        init((String) in.readObject());
    }

    public char[] toCharArray() { return toString().toCharArray(); }

    public byte[] getBytes(String charsetName)
        throws java.io.UnsupportedEncodingException
    { return toString().getBytes(charsetName); }

    public int hashCode() { return toString().hashCode(); }

    public boolean equals(Object other) {
        if (other instanceof IString) {
            IString str2 = (IString) other;
            return this.length() == str2.length()
                && Strings.compareTo(this, str2) == 0;
        }
        return false;
    }

    public int compareTo(IString other) {
        return Strings.compareTo(this, other);
    }
    int cpStart() { return 0; }
    int jlStart() { return 0; }

    public static final class SubString extends IString {
        int cpStart;
        int jlStart;
        int jlLength;
        private String jlString; // cache of substring

        /** Create a substring of the given base string.
         * Assumes caller has validated start and end.
         */
        public SubString(IString base, int start, int end) {
            super();
            this.str = base.str;
            int jlStart = base.offsetByCodePoints(start);
            int jlEnd = base.offsetByCodePoints(end);
            this.jlStart = jlStart;
            this.jlLength = jlEnd - jlStart;
            this.cpStart = start + base.cpStart();
            this.cplength = end - start;
            if (jlLength != cplength) {
                this.offsets = base.offsets;
            }
        }

        @Override
        int cpStart() { return cpStart; }

        @Override
        int jlStart() { return jlStart; }

        @Override
        public char charAt(int i) {
            if (i >= jlLength)
                throw new StringIndexOutOfBoundsException(i);
            return str.charAt(i+jlStart); }

        @Override
        public String toString() {
            String jstr = jlString;
            if (jstr == null) {
                jstr = str.substring(jlStart, jlStart+jlLength);
                jlString = jstr; // atomic cache update
            }
            return jstr;
        }
        @Override
        public int length() { return jlLength; }

        public void writeExternal(ObjectOutput out) throws IOException {
            out.writeObject(toString());
        }

        public void readExternal(ObjectInput in)
            throws IOException, ClassNotFoundException {
            this.str = (String) in.readObject();
        }

        public Object readResolve() throws ObjectStreamException {
            return new IString(str);
        }
    }
}
