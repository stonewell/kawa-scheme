package gnu.lists;

import java.io.*;

// FIXME should also implement CharSeq, Consumable
// maybe also AVector<Char>, RandomAccess
// FIXME should extend AbstractSequence<gnu.text.Char>
//    of perhaps (unlikely) extend SimpleVector<gnu.text.Char>

/** A string implementation with contant-time codepoint indexing.
 * Suitable for SRFI-135 or SRFI-140.
 */
public class IString implements CharSequence, Externalizable {
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

    public IString(String str) {
        init(str);
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
        if (offsets == null)
            return (int) str.charAt(i);
        return str.codePointAt(offsetByCodePoints(i));
    }

    public int offsetByCodePoints(int i) {
        if (offsets == null)
            return i;
        if (i < 0 || i >= cplength)
            throw new StringIndexOutOfBoundsException(); // FIXME
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
            return clo + restStep(i);
        // Optimization: no characters in range are in BMP
        if (chi - clo == 2 * (ihi - ilo))
            return clo + 2 * restStep(i);
        // scan linearly from pos, at most INDEX_STEP-1 characters forward:
        return str.offsetByCodePoints(clo, restStep(i));
    }

    /* used for string-length */
    public int lengthByCodePoints() { return cplength; }

    /** To implement CharSequence */
    public char charAt(int i) { return str.charAt(i); }
    public String toString() { return str; }
    public int length() { return str.length(); }
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
}
