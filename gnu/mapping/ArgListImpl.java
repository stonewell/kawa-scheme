package gnu.mapping;

import java.util.List;

class ArgListImpl implements ArgList, ArgListBuilder {
    /*
    public Object value0;
    public Object value1;
    public Object value2;
    public Object value3;
    public int ivalue0;
    public int ivalue1;
    public long lvalue0;
    public long lvalue1;
    public long lvalue2;
    */
    protected Object[] values = new Object[8];
    public/*FIXME*/ int count;

    public void printArgs() {
        System.err.print("args count:"+count);
        for (int i = 0; i < count; ) {
            System.err.print(" "+i+": "+values[i]);
            if (++i != count) System.err.print(';');
        }
        System.err.println();
    }

    /*public final static int ARG_IN_VALUE0 = 0;
    public final static int ARG_IN_VALUE1 = 1;
    public final static int ARG_IN_VALUE2 = 2;
    public final static int ARG_IN_VALUE3 = 3;
    public final static int ARG_IN_IVALUE0 = 4;
    public final static int ARG_IN_IVALUE1 = 5;
    //public final static int ARG_IN_LVALUE0 = 6;
    //public final static int ARG_IN_LVALUE0 = 7;
    //FIXME also handle float and double
    */

    /* * Encoding of where the arguments are.
     * Each argument uses 4 bits.
     * Arguments beyond 8 are implicitly ARG_IN_VALUES_ARRAY.
     * /
    int where;
    */

    public ArgListImpl() {
    }

    public ArgListImpl(ArgListImpl args) {
        int n = args.count;
        this.count = n;
        Object[] v = new Object[n];
        System.arraycopy(args.values, 0, v, 0, n);
        values = v;
        int nk = args.numKeywords;
        this.firstKeyword = args.firstKeyword;
        this.numKeywords = nk;
        String[] kw = args.keywords;
        if (kw != null && kw.length != nk) {
            String[] knew = new String[nk];
            System.arraycopy(kw, 0, knew, 0, nk);
            kw = knew;
        }
        this.keywords = kw;
        this.sortedKeywords = args.sortedKeywords;
    }

    public int numArguments() { return count; }

    void resetArgCount(int size) {
        if (count > values.length) throw new Error("bad count:"+count+" vlen:"+values.length);
        int len = values.length;
        if (len < size) {
            int nsize = size > 32 ? size : 2 * size;
            values = new Object[nsize];
        } else if (len > size + 64) {
            values = null;
        } else {
            for (int i = size; i < count; i++)
                values[i] = null;
        }
        count = size;
        numKeywords = 0;
        keywords = null;
        sortedKeywords = null;
    }

    void ensureSpace(int size) {
        int osize = values.length;
        if (osize < size) {
            int nsize = osize <= 16 ? 32 : osize + (osize >> 1);
            if (nsize < size)
                nsize = size;
            Object[] v = new Object[nsize];
            System.arraycopy(values, 0, v, 0, count);
            values = v;
        }
    }

    public void shiftArgs(int toDrop) {
        count -= toDrop;
        firstKeyword -= toDrop;
        if (firstKeyword < 0)
            if (numKeywords == 0)
                firstKeyword = 0;
            else
                throw new Error("bad shiftArgs with keyword");
        System.arraycopy(values, toDrop, values, 0, count);
    }

    public void clear() {
        resetArgCount(0);
    }

    public void setArgs() {
        resetArgCount(0);
    }

    public void setArgs(Object arg0) {
        resetArgCount(1);
        values[0] = arg0;
        //where = ARG_IN_VALUE0;
    }

    public void setArgs(Object arg0, Object arg1) {
        resetArgCount(2);
        values[0] = arg0;
        values[1] = arg1;
        //where = ARG_IN_VALUE0|(ARG_IN_VALUE0<<4);
    }

    public void setArgs(Object arg0, Object arg1, Object arg2) {
        resetArgCount(3);
        values[0] = arg0;
        values[1] = arg1;
        values[2] = arg2;
    }

    public void setArgs(Object arg0, Object arg1, Object arg2, Object arg3) {
        resetArgCount(4);
        values[0] = arg0;
        values[1] = arg1;
        values[2] = arg2;
        values[3] = arg3;
    }

    public void setArgsAll(Object[] args) {
        resetArgCount(args.length);
        System.arraycopy(args, 0, values, 0, args.length);
    }

    public void setArgsAll(Object[] args, int fromIndex, int toIndex) {
        int length = toIndex - fromIndex;
        resetArgCount(length);
        System.arraycopy(args, fromIndex, values, 0, length);
    }

    public void add(Object arg) {
        int n = count;
        ensureSpace(n+1);
        count = n+1;
        values[n] = arg;
    }
    public void add(Object arg0, Object arg1, Object arg2, Object arg3) {
        int n = count;
        ensureSpace(n+4);
        count = n+4;
        values[n] = arg0;
        values[n+1] = arg1;
        values[n+2] = arg2;
        values[n+3] = arg3;
    }

    public void addAll(ArgList args) {
        int sz = args.numArguments();
        int k0 = args.firstKeyword();
        int nk = args.numKeywords();
        ensureSpace(count+sz);
        if (args instanceof ArgListImpl) {
            // FIXME optimize
        }
        for (int i = 0; i < sz; i++) {
            Object a = args.getArgAsObject(i);
            if (i >= k0 && i < k0+nk)
                addKey(args.getKeyword(i), a);
            else
                add(a);
        }
    }

    public void addSequence(Object args) {
        // FIXME optimize
        addAll(gnu.lists.Sequences.coerceToSequence(args));
    }
    public void addAll(List<?> args) {
        int sz = args.size();
        int n = count;
        ensureSpace(n+sz);
        for (Object a : args) {
            values[n++] = a;
        }
        count = n;
    }

    public void addKey(String keyword, Object arg) {
        if (numKeywords == 0)
            firstKeyword = count;
        else if (firstKeyword + numKeywords != count)
            throw new RuntimeException("keyword arguments must be continuous");
        add(arg);
        if (keywords == null) {
            keywords = new String[16];
        } else if (numKeywords == keywords.length) {
            String[] knew = new String[2 * numKeywords];
            System.arraycopy(keywords, 0, knew, 0, numKeywords);
            keywords = knew;
        }
        keywords[numKeywords++] = keyword;
        sortedKeywords = null;
    }
            
    public Object getArgAsObject(int i) {
        /*
        if (i < 8) {
            switch ((where >> (4 * i)) & 15) {
            case ARG_IN_VALUE0:  return value0;
            case ARG_IN_VALUE1:  return value1;
            case ARG_IN_VALUE2:  return value2;
            case ARG_IN_VALUE3:  return value3;
            case ARG_IN_IVALUE0:  return Integer.make(ivalue0);
            case ARG_IN_IVALUE1:  return Integer.make(ivalue1);
                //case ARG_IN_LVALUE0:  return IntNum.make(ivalue0);
                //case ARG_IN_LVALUE1:  return IntNum.make(ivalue1);
            }
        }
        */
        return values[i];
    }

    /** Index in values array */
    int firstKeyword;
    int numKeywords;

    /** For the i'th keyword in alphabethical order,
     * keywords[sortedKeywords[i]] is the actual keyword.
     * This array is never modified - it is normally statically allocacted.
     */
    short[] sortedKeywords;

    /** The keywords in the argument list, in encounter order.
     * Each keyword is an interned String.
     * Normally this array is statically allocated.
     */
    String[] keywords;

    public int firstKeyword() { return firstKeyword; }
    public int numKeywords() { return numKeywords; }

    public String getKeyword(int index) {
        return index < firstKeyword || index >= firstKeyword+numKeywords ? null
            : keywords[index-firstKeyword];
    }

    public void setKeys(int numKeywords, String[] keys, short[] sorted) {
        this.numKeywords = numKeywords;
        this.keywords = keys;
        this.sortedKeywords = sorted;
        this.firstKeyword = count - numKeywords;
    }

    public ArgList asArgList() { return this; }
    public ArgList asFreshArgList() { return new ArgListImpl(this); }
}
