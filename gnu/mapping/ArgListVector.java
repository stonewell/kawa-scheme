package gnu.mapping;
import gnu.lists.*;
import gnu.expr.Keyword;

public class ArgListVector extends ConstVector implements ArgList {
    private int firstKeyword;
    private int numKeywords;

    public ArgListVector(Object[] args, int firstKeyword, int numKeywords) {
        super(args);
        this.firstKeyword = firstKeyword;
        this.numKeywords = numKeywords;
    }

    public static ArgListVector getArgs(CallContext ctx) {
        return drop(ctx, 0);
    }

    public static ArgListVector prepend(ArgListVector args, Object... extra) {
        int xlen = extra.length;
        int n = args.size();
        Object[] vals = new Object[xlen+n];
        for (int i = 0; i < n; i++)
            vals[xlen+i] = args.get(i);
        System.arraycopy(extra, 0, vals, 0, xlen);
        return new ArgListVector(vals, args.firstKeyword()+xlen,
                                 args.numKeywords());
    }
    public static ArgListVector drop(ArgList args, int toSkip) {
        int numKeywords = args.numKeywords();
        int firstKeyword = args.firstKeyword();
        int count = args.numArguments();
        int size = numKeywords + count - toSkip;
        Object[] vals = new Object[size];
        int skipKeys;
        int i;
        int newFirst;
        if (toSkip <= firstKeyword) {
            i = toSkip;
            skipKeys = 0;
            newFirst = firstKeyword - toSkip;
        } else if (toSkip >= firstKeyword+2*numKeywords) {
            i = toSkip - numKeywords;
            skipKeys = numKeywords;
            newFirst = 0;
        } else {
            int skipKeys2 = toSkip - firstKeyword;
            skipKeys = (skipKeys2 + 1) >> 1;
            i = firstKeyword + skipKeys;
            newFirst = skipKeys2 & 1;
            if (newFirst != 0) {
                firstKeyword++;
                numKeywords--;
                i--;
             }
        }
        for (int j = 0; j < size;) {
            if (i >= firstKeyword && i < firstKeyword+numKeywords) {
                vals[j++] = Keyword.make(args.getKeyword(i));
            }
            vals[j++] = args.getArgAsObject(i);
            i++;
        }
        return new ArgListVector(vals, newFirst, numKeywords-skipKeys);
    }

    public int numArguments() { return size() - numKeywords; }

    public Object getArgAsObject(int i) {
        // adjust because keywords have 2 elements when viewed as a vector.
        if (i < firstKeyword)
            return get(i);
        else if (i >= firstKeyword + numKeywords)
            return get(i + numKeywords);
        else
            return get(2  * i - firstKeyword + 1);
    }

    public String getKeyword(int i) {
        if (i < firstKeyword || i >= firstKeyword + numKeywords)
            return null;
        else
            return ((Keyword) get(2  * i - firstKeyword)).getName();
     }

    public int numKeywords() { return numKeywords; }
    public int firstKeyword() { return firstKeyword; }

    public int findKeyword(String key) {
        return ArgListImpl.findKeyword(this, key);
    }
}
