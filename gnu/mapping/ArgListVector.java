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
}
