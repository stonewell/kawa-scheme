package gnu.mapping;

import gnu.expr.Keyword;
import gnu.lists.*;
import java.util.RandomAccess;

public class ArgListPair
    extends ImmutablePair implements ArgList, RandomAccess
{
    private ArgListVector arguments;
    private int start;

    protected ArgListPair(ArgListVector base, int skip) {
        super(base.get(skip), null);
        this.arguments = base;
        this.start = skip;
    }

    public static LList getArgs(CallContext ctx) {
        return valueOf(ArgListVector.getArgs(ctx));
    }

    public static LList valueOf(ArgListVector args) {
        return args == null || args.isEmpty() ? LList.Empty
            : new ArgListPair(args, 0);
    }

    public static LList valueOf(ArgListVector args, int skip) {
        return args == null || args.size() <= skip ? LList.Empty
            : new ArgListPair(args, skip);
    }

    public Object getArgAsObject(int i) {
        if (i < firstKeyword())
            return get(i);
        else if (i >= firstKeyword() + numKeywords())
            return get(i + numKeywords());
        else
            return get(2  * i - firstKeyword() + 1);
    }

    public String getKeyword(int i) {
        int firstKeyword = firstKeyword();
        if (i < firstKeyword || i >= firstKeyword + numKeywords())
            return null;
        else
            return ((Keyword) get(2  * i - firstKeyword)).getName();
    }

    public int findKeyword(String key) {
         return ArgListImpl.findKeyword(this, key);
    }

    public int numArguments() {
        return arguments.size() - start - numKeywords();
    }

    public int numKeywords() {
        int numKeywordsBase = arguments.numKeywords();
        int firstKeywordBase = arguments.firstKeyword();
        if (start <= firstKeywordBase)
            return numKeywordsBase;
        else if (start >= firstKeywordBase + 2 * numKeywordsBase)
            return 0;
        else
            return firstKeywordBase - ((start+1) >> 1);
    }
    public int firstKeyword() {
        int numKeywordsBase = arguments.numKeywords();
        int firstKeywordBase = arguments.firstKeyword();
        if (start <= firstKeywordBase)
            return firstKeywordBase;
        else if (start >= firstKeywordBase + 2 * numKeywordsBase)
            return 0;
        else
            return start & 1;
    }

    @Override
    public int size() { return arguments.size() - start; }

    @Override
    public Object get(int index) {
        return arguments.get(start+index);
    }

    @Override
    public Object getCdr() {
        if (cdr == null)
            cdr = valueOf(arguments, start+1);
        return cdr;
    }
}

