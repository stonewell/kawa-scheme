package gnu.mapping;

import java.util.List;

public interface ArgListBuilder {
    public void clear();

    public void add(Object arg);

    public void addAll(ArgList args);
    public void addAll(List<?> args);

    public void setArgs();
    public void setArgs(Object arg0);
    public void setArgs(Object arg0, Object arg1);
    public void setArgs(Object arg0, Object arg1, Object arg2);
    public void setArgs(Object arg0, Object arg1, Object arg2, Object arg3);

    /**
     * @param keyword must be an interned String
     * @param arg the corresponding value
     */
    public void addKey(String keyword, Object arg);
    /** Note the the previous numKeywords arguments were keywords arguments.
     */
    public void setKeys(int numKeywords, String[] keys, short[] sorted);

    public ArgList asArgList();
    public ArgList asFreshArgList();
}
