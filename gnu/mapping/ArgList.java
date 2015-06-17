package gnu.mapping;

public interface ArgList {
    /** Number of actual arguments.
     * Each (keyword,argument)-pair counts as one (not two).
     */
    public int size();

    public Object getArgAsObject(int i);

    public int numKeywords();
    public int firstKeyword();
    public String getKeyword(int index);
}
