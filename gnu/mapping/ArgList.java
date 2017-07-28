package gnu.mapping;

public interface ArgList {
    /** Number of actual arguments.
     * Each (keyword,argument)-pair counts as one (not two).
     */
    public int numArguments();

    /** Index into arguments, not counting keywords.
     * I.e. for [a b k1: c k2: d e] the index 0 returns a,
     *   2 returns c, 3 returns d, 4 returns e.
     */
    public Object getArgAsObject(int i);

    public int numKeywords();
    public int firstKeyword();
    /** Get keyword (as an interned String) for given argument.
     * Indexing is the same as getArgAsObject.
     * Return null for non-keyword arguments.
     */
    public String getKeyword(int index);

    /** Find index for keyword.
     * @param key an interned string
     */
    public int findKeyword(String key);
}
