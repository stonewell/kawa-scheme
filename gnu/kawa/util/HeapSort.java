package gnu.kawa.util;

public abstract class HeapSort<T, C> {
    public void heapSort(T a, int count, C comparator) {
        heapify(a, count, comparator);
        int end = count - 1;
        while (end > 0) {
            swap(a, end, 0);
            end--;
            siftDown(a, 0, end, comparator);
        }
    }
    protected abstract void swap(T a, int i, int j);

    protected abstract int compare(T a, int i, int j, C comparator);

    void heapify(T a, int count, C comparator) {
        for (int start = (count - 2) >> 1; start >= 0; start--) {
            siftDown(a, start, count-1, comparator);
        }
    }
    static int parent(int i) { return (i-1) >> 1; }

    void siftDown(T a, int start, int end, C comparator) {
        int root = start;
        while (2 * root  + 1 <= end) {
            int child = 2 * root + 1;
            int swap = root;
            if (compare(a, swap, child, comparator) < 0)
                swap = child;
            if (child+1 <= end && compare(a, swap, child+1, comparator) < 0)
                swap = child+1;
            if (swap == root)
                return;
            swap(a, root, swap);
            root = swap;
        }
    }

    /*
    void siftDown(T a, int start, int end) {
        int j = leafSearch(a, end, i);
        while (compare(a, i j) > 0)
            j = parent(j);
        
    }
    int leafSearch(T a, int end, int i) {
        int j = i;
        while (2*j <= end) {
            if (2*j+1 < end && compare(a, 2*j+1, 2*j) > 0)
                j = 2*j+1;
            else
                j=2*j;
        }
        return j;
    }
    */

    /** Sort an error of integer indexes based on a lookup object.
     * Could be used to implement APL "grade up": Initialize the array
     * to [0 ... count-1], and override lookup to index in the soyrce.
     */
    public abstract static class IndexSort extends HeapSort<int[],Object> {
        protected void swap(int[] a, int i, int j) {
            int t = a[i];
            a[i] = a[j];
            a[j] = t;
        }
        protected abstract Object lookup(Object comparator, int i);
        protected abstract int compare(Object x, Object y);
        protected int compare(int[] a, int i, int j, Object comparator) {
            int ai = a[i];
            int aj = a[j];
            Object x = lookup(comparator, ai);
            Object y = lookup(comparator, aj);
            int cmp = compare(x, y);
            if (cmp != 0)
                return cmp;
            return ai < aj ? -1 : ai > aj ? 1 : 0;
        }
    }
}
