package gnu.kawa.util;

public class PrimArrayUtils
{
    /** Initialize six elements of an int array at once.
     * This is a hack to work around Code size limitations.
     */
    public static int[] initArray6Int(int[] arr, int index,
                                      int a, int b, int c,
                                      int d, int e, int f)
    {
        arr[index] = a;
        arr[index+1] = b;
        arr[index+2] = c;
        arr[index+3] = d;
        arr[index+4] = e;
        arr[index+5] = f;
        return arr;
    }
}
