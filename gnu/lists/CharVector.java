// Copyright (c) 2001, 2004  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import gnu.text.Char;
import java.io.*;
import gnu.text.Char;

/** Simple adjustable-length vector whose elements are 16-bit chars.
 * Meant to be used as a wrapper for char arrays, so does not
 * implement CharSequence.
 * @author Per Bothner
 */

public class CharVector extends AbstractCharVector<Character>
{
    /** Create an CharVector from a char[].
     * Note that this contructor does *not* copy the argument. */
    public CharVector(char[] values) {
        data = values;
    }

    public final Character getRaw(int index) {
        return data[index];
    }

    @Override
    public final void setRaw(int index, Character value) {
        data[index] = value.charValue();
    }

    public static CharVector castOrNull(Object obj) {
        if (obj instanceof char[])
            return new CharVector((char[]) obj);
        if (obj instanceof CharVector)
            return (CharVector) obj;
        if (obj instanceof CharSequence) {
            char[] chars = obj instanceof FString ? ((FString) obj).toCharArray()
                : obj.toString().toCharArray();
            return new CharVector(chars);
        }
        return null;
    }

    public static CharVector cast(Object value) {
        CharVector vec = castOrNull(value);
        if (vec == null) {
            String msg;
            if (value == null)
                msg = "cannot convert null to CharVector";
            else
                msg = "cannot convert a "+value.getClass().getName()+" to CharVector";
            throw new ClassCastException(msg);
        }
        return vec;
    }

    public boolean equals(Object obj) {
        return obj instanceof CharVector && equals(this, (CharVector) obj);
    }

   @Override
    protected CharVector newInstance(int newLength) {
        return new CharVector(newLength < 0 ? data : new char[newLength]);
    }

    public int getElementKind() { return CHAR_VALUE; }

    public String getTag() { return "c16"; }
}
