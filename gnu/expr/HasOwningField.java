package gnu.expr;

import gnu.bytecode.Field;

/** Used for compiling literals. */

public interface HasOwningField
{
    /** If non-null a field that has this value. */
    public Field getOwningField();
}
