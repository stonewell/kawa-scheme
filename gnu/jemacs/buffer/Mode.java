package gnu.jemacs.buffer;
import gnu.mapping.*;
import gnu.expr.*;

/** Represents a "mode instance" - a mode active for a specific {@link Buffer}. */

public class Mode extends ModuleBody
{
  public Buffer buffer;
  public Mode next;

  public Buffer getBuffer()
  {
    return buffer;
  }
}
