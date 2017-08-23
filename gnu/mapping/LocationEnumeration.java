// Copyright (c) 2000, 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

public class LocationEnumeration
  implements
  java.util.Iterator<NamedLocation<Object>>,
  java.util.Enumeration<NamedLocation<Object>>
{
  SimpleEnvironment env;
  NamedLocation prevLoc;
  NamedLocation nextLoc;
  /** Index field used by Environment.hasMoreElements.
      If inherited==null, index in bindings, else index in env.inherited. */
  int index;
  LocationEnumeration inherited;
  NamedLocation[] bindings;

  public LocationEnumeration(NamedLocation[] bindings, int count)
  {
    this.bindings = bindings;
    index = count;
  }

  public LocationEnumeration(SimpleEnvironment env)
  {
    this(env.table, 1 << env.log2Size);
  }

  public boolean hasMoreElements()
  {
    return env.hasMoreElements(this);
  }

  public NamedLocation nextElement()
  {
    return nextLocation();
  }

  public NamedLocation nextLocation()
  {
    if (nextLoc == null && ! hasMoreElements())
      throw new java.util.NoSuchElementException();
    NamedLocation oldPrev = prevLoc;
    if (prevLoc == null)
      {
        NamedLocation first = bindings[index];
        if (nextLoc != first)
          prevLoc = first;
      }
    while (prevLoc != null && prevLoc.next != nextLoc)
      prevLoc = prevLoc.next;
    NamedLocation r = nextLoc;
    nextLoc = nextLoc.next;
    return r;
  }

  public boolean hasNext ()
  {
    return hasMoreElements();
  }

  public NamedLocation next ()
  {
    return nextLocation();
  }

  public void remove ()
  {
    NamedLocation curLoc = prevLoc != null ? prevLoc.next : bindings[index];
    if (curLoc == null || curLoc.next != nextLoc)
      throw new IllegalStateException();
    curLoc.next = null;
    if (prevLoc != null)
      prevLoc.next = nextLoc;
    else
      bindings[index] = nextLoc;
    env.num_bindings--;
  }
}
