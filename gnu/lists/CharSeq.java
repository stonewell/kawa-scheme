// Copyright (c) 2001, 2005  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import gnu.text.Char;

/** A sequence where each element is a character (Unicode code point).
 */

public interface CharSeq
    extends CharSequence, Sequence<Char>, Consumable
{
  /** Get length of string, in code units (not characters).
   * In contract, size() returns the number of 16-bit code points. */
  public int length();

  /** The index is in terms of code units. */
  public char charAt(int index);

  /** Copy characters into a destination buffer.
   * Same interface as java.lang.String's getChars.
   * @param srcBegin source start position, in 16-bit code units.
   * @param srcEnd source end position, in 16-bit code units.
   * @param dst destination
   * @param dstBegin index (in code units) in dst array
   */
  public void getChars (int srcBegin, int srcEnd, char[] dst, int dstBegin);

  public void setCharAt(int index, char ch);
  public void setCharacterAt(int index, int ch);

  /** Append a specified subsequence to an <code>Appendable</code>.
   * An allowable implementation is:
   * <code>dest.append(this, start, start+count)</code>.
   * Hence implementors of <code>Appendable</code> should avoid calling
   * <code>writeTo</code> - though they can call <code>getChars</code>.
   */
  public void writeTo(int start, int count, Appendable dest)
    throws java.io.IOException;

  public void writeTo(Appendable dest)
    throws java.io.IOException;

  public String toString();
}
