// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

/**
  * Represents an Attribute of an AttrContainer.
  * <p>
  * Various sub-classses are used for standard attributes,
  * or you can use MiscAttr for a generic attribute.
  * @author      Per Bothner
  */

public abstract class Attribute
{
  /** Every Attribute belongs to some AttrContainer object. */
  AttrContainer container;
  /** Return the Attribute container that contains this Attribute. */
  public final  AttrContainer getContainer() { return container; }
  public final void setContainer(AttrContainer container)
  { this.container = container; }

  private Attribute next;
  /** Get the next Attribute belonging to getContainer(). */
  public final Attribute getNext() { return next; }
  /** Set the next Attribute in the chain belonging to getContainer(). */
  public final void setNext(Attribute next) { this.next = next; }

  /** Add this to (the front of) of the specified attribute container. */
  public void addToFrontOf(AttrContainer container)
  {
    setContainer(container);
    setNext(container.getAttributes()); 
    container.setAttributes(this);
  }

  String name; // This is an interned string.

  public final String getName() { return name; }
  public final void setName(String name) { this.name = name.intern(); }

  /** Create a new Attribute.
    * @param name - an interned String that names the Attribute. */
  public Attribute (String name)
  {
    this.name = name;
  }

  /** Find an Attribute by name, in an attribute cointainer.
    * @param container the attribute container to search
    * @param name the (interned) name of the attribute we are seeking
    * @return the matching Attribute, or null if the search failed.
    */
  public static Attribute get (AttrContainer container, String name)
  {
    for (Attribute attr = container.getAttributes();
	 attr != null;  attr = attr.next)
      {
	if (attr.getName() == name)
	  return attr;
      }
    return null;
  }
};
