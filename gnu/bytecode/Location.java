// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

/** An abstracted "variable", inherited by Field and Variable. */

public class Location {
  protected String name;
  private Type type;
  int name_index; /* Index in constant table, or 0 if un-assigned */
  int signature_index; /* Index in constant table, or 0 if un-assigned */

  public final String getName ()
  {
    return name;
  }

  public final void setName (String name)
  {
    this.name = name;
  }

    public Type getType() {
        return type;
    }

  public final void setType(Type type)
  {
    this.type = type;
  }

    public final String getSignature () { return getType().getRawType().getSignature (); }

}
