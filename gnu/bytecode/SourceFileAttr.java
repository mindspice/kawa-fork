// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;


/* Represents the contents of a standard "SourceFile" attribute.
 * @author      Per Bothner
 */

public class SourceFileAttr
{
  public static String fixSourceFile (String fname)
  {
    String fsep = System.getProperty("file.separator", "/");
    if (fsep != null && fsep.length() == 1)
      {
	char fsep0 = fsep.charAt(0);
	if (fsep0 != '/')
	  fname = fname.replace(fsep0, '/');
      }
    return fname;
  }
}
