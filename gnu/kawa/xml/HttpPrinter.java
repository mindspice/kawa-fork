// Copyright (c) 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.xml.*;
import java.io.*;
import gnu.mapping.*;
import java.util.Vector;

/** Output as an Http response.
 * Used for both CGI scripts (default) and HttpServletResponse (future).
 */

public class HttpPrinter extends FilterConsumer
{
  Vector headers = new Vector();
  StringBuffer sbuf = new StringBuffer(100);
  String currentHeader;

  String sawContentType;

  OutputStream ostream;
  OutPort writer;

  public HttpPrinter(OutputStream out)
  {
    super(null);
    ostream = out;
    //ostream = System.out;
  }

  public HttpPrinter(OutPort out)
  {
    super(null);
    writer = out;
    //ostream = System.out;
  }

  public static HttpPrinter make (OutPort out)
  {
    return new HttpPrinter(out);
  }

  private void writeRaw(String str)
    throws java.io.IOException
  {
    if (writer != null)
      writer.write(str);
    else
      {
	int len = str.length();
	for (int i = 0;  i < len;  i++)
	  ostream.write((byte) str.charAt(i));
      }
  }

  public void printHeader(String label, String value)
    throws java.io.IOException
  {
    writeRaw(label);
    writeRaw(": ");
    writeRaw(value); // FIXME - need to quote?
    writeRaw("\n");
  }

  public void printHeaders()
    throws java.io.IOException
  {
    int num = headers.size();
    for (int i = 0;  i < num;  i += 2)
      printHeader(headers.elementAt(i).toString(),
		  headers.elementAt(i + 1).toString());
    //  if (sawContentType == null) writeRaw("Content-Type: text/html"); FIXME
    writeRaw("\n");
  }

  public void addHeader(String label, String value)
  {
    if (label.equalsIgnoreCase("Content-type"))
      sawContentType = value;
    headers.add(label);
    headers.add(value);
  }

  public void beginAttribute(String attrName, Object attrType)
  {
    if (base == null)
      currentHeader = attrName;
    else
      base.beginAttribute(attrName, attrType);
  }

  public void endAttribute()
  {
    if (currentHeader != null)
      {
	addHeader(currentHeader, sbuf.toString());
	sbuf.setLength(0);
	currentHeader = null;
      }
    else
      base.endAttribute();
  }

  boolean seenXmlHeader;

  public void beginGroup(String typeName, Object type)
  {
    if (base == null)
      {
	if (sawContentType == null)
	  {
	    String mimeType;
	    if (! seenXmlHeader)
	      mimeType = "text/html";
	    else if (typeName.equals("html"))
	      mimeType = "text/xhtml";
	    else
	      mimeType = "text/xml";
	    addHeader("Content-type", mimeType);
	  }
	if (writer == null)
	  writer = new OutPort(ostream); // FIXME use encoding.
	String style = null;
	if ("text/html".equalsIgnoreCase(sawContentType))
	  style = "html";
	else if ("text/xhtml".equalsIgnoreCase(sawContentType))
	  style = "xhtml";
	else if ("text/plain".equalsIgnoreCase(sawContentType))
	  style = "plain";
	base = XMLPrinter.make(writer, style);
	try
	  {
	    printHeaders();
	  }
	catch (Throwable ex)
	  {
	    throw new RuntimeException(ex.toString());
	  }
      }
    base.beginGroup(typeName, type);
  }

  public void writeChars(String str)
  {
    if (base == null)
      sbuf.append(str);
    else
      base.writeChars(str);
  }

  public void beginDocument()
  {
    if (base != null)
      base.beginDocument();
  }

  public void endDocument()
  {
    System.out.println("HttpPrinter.endDocument called"); System.out.flush();
    if (base != null)
      base.endDocument();
    try
      {
	// else ???;
	if (writer != null)
	  writer.flush();
	if (ostream != null)
	  ostream.flush();
      }
    catch (Throwable ex)
      {
      }
  }
}
