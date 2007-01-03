package kawa;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.text.*;

public class TelnetRepl extends Procedure0
{
  // close when finished.
  java.net.Socket socket;

  Language language;

  public TelnetRepl(Language language, java.net.Socket socket)
  {
    this.language = language;
    this.socket = socket;
  }

  public Object apply0 ()
  {
    try
      {
	Shell.run(language, Environment.getCurrent());
	return Values.empty;
      }
    finally
      {
	try
	  {
	    socket.close();
	  }
	catch (java.io.IOException ex)
	  {
	  }
      }
  }


  /** Run a Kawa repl as a telnet server.
      @param client A client that has connected to us,
      and that wants to use the telnet protocol to talk to a
      Scheme read-eval-print-loop. */
  public static void serve (Language language, java.net.Socket client)
    throws java.io.IOException
  {
    Telnet conn = new Telnet(client, true);
    java.io.OutputStream sout = conn.getOutputStream();
    java.io.InputStream sin = conn.getInputStream();
    OutPort out = new OutPort(sout, FilePath.valueOf("/dev/stdout"));
    TtyInPort in = new TtyInPort(sin, FilePath.valueOf("/dev/stdin"), out);
    /*
    conn.request(Telnet.DO, Telnet.EOF);
    conn.request(Telnet.DO, Telnet.NAWS);
    conn.request(Telnet.DO, Telnet.TTYPE);
    conn.request(Telnet.DO, Telnet.LINEMODE);
    */

    Thread thread = new Future(new TelnetRepl(language, client),
			       Environment.getCurrent(),
			       in, out, out);
    thread.start();
  }
}

