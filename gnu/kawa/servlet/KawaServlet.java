// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;
import javax.servlet.*;
import javax.servlet.http.*;
import gnu.mapping.*;
import gnu.xml.*;
import java.io.IOException;

public abstract class KawaServlet
extends HttpServlet implements CpsMethodContainer
{
  public void doGet(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException
  {
    ServletCallContext ctx = new ServletCallContext();
    ctx.request = request;
    ctx.response = response;
    ctx.servlet = this;
    ctx.values = Values.noArgs;
    ctx.consumer = new ServletPrinter(response);
    Thread thread = Thread.currentThread();
    CallContext.setInstance(thread, ctx);

    /* FIXME should use fluid binding!
    gnu.expr.Interpreter interp = gnu.expr.Interpreter.getInterpreter();
    String lang = interp.getName();
    Environment env = Environment.getCurrent();
    if (lang == "XQuery")
      {
	env.defineValue("request", request);
	env.defineValue("response", response);
	env.defineValue("servlet", this);
	env.defineValue("out", out);
      }
    else
      {
	env.defineValue("*request*", request);
	env.defineValue("*response*", response);
	env.defineValue("*servlet*", this);
	env.defineValue("*out*", out);
      }
    */

    ctx.consumer.beginDocument();
    apply(ctx);
    ctx.run();
    ctx.consumer.endDocument();
    CallContext.setInstance(thread, null);
  }

  public void apply(CpsMethodProc proc, CallContext context)
  {
  }

  public abstract void apply(CallContext context);
}

