// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;

import gnu.expr.*;
import gnu.mapping.*;
import gnu.text.*;
import java.io.*;
import java.net.URL;
import java.util.*;
import javax.servlet.*;
import javax.servlet.http.*;

/**
 * The Kawa servlet interpreter
 *
 * This servlet is responsible for reading and interpeting Kawa language files
 * using the QEXO GNU library.
 *
 * The implementation borrows ideas from Apache Jakarta Tomcat Jasper.
 *
 * @author Ivelin Ivanov
 * @author Tom Reilly
 * @author Per Bothner
 */
public class KawaPageServlet extends KawaServlet
{
  private static long LAST_MODIFIED_CACHE_TIME = 1000;
  private static Map procedureCache = new WeakHashMap();
  private ServletContext context;

  public void init(ServletConfig config)
      throws ServletException
  {
    super.init(config);
    context = config.getServletContext();
  }

  public void run(CallContext ccontext) throws Throwable
  {
    ServletCallContext ctx = (ServletCallContext) ccontext;
    HttpServletRequest request = ctx.request;
    HttpServletResponse response = ctx.response;

    boolean saveClass = request.getParameter("qexo-save-class") != null;
    String path = request.getServletPath();
    ModuleBody mod = getModule(ctx, path, saveClass);

    if (mod != null)
      mod.run(ctx);
  }

  private ModuleBody getModule(ServletCallContext ctx, String path, boolean saveClass)
    throws Exception
  {
    URL url = context.getResource(path);

    if (url == null)
      {
	ctx.response.reset();
	ctx.response.sendError(HttpServletResponse.SC_NOT_FOUND, path);
	return null;
      }

    // don't compile the same page concurrently
    synchronized (procedureCache)
      {
	CacheEntry entry = getCacheEntry(path,  url);

	if (entry.proc == null || saveClass)
	  {
	    InputStream resourceStream = url.openStream();
	    InPort port = new InPort(resourceStream,
				     path.substring(path.lastIndexOf('/')+1));
	    Interpreter interp = Interpreter.getInstanceFromFilenameExtension(path);
	    Interpreter.defaultInterpreter = interp;
	    Environment.setCurrent(interp.getEnvironment());
	    SourceMessages messages = new SourceMessages();
	    Compilation comp;
	    try
	      {
		comp = interp.parse(port, messages, interp.PARSE_IMMEDIATE);
		String name = path.substring(path.lastIndexOf('/')+1,
					     path.indexOf('.'));
		comp.getModule().setName(name);
	      }
	    catch (SyntaxException ex)
	      {
		if (ex.getMessages() != messages)
		  throw ex;
		// Otherwise handled below ...
		comp = null; // Needed to avoid spurious compilation error.
	      }

	    // FIXME: we could output a nice pretty HTML table of the errors
	    // or show the script with the errors highlighted, for bonus
	    // points the pretty page could be generated by a precompiled
	    // xql script with the errors passed as XML somehow and accessed
	    // via input()
	    if (messages.seenErrors())
	      {
		ctx.response.reset();
		ServletOutputStream out = ctx.response.getOutputStream();
		out.print(messages.toString(20));
		return null;
	      }

	    Class cl = ModuleExp.evalToClass(comp);

	    if (saveClass)
	      comp.outputClass(context.getRealPath("WEB-INF/classes")+'/');

	    entry.proc = (ModuleBody) cl.newInstance();
	  }

	return entry.proc;
      }
  }

  private CacheEntry getCacheEntry (String path, URL url)
      throws IOException
  {
    CacheEntry entry = (CacheEntry) procedureCache.get(path);
    long now = System.currentTimeMillis();

    // avoid hitting the disk too much
    if (entry != null && (now - entry.lastCheck < LAST_MODIFIED_CACHE_TIME))
      return entry;

    long lastModified = url.openConnection().getLastModified();

    if (entry != null)
      {
	if (entry.lastModified != lastModified)
	  entry.proc = null;
      }
    else
      {
	entry = new CacheEntry();
	procedureCache.put(path, entry);
      }

    entry.lastCheck = now;
    entry.lastModified = lastModified;

    return entry;
  }

  static class CacheEntry
  {
    long lastCheck;
    long lastModified;
    ModuleBody proc;
  }
}

