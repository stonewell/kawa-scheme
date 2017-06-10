// Copyright (c) 2001, 2003, 2004  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.xml.*;
import java.io.*;
/* #ifdef use:java.lang.invoke */
import java.lang.invoke.*;
/* #else */
// import gnu.mapping.CallContext.MethodHandle; 
/* #endif */

public class MakeElement extends NodeConstructor implements Externalizable {
    static final MethodHandle applyToConsumerME =
        Procedure.lookupApplyHandle(MakeElement.class, "applyToConsumerME");
    public static final MakeElement makeElementS = new MakeElement();
    static { makeElementS.setStringIsText(true); }

    public MakeElement() {
        this.applyToConsumerMethod = applyToConsumerME;
        setCopyNamespacesMode(XMLFilter.COPY_NAMESPACES_PRESERVE);
    }

    public static MakeElement valueOf (Symbol tag, NamespaceBinding namespaceNodes, int options) {
        MakeElement m = new MakeElement();
        m.tag = tag;
        m.namespaceNodes = namespaceNodes;
        m.options = options;
        return m;
    }

    public int numArgs() { return tag == null ? 0xFFFFF001 : 0xFFFFF000; }

    /** Optional tag.  If non-null, the element tag is this value,
     * rather than the first parameter. */
    public Symbol tag;

    public String toString() { return "makeElement["+tag+"]"; }

    private static final int HANDLING_KEYWORD_PARAMETERS = 4;

    public int getCopyNamespacesMode() {
        return options & XMLFilter.COPY_NAMESPACES_MASK;
    }
    public void setCopyNamespacesMode(int val) {
        options = val;
    }

    /** Should {@code KEYWORD: EXPRESSION} be mapped to an
     * attribute constructor? */
    public boolean isHandlingKeywordParameters() {
        return (options & HANDLING_KEYWORD_PARAMETERS) != 0;
    }

    public void setHandlingKeywordParameters(boolean value) {
        if (value)
            options |= HANDLING_KEYWORD_PARAMETERS;
        else
            options &= ~HANDLING_KEYWORD_PARAMETERS;
    }

  NamespaceBinding namespaceNodes;

  public NamespaceBinding getNamespaceNodes ()
  {
    return namespaceNodes;
  }

  public void setNamespaceNodes (NamespaceBinding bindings)
  {
    namespaceNodes = bindings;
  }

  public static Symbol getTagName (ApplyExp exp)
  {
    Expression[] args = exp.getArgs();
    if (args.length > 0)
      {
	Expression arg0 = args[0];
	if (arg0 instanceof QuoteExp)
	  {
	    Object val = ((QuoteExp) arg0).getValue();
	    if (val instanceof Symbol)
	      return (Symbol) val;
	  }
      }
    return null;
  }

  public static void startElement (Consumer out, Symbol qname,
                                   int copyNamespacesMode,
                                   NamespaceBinding namespaceNodes)
  {
    XName type = new XName((Symbol) qname, namespaceNodes);
    if (out instanceof XMLFilter)
      ((XMLFilter) out).copyNamespacesMode = copyNamespacesMode;
    out.startElement(type);
  }

  public static void startElement (Consumer out, Symbol qname,
                                   int copyNamespacesMode)
  {
    if (out instanceof XMLFilter)
      ((XMLFilter) out).copyNamespacesMode = copyNamespacesMode;
    out.startElement(qname);
  }

  public static void endElement (Consumer out, Object type/*FIXME:unused*/)
  {
    out.endElement();
  }

    public static Object applyToConsumerME(Procedure proc, CallContext ctx) throws Throwable {
        System.err.println("ME.applyToConsumerME");
    Consumer saved = ctx.consumer;
    MakeElement mk = (MakeElement) proc;
    Symbol tag = mk.tag;
    Consumer out = pushNodeContext(ctx);
    try
      {
        Symbol type = tag != null ? tag : (Symbol) ctx.getNextArg();
        int i = tag != null ? 0 : 1;
	if (mk.namespaceNodes != null)
	  startElement(out, type, mk.options, mk.namespaceNodes);
	else
	  startElement(out, type, mk.options);
        int len = ctx.numArguments()-1;
	for (; i <= len;  i++)
	  {
            Object arg = ctx.getArgAsObject(i);
            String key = ctx.getKeyword(i);
            if (key != null) {
                writeContent(Keyword.make(key), out);
            }
            if (mk.getStringIsText())
                writeContentS(arg, out);
            else
                writeContent(arg, out);
            // Handling Keyword values is actually done by the Consumer.
            if (mk.isHandlingKeywordParameters())
              out.endAttribute();
	  }
	endElement(out, type);
      }
    finally
      {
	popNodeContext(saved, ctx);
      }
    return null;
  }

  public void compileToNode (ApplyExp exp, Compilation comp,
			     ConsumerTarget target)
  {
    Variable consumer = target.getConsumerVariable();
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    CodeAttr code = comp.getCode();
    code.emitLoad(consumer);
    code.emitDup();
    int i;
    Target tagTarget = CheckedTarget.getInstance(Compilation.typeSymbol);
    if (tag == null)
      {
        args[0].compile(comp, tagTarget);
        i = 1;
      }
    else
      {
        comp.compileConstant(tag, tagTarget);
        i = 0;
      }
    code.emitDup(1, 1); // dup_x1
    // Stack:  consumer, tagtype, consumer, tagtype
    code.emitPushInt(options);
    if (namespaceNodes != null)
      {
	comp.compileConstant(namespaceNodes, Target.pushObject);
	code.emitInvokeStatic(startElementMethod4);
      }
    else
      code.emitInvokeStatic(startElementMethod3);
    for (;  i < nargs;  i++)
      {
        compileChild(args[i], getStringIsText(), comp, target);
        if (isHandlingKeywordParameters())
          {
            code.emitLoad(consumer);
            code.emitInvokeInterface(MakeAttribute.endAttributeMethod);
          }
      }
    code.emitInvokeStatic(endElementMethod);
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }

    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeObject(tag);
        out.writeObject(namespaceNodes);
        out.writeInt(options);
    }

    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        tag = (Symbol) in.readObject();
        namespaceNodes = (NamespaceBinding) in.readObject();
        options = in.readInt();
    }

  static final ClassType typeMakeElement
    = ClassType.make("gnu.kawa.xml.MakeElement");
  static final Method startElementMethod3
    = typeMakeElement.getDeclaredMethod("startElement", 3);
  static final Method startElementMethod4
    = typeMakeElement.getDeclaredMethod("startElement", 4);
  static final Method endElementMethod
    = typeMakeElement.getDeclaredMethod("endElement", 2);
}
