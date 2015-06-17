package gnu.commonlisp.lang;

import gnu.bytecode.Type;
import gnu.expr.*;
import gnu.kawa.lispexpr.LangObjType;
import gnu.lists.Consumer;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.lists.PairWithPosition;
import gnu.mapping.Symbol;
import java.util.ArrayList;
import kawa.lang.Lambda;
import kawa.lang.SyntaxForm;
import kawa.lang.TemplateScope;
import kawa.lang.Translator;

/**
 * The syntax transformer that rewrites the ordinary lambda list from CL.
 * 
 * TODO:
 *   - arbitrary keyword symbols not supported (&key (secret password)).
 *     Most of it is implemented in comments, but there are a few bugs
 *     to iron out.
 *   - This class and its superclass could do with a refactoring. The
 *     goal would be to identify methods than can be sensibly overridden
 *     by the various parameter passing protocols in CL.
 *   - ANSI, related to refactoring, should differentiate between the
 *     lambda lists.
 * 
 * @author Charles Turner
 */
public class OrdinaryLambda extends Lambda
{
  protected Object allowOtherKeysKeyword;
  protected Object auxKeyword;
  protected Object bodyKeyword;
  
  public void setKeywords (Object optional, Object rest, Object key,
                           Object allowOthers, Object aux, Object body)
  {
    optionalKeyword = optional;
    restKeyword = rest;
    keyKeyword = key;
    allowOtherKeysKeyword = allowOthers;
    auxKeyword = aux;
    bodyKeyword = body;
  }
  
  /**
   * Top-level rewriter of lambda expressions.
   * 
   * @param formals the formal parameter list (or symbol)
   * @param body the body of the procedure
   * @param tr the {@link Translator}
   */
  @Override
  public void rewrite(LambdaExp lexp, Object formals, Object body, Translator tr,
                      TemplateScope templateScopeRest)
  {
    rewriteFormals(lexp, formals, tr, templateScopeRest);
    if (body instanceof PairWithPosition) {
      lexp.setFile(((PairWithPosition) body).getFileName());
    }
    rewriteBody(lexp, body, tr);
  }
  
  /**
   * Rewrite the formal parameters in a lambda expression.
   * 
   * @param lexp the lambda expression to rewrite.
   * @param formals the formal parameters list (or symbol)
   */
  @Override
  public void rewriteFormals (LambdaExp lexp, Object formals, Translator tr,
                              TemplateScope templateScopeRest)
  {
    if (lexp.getSymbol() == null)
    {
      String filename = lexp.getFileName();
      int line = lexp.getLineNumber();
      if (filename != null && line > 0)
        lexp.setSourceLocation(filename, line);
    }
    Object bindings = formals;
    Object mode = null;
    TemplateScope templateScope = templateScopeRest;
    int opt_args, rest_args, key_args, aux_args, allow_other_keys;
    opt_args = rest_args = aux_args = key_args = aux_args = allow_other_keys = -1;
    Pair pair;
    Object pair_car;
    //ArrayList<Symbol> keywords = null;
    ArrayList<Keyword> keywords = null;
    for (;; bindings = pair.getCdr())
    {
      if (bindings instanceof SyntaxForm)
      {
        SyntaxForm sf = (SyntaxForm) bindings;
        bindings = sf.getDatum();
        // The SyntaxForm "surrounds" both the current binding (the car),
        // as well as the cdr - i.e. the remaining bindings.
        templateScopeRest = sf.getScope();
      }
      if (!(bindings instanceof Pair))
        break;
      pair = (Pair) bindings;
      pair_car = pair.getCar();
      if (pair_car instanceof SyntaxForm)
      {
        SyntaxForm sf = (SyntaxForm) pair_car;
        pair_car = sf.getDatum();
        templateScope = sf.getScope();
      }

      if (pair_car == optionalKeyword)
      {
        if (opt_args >= 0)
          tr.syntaxError("multiple " + optionalKeyword + " keywords in parameter list");
        else if (rest_args >= 0 || key_args >= 0 || aux_args >= 0)
          tr.syntaxError(optionalKeyword + " after " + restKeyword + " or "
              + keyKeyword + " or " + auxKeyword);
        opt_args = 0;
      }
      else if (pair_car == restKeyword || pair_car == bodyKeyword)
      {
        if (rest_args >= 0)
          tr.syntaxError("multiple " + pair_car + " keywords in parameter list");
        else if (key_args >= 0 || aux_args >= 0)
          tr.syntaxError(pair_car + " after " + keyKeyword + " or " + auxKeyword);
        rest_args = 0;
      }
      else if (pair_car == keyKeyword)
      {
        if (key_args >= 0)
          tr.syntaxError("multiple " + keyKeyword + " keywords in parameter list");
        key_args = 0;
      }
      else if (pair_car == auxKeyword)
      {
        if (aux_args >= 0)
          tr.syntaxError("multiple " + auxKeyword + " keywords in parameter list");
        aux_args = 0;
      }
      else if (key_args >= 0)
        key_args++;
      else if (rest_args >= 0)
        rest_args++;
      else if (opt_args >= 0)
        opt_args++;
      else if (aux_args >= 0)
        aux_args++;
      else
        lexp.min_args++;
      if (pair_car == optionalKeyword || pair_car == restKeyword || pair_car == bodyKeyword
          || pair_car == keyKeyword || pair_car == auxKeyword)
      {
        mode = pair_car;
        continue;
      }
      Object savePos = tr.pushPositionOf(pair);
      Object name = null;
      Object defaultValue = defaultDefault;
      Object suppliedp = null;
      //Object keyname = null;
      Pair p = null;
      pair_car = tr.namespaceResolve(pair_car);
      if (pair_car instanceof Symbol)
      {
        name = pair_car;
      }
      else if (pair_car instanceof Pair)
      {
        p = (Pair) pair_car;
        pair_car = p.getCar();
        if (pair_car instanceof SyntaxForm)
        {
          SyntaxForm sf = (SyntaxForm) pair_car;
          pair_car = sf.getDatum();
          templateScope = sf.getScope();
        }
        pair_car = tr.namespaceResolve(pair_car);
        if (pair_car instanceof Symbol
            && p.getCdr() instanceof Pair)
        {
          name = pair_car;
          p = (Pair) p.getCdr();
          if (p != null && mode != null)
          {
            defaultValue = p.getCar();
            if (p.getCdr() instanceof Pair)
            {
              p = (Pair) p.getCdr();
              suppliedp = p.getCar();
            }
            if (p.getCdr() == LList.Empty)
              p = null;
            else
            {
              tr.syntaxError("improper list in specifier for parameter '"
                  + name + "')");
              break;
            }
          }
          if (p != null)
          { // no keyword parameters seen yet
            if (p.getCdr() != LList.Empty)
            {
              tr.syntaxError("junk at end of specifier for parameter `"
                  + name + "`" + ": " + p.getCdr());
              break;
            }
          }
        }
      }
      if (name == null && p != null)
      {
        tr.syntaxError("general symbols in keyword parameter not implemented");
        //        if ((pair_car = p.getCar()) instanceof Pair) {
        //          keyname = ((Pair) pair_car).getCar();
        //          if ((pair_car = ((Pair) pair_car).getCdr()) instanceof Pair) {
        //            name = ((Pair) pair_car).getCar();
        //          } else {
        //            tr.syntaxError("invalid named keyword syntax `" + p + "`");
        //          }
        //          if (p.getCdr() instanceof Pair) {
        //            p = (Pair) p.getCdr();
        //            defaultValue = p.getCar();
        //            if (p.getCdr() instanceof Pair) {
        //              p = (Pair) p.getCdr();
        //              suppliedp = p.getCar();
        //            }
        //          }
        //        } else {
        //          tr.syntaxError("malformed lambda list `" + p + "`");
        //        }
      }
      Declaration decl = new Declaration(name);
      decl.setFlag(Declaration.IS_PARAMETER);
      //if (suppliedp != null || keyname != null) {

      if (mode == optionalKeyword || mode == keyKeyword || mode == auxKeyword)
      {
        //if (suppliedp != null || keyname != null) {
        decl.setInitValue(new LangExp(defaultValue));

        if (mode == keyKeyword)
        {
          if (keywords == null)
            keywords = new ArrayList<Keyword>();
          //          if (keyname != null) {
          //            keywords.add(((Symbol) keyname));
          //          } else {
          keywords.add(Keyword.make(name instanceof Symbol
              ? ((Symbol) name).getName()
              : name.toString()));
          //          }
        }
      }
      Translator.setLine(decl, bindings);
      if (mode == restKeyword || mode == bodyKeyword) {
          decl.setType(LangObjType.listType);
          decl.setFlag(Declaration.IS_REST_PARAMETER);
      }
      decl.setFlag(Declaration.IS_SINGLE_VALUE);
      addParam(decl, templateScope, lexp, tr);
      if (suppliedp != null) {
          decl.setFlag(Declaration.IS_SUPPLIED_PARAMETER);
          Declaration suppliedDecl = new Declaration(suppliedp);
          suppliedDecl.setFlag(Declaration.IS_SUPPLIED_PARAMETER|Declaration.IS_PARAMETER);
          suppliedDecl.setType(Type.booleanType);
          addParam(suppliedDecl, templateScope, lexp, tr);
      }
      tr.popPositionOf(savePos);
    }
    if (bindings instanceof SyntaxForm)
    {
      SyntaxForm sf = (SyntaxForm) bindings;
      bindings = sf.getDatum();
      templateScopeRest = sf.getScope();
    }
    if (bindings instanceof Symbol)
    {
      if (opt_args >= 0 || key_args >= 0 || aux_args >= 0)
      {
        tr.syntaxError("dotted rest-arg after " + optionalKeyword
            + ", " + restKeyword + ", or " + keyKeyword
            + ", or " + auxKeyword);
      }
      else
      {
        rest_args = 1;
        Declaration decl = new Declaration(bindings);
        decl.setType(LangObjType.listType);
        decl.setFlag(Declaration.IS_SINGLE_VALUE);
        decl.noteValueUnknown();
        addParam(decl, templateScopeRest, lexp, tr);
      }
    }
    else if (bindings != LList.Empty)
    {
      tr.syntaxError("misformed formals in lambda");
    }
    if (rest_args > 1)
    {
      tr.syntaxError("multiple " + restKeyword + " parameters");
      rest_args = 1;
    }
    if (opt_args < 0)
      opt_args = 0;
    if (rest_args < 0)
      rest_args = 0;
    if (key_args < 0)
      key_args = 0;
    if (aux_args < 0)
      aux_args = 0;
    if (rest_args > 0)
      lexp.max_args = -1;
    else // Is this useful?
      lexp.max_args = lexp.min_args + opt_args + 2 * key_args;
    lexp.opt_args = opt_args;
    if (keywords != null)
      lexp.keywords = keywords.toArray(new Keyword[keywords.size()]);
  }
  
  @Override
  public void print (Consumer out)
  {
    out.write("#<BUILTIN LAMBDA>");
  }
}
