package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.bytecode.Type;

/** A Constraint whose value is that of a named field/method of an object.
 * The object used is the owning Binding's value.
 * (For now, only fields are supported.)
 */

public class ClassMemberConstraint extends Constraint
{
  Type type;
  String name;
  gnu.bytecode.Field field;
  java.lang.reflect.Field rfield;

  public ClassMemberConstraint(Type type, String name)
  {
    this.type = type;
    this.name = name;
  }

  public ClassMemberConstraint(Class clas, String name)
  {
    this.type = Type.make(clas);
    this.name = name;
  }

  public ClassMemberConstraint(Type type, gnu.bytecode.Field field)
  {
    this.type = type;
    this.field = field;
    this.name = field.getName();
  }

  public ClassMemberConstraint(java.lang.reflect.Field field)
  {
    this.rfield = field;
    this.name = field.getName();
  }

  void setup()
  {
    if (rfield == null)
      {
        Class clas = type.getReflectClass();
        try
          {
            rfield = clas.getField(name);
          }
        catch (java.lang.NoSuchFieldException ex)
          {
          }
      }
  }

  public Object get (Binding binding)
  {
    setup();
    try
      {
        return rfield.get(getValue(binding));
      }
    catch (IllegalAccessException ex)
      {
        throw new WrappedException(ex);
      }
  }

  public void set (Binding binding, Object value)
  {
    setup();
    try
      {
        rfield.set(getValue(binding), value);
      }
    catch (IllegalAccessException ex)
      {
        throw new WrappedException(ex);
      }
  }

  public static void define (String name, Object object, String fname)
  {
    define(name, object, fname, Environment.getCurrent());
  }

  public static void define (String name, Object object, String fname,
                             Environment env)
  {
    Binding binding = env.getBinding(name);
    synchronized (binding)
      {
	setValue(binding, object);
	setConstraint(binding,
                      new ClassMemberConstraint(object.getClass(), fname));
      }
  }

  public static void define (String name, Object object,
                             gnu.bytecode.Field field, Environment env)
  {
    Binding binding = env.getBinding(name);
    synchronized (binding)
      {
	setValue(binding, object);
	setConstraint(binding,
                      new ClassMemberConstraint(field.getType(), field));
      }
  }

  public static void define (String name, Object object,
                             java.lang.reflect.Field field, Environment env)
  {
    if ((field.getModifiers() & java.lang.reflect.Modifier.FINAL) != 0)
      {
	try
	  {
	    Object value = field.get(object);
	    if (value instanceof Binding)
	      {
		env.addBinding((Binding) value);
		return;
	      }
	    if (value instanceof Named)
	      name = ((Named) value).getName();

	    // The problem with the following is that we can't catch
	    // set! to a constant (defined using define-contsant).  (Note we
	    // do want to allow a new define, at leastwhen interactive.)
	    // However, if we always use a ClassMemberConstraint then
	    // some hitherto-valid Scheme programs will break:  Since it
	    // will also prohibit re-assigning to a procedure defined
	    // using (define (foo ...) ...) since that gets compiled to a
	    // final Procedure field.  FIXME.
	    if (true)
	      {
		name = name.intern();
		env.define(name, value);
		return;
	      }
	  }
	catch (Exception ex)
	  {
	    throw new WrappedException("error accessing field "+field, ex);
	  }
      }
    name = name.intern();
    Binding binding = new Binding(name);
    setValue(binding, object);
    setConstraint(binding, new ClassMemberConstraint(field));
    env.addBinding(binding);
  }

  /** Import all the public fields of an object. */
  public static void defineAll(Object object, Environment env)
  {
    Class clas = object.getClass();
    java.lang.reflect.Field[] fields = clas.getFields();
    for (int i = fields.length;  --i >= 0; )
      {
	java.lang.reflect.Field field = fields[i];
	define(field.getName(), object, field, env);
      }
  }
}
