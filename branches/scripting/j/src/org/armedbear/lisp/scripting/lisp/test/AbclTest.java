package org.armedbear.lisp.scripting;

import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.io.StringWriter;

import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.ScriptException;
import javax.script.SimpleScriptContext;

import junit.framework.TestCase;

import org.armedbear.lisp.ConditionThrowable;
import org.armedbear.lisp.Cons;
import org.armedbear.lisp.Fixnum;
import org.armedbear.lisp.Lisp;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.scripting.AbclScriptEngine;

public class AbclTest extends TestCase {

	private static AbclScriptEngine engine = new AbclScriptEngine(false);
	
	public void testBindings() {
		try {
			engine.put("foo", 42);
			assertEquals(new Integer(42), engine.eval("foo"));
			engine.eval("(setq foo 45)");
			assertEquals(new Integer(45), engine.get("foo"));
		} catch (ScriptException e) {
			e.printStackTrace();
			fail("Exception was thrown.");
		}
	}
	
	public void testContext() {
		try {
			SimpleScriptContext ctx = new SimpleScriptContext();
			ctx.setReader(new StringReader("\"test\""));
			StringWriter out = new StringWriter();
			ctx.setWriter(out);
			Bindings bindings = engine.createBindings();
			ctx.setBindings(bindings, ScriptContext.ENGINE_SCOPE);
			
			bindings.put("bar", 42);
			assertEquals(new Integer(42), engine.eval("bar", ctx));
			engine.eval("(setq bar 45)", ctx);
			assertEquals(new Integer(45), bindings.get("bar"));
			
			engine.eval("(princ (read))", ctx);
			assertEquals("test", out.toString());
		} catch (ScriptException e) {
			e.printStackTrace();
			fail("Exception was thrown.");
		}		
	}

	public void testFunctions() {
		try {
			assertEquals(42, ((Fixnum) engine.invokeFunction("+", 40, 2)).value);
			assertEquals(Lisp.NIL, engine.invokeFunction("car", new Cons(Lisp.NIL, Lisp.NIL)));
			assertEquals(9, ((Fixnum) engine.invokeFunction("length", "megaceppa")).value);
		} catch (Throwable t) {
			t.printStackTrace();
			fail("Exception: " + t);
		}
	}
	
	public void testInterface() {
		try {
			engine.eval("(define-java-interface-implementation \"java.lang.Comparable\" \"compareTo\" (lambda (obj) 42))");
			Comparable comp = engine.getInterface(Comparable.class);
			assertEquals(42, comp.compareTo(null));
		} catch (Exception e) {
			e.printStackTrace();
			fail("Exception: " + e);
		}
	}
	
	public static void main(String[] args) {
		AbclScriptEngine engine = new AbclScriptEngine(false);
		try {
			//System.out.println(((LispObject) engine.eval("(print (read))")).writeToString());
			SimpleScriptContext ctx = new SimpleScriptContext();
			ctx.setReader(new InputStreamReader(System.in));
			ctx.setWriter(new OutputStreamWriter(System.out));
			Bindings bindings = engine.createBindings();
			bindings.put("x", 3);
			ctx.setBindings(bindings, ctx.ENGINE_SCOPE);
			engine.eval("(print \"Hello, World!\")");
			System.out.println("EVAL returned: " + ((LispObject) engine.eval("(print x) (print (jcall (jmethod \"java.lang.Integer\" \"intValue\") x)) (print (type-of (jcall (jmethod \"java.lang.Integer\" \"intValue\") x))) (print 4)", ctx)).writeToString());
			engine.put("y", 42);
			System.out.println("EVAL returned: " + ((LispObject) engine.eval("(print y) (print (jcall (jmethod \"java.lang.Integer\" \"intValue\") y)) (setq y 45)")).writeToString());
			System.out.println("y = " + engine.get("y"));
		} catch (ScriptException e) {
			e.printStackTrace();
		} catch (ConditionThrowable e) {
			e.printStackTrace();
		}
	}
	
}
