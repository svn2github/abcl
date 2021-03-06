/*
 * CompiledFunction.java
 *
 * Copyright (C) 2003 Peter Graves
 * $Id: CompiledFunction.java,v 1.13 2003-09-19 14:44:10 piso Exp $
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package org.armedbear.lisp;

public class CompiledFunction extends Closure
{
    public CompiledFunction(String name, LispObject lambdaList,
                            LispObject body, Environment env)
        throws ConditionThrowable
    {
        super(name, lambdaList, body, env);
    }

    public LispObject typep(LispObject typeSpecifier) throws ConditionThrowable
    {
        if (typeSpecifier == Symbol.COMPILED_FUNCTION)
            return T;
        return super.typep(typeSpecifier);
    }

    public LispObject execute(LispObject arg) throws ConditionThrowable
    {
        LispObject[] args = new LispObject[1];
        args[0] = arg;
        return execute(args);
    }

    public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
    {
        LispObject[] args = new LispObject[2];
        args[0] = first;
        args[1] = second;
        return execute(args);
    }

    public LispObject execute(LispObject first, LispObject second,
                              LispObject third)
        throws ConditionThrowable
    {
        LispObject[] args = new LispObject[3];
        args[0] = first;
        args[1] = second;
        args[2] = third;
        return execute(args);
    }

    public LispObject execute(LispObject[] args) throws ConditionThrowable
    {
        throw new ConditionThrowable(new LispError("not implemented"));
    }

    public String toString()
    {
        StringBuffer sb = new StringBuffer("#<COMPILED-FUNCTION @ ");
        sb.append(Integer.toHexString(hashCode()));
        sb.append(">");
        return sb.toString();
    }

    // ### make-compiled-function
    // make-compiled-function name lambda-list body => object
    private static final Primitive3 MAKE_COMPILED_FUNCTION =
        new Primitive3("make-compiled-function", PACKAGE_SYS, false) {
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)
            throws ConditionThrowable
        {
            String name;
            if (first == NIL)
                name = null;
            else
                name = LispString.getValue(first);
            LispObject lambdaList = second;
            LispObject body = third;
            return new CompiledFunction(name, lambdaList, body,
                                        new Environment());
        }
    };

    // ### load-compiled-function
    private static final Primitive1 LOAD_COMPILED_FUNCTION =
        new Primitive1("load-compiled-function", PACKAGE_SYS, false) {
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            String className = ((LispString)arg).getValue();
            if (className.endsWith(".class")) {
                try {
                    JavaClassLoader loader = new JavaClassLoader();
                    Class c = loader.loadClassFromFile(className);
                    if (c != null) {
                        Class[] parameterTypes = new Class[0];
                        java.lang.reflect.Constructor constructor =
                            c.getConstructor(parameterTypes);
                        Object[] initargs = new Object[0];
                        LispObject obj =
                            (LispObject) constructor.newInstance(initargs);
                        return obj;
                    }
                }
                catch (VerifyError e) {
                    throw new ConditionThrowable(new LispError("class verification failed: " +
                                                               e.getMessage()));
                }
                catch (Throwable t) {
                    Debug.trace(t);
                }
            }
            throw new ConditionThrowable(new LispError("unable to load ".concat(className)));
        }
    };

    private static final Primitive1 VARLIST =
        new Primitive1("varlist", PACKAGE_SYS, false) {
        public LispObject execute(LispObject arg) throws ConditionThrowable {
            if (arg instanceof CompiledFunction)
                return ((CompiledFunction)arg).getVariableList();
            throw new ConditionThrowable(new TypeError(arg, "compiled function"));
        }
    };
}
