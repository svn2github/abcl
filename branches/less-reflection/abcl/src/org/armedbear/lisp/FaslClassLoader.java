/*
 * JavaClassLoader.java
 *
 * Copyright (C) 2010 Alessio Stalla
 * $Id: JavaClassLoader.java 12298 2009-12-18 21:50:54Z ehuelsmann $
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
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce an
 * executable, regardless of the license terms of these independent
 * modules, and to copy and distribute the resulting executable under
 * terms of your choice, provided that you also meet, for each linked
 * independent module, the terms and conditions of the license of that
 * module.  An independent module is a module which is not derived from
 * or based on this library.  If you modify this library, you may extend
 * this exception to your version of the library, but you are not
 * obligated to do so.  If you do not wish to do so, delete this
 * exception statement from your version.
 */

package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

import java.util.*;

public class FaslClassLoader extends JavaClassLoader {
    
    protected Class<?> findClass(String name) throws ClassNotFoundException {
	try {
	    Pathname pathname = new Pathname(name.substring("org/armedbear/lisp/".length()) + ".cls");
	    byte[] b = readFunctionBytes(pathname);
	    return defineClass(name, b, 0, b.length);
	} catch(Throwable e) { //TODO handle this better, readFunctionBytes uses Debug.assert() but should return null
	    e.printStackTrace();
	    if(e instanceof ControlTransfer) { throw (ControlTransfer) e; }
	    throw new ClassNotFoundException("Function class not found: " + name, e);
	}
    }

    //TODO have compiler generate subclass, TEST ONLY!!!
    protected Map<String, LispObject> functions = new HashMap<String, LispObject>();

    public LispObject loadFunction(String className) {
	try {
	    LispObject o = (LispObject) loadClass(className).newInstance();
	    functions.put(className, o);
	    return o;
	} catch(Exception e) {
	    e.printStackTrace();
	    if(e instanceof ControlTransfer) { throw (ControlTransfer) e; }
	    throw new RuntimeException(e);
	}
    }
    
    public LispObject getFunction(final String className) {
	LispObject o = functions.get(className);
	if(o == null) {
	    o = loadFunction(className);
	}
	return o;
    }

    public static LispObject faslLoadFunction(String className) {
	FaslClassLoader cl = (FaslClassLoader) LispThread.currentThread().safeSymbolValue(_FASL_LOADER_).javaInstance();
	return cl.getFunction(className);
    }

    private static final Primitive MAKE_FASL_CLASS_LOADER = new pf_make_fasl_class_loader();
    private static final class pf_make_fasl_class_loader extends Primitive {
	pf_make_fasl_class_loader() {
            super("make-fasl-class-loader", PACKAGE_SYS, false, "");
        }

        @Override
        public LispObject execute() {
            return new JavaObject(new FaslClassLoader());
        }
    };

    private static final Primitive GET_FASL_FUNCTION = new pf_get_fasl_function();
    private static final class pf_get_fasl_function extends Primitive {
	pf_get_fasl_function() {
            super("get-fasl-function", PACKAGE_SYS, false, "loader class-name");
        }

        @Override
        public LispObject execute(LispObject loader, LispObject className) {
            FaslClassLoader l = (FaslClassLoader) loader.javaInstance(FaslClassLoader.class);
	    return l.getFunction("org.armedbear.lisp." + className.getStringValue());
        }
    };

}