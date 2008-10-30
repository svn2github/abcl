/*
 * JProxy.java
 *
 * Copyright (C) 2002-2005 Peter Graves, Andras Simon
 * $Id$
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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.HashMap;
import java.util.Map;
import java.util.WeakHashMap;

public final class JProxy extends Lisp
{
  private static final Map<Object,Entry> table = new WeakHashMap<Object,Entry>();

  // ### %jnew-proxy interface &rest method-names-and-defs
  private static final Primitive _JNEW_PROXY =
    new Primitive("%jnew-proxy", PACKAGE_JAVA, false,
                  "interface &rest method-names-and-defs")
    {
      public LispObject execute(LispObject[] args) throws ConditionThrowable
      {
        int length = args.length;
        if (length < 3 || length % 2 != 1)
          return error(new WrongNumberOfArgumentsException(this));
        Map<String,Function> lispDefinedMethods = new HashMap<String,Function>();
        for (int i = 1; i < length; i += 2)
          lispDefinedMethods.put(args[i].getStringValue(),
                                 (Function) args[i + 1]);
        Class iface = (Class) args[0].javaInstance();
        Object proxy = Proxy.newProxyInstance(iface.getClassLoader(),
                                              new Class[] { iface },
                                              new LispHandler(table));
        table.put(proxy, new Entry(iface, lispDefinedMethods));
        return new JavaObject(proxy);
      }
    };

  private static class LispHandler implements InvocationHandler
  {
    Map table;

    LispHandler (Map table)
    {
      this.table = table;
    }

    public Object invoke(Object proxy, Method method, Object[] args)
    {
      String methodName = method.getName();

      if (methodName.equals("hashCode"))
          return new Integer(System.identityHashCode(proxy));
      if (methodName.equals("equals"))
        return (proxy == args[0] ? Boolean.TRUE : Boolean.FALSE);
      if (methodName.equals("toString"))
        return proxy.getClass().getName() + '@' + Integer.toHexString(proxy.hashCode());

      if (table.containsKey(proxy))
        {
          Entry entry = (Entry) table.get(proxy);
          Function f = entry.getLispMethod(methodName);
          if (f != null)
            {
              try
                {
                  LispObject lispArgs = NIL;
                  if (args != null)
                    {
                      for (int i = args.length - 1 ; 0 <= i  ; i--)
                        lispArgs = lispArgs.push(new JavaObject(args[i]));
                    }
                  LispObject result = evalCall(f, lispArgs, new Environment(),
                                               LispThread.currentThread());
                  return (method.getReturnType() == void.class ? null : result.javaInstance());
                }
              catch (ConditionThrowable t)
                {
                  t.printStackTrace();
                }
            }
        }
      return null;
    }
  }

  private static class Entry
  {
    Class iface;
    Map lispDefinedMethods;

    public Entry (Class iface, Map lispDefinedMethods)
    {
      this.iface = iface;
      this.lispDefinedMethods = lispDefinedMethods;
    }

    public Function getLispMethod(String methodName)
    {
      if (lispDefinedMethods.containsKey(methodName))
        return (Function)lispDefinedMethods.get(methodName);
      return null;
    }
  }
  
  	//NEW IMPLEMENTATION by Alessio Stalla 
  
  	public static class LispInvocationHandler implements InvocationHandler {
  		
  		private Function function;
  		private static Method hashCodeMethod;
  		private static Method equalsMethod;
  		private static Method toStringMethod;
  		
  		static {
  			try {
				hashCodeMethod = Object.class.getMethod("hashCode", new Class[] {});
				equalsMethod = Object.class.getMethod("equals", new Class[] { Object.class });
				toStringMethod = Object.class.getMethod("toString", new Class[] {});
			} catch (Exception e) {
				throw new Error("Something got horribly wrong - can't get a method from Object.class", e);
			}
  		}

  		public LispInvocationHandler(Function function) {
  			this.function = function;
  		}
  		
		@Override
		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
	    	if(hashCodeMethod.equals(method)) {
	    		return proxy.hashCode();
	    	}
	    	if(equalsMethod.equals(method)) {
	    		return proxy.equals(args[0]);
	    	}
	    	if(toStringMethod.equals(method)) {
	    		return proxy.toString();
	    	}
	    	
	    	if(args == null) {
	    		args = new Object[0];
	    	}
			LispObject[] lispArgs = new LispObject[args.length + 2];
			lispArgs[0] = toLispObject(proxy);
			lispArgs[1] = new JavaObject(method);
			for(int i = 0; i < args.length; i++) {
				lispArgs[i + 2] = toLispObject(args[i]);
			}
			Object retVal = (function.execute(lispArgs)).javaInstance();
			/* DOES NOT WORK due to autoboxing!
			if(retVal != null && !method.getReturnType().isAssignableFrom(retVal.getClass())) {
				return error(new TypeError(new JavaObject(retVal), new JavaObject(method.getReturnType())));
			}*/
			return retVal;
		}
	}
  
  	private static final Primitive _JMAKE_INVOCATION_HANDLER =
	    new Primitive("%jmake-invocation-handler", PACKAGE_JAVA, false,
	                  "function") {
		
	      	public LispObject execute(LispObject[] args) throws ConditionThrowable {
	      		int length = args.length;
	      		if (length != 1) {
	      			return error(new WrongNumberOfArgumentsException(this));
	      		}
	      		if(!(args[0] instanceof Function)) {
	      			return error(new TypeError(args[0], Symbol.FUNCTION));
	      		}
	      		
	      		return new JavaObject(new LispInvocationHandler((Function) args[0]));
	      	}
	    };

    private static final Primitive _JMAKE_PROXY =
	    new Primitive("%jmake-proxy", PACKAGE_JAVA, false,
	                  "interface invocation-handler") {
		
	      	public LispObject execute(final LispObject[] args) throws ConditionThrowable {
	      		int length = args.length;
	      		if (length != 2) {
	      			return error(new WrongNumberOfArgumentsException(this));
	      		}
	      		if(!(args[0] instanceof JavaObject) ||
	      		   !(((JavaObject) args[0]).javaInstance() instanceof Class)) {
	      			return error(new TypeError(args[0], new SimpleString(Class.class.getName())));
	      		}
	      		if(!(args[1] instanceof JavaObject) ||
 	      		   !(((JavaObject) args[1]).javaInstance() instanceof InvocationHandler)) {
	 	      			return error(new TypeError(args[1], new SimpleString(InvocationHandler.class.getName())));
	 	      		}
	      		Class<?> iface = (Class<?>) ((JavaObject) args[0]).javaInstance();
	      		InvocationHandler invocationHandler = (InvocationHandler) ((JavaObject) args[1]).javaInstance(); 
	      		Object proxy = Proxy.newProxyInstance(
	      				iface.getClassLoader(),
	      				new Class[] { iface },
	      				invocationHandler);
	      		return new JavaObject(proxy);
	      	}
	    };    
	    
	private static LispObject toLispObject(Object obj) {
		return (obj instanceof LispObject) ? (LispObject) obj : new JavaObject(obj);
	}    
	    
  	private static final Primitive _JIMPLEMENT_INTERFACE =
	    new Primitive("%jimplement-interface", PACKAGE_JAVA, false,
	                  "interface &rest method-names-and-defs") {
  		
	      	public LispObject execute(LispObject[] args) throws ConditionThrowable {
	      		int length = args.length;
	      		if (length < 3 || length % 2 != 1) {
	      			return error(new WrongNumberOfArgumentsException(this));
	      		}
	      		final Map<String,Function> lispDefinedMethods = new HashMap<String,Function>();
	      		for (int i = 1; i < length; i += 2) {
	      			lispDefinedMethods.put(args[i].getStringValue(), (Function) args[i + 1]);
	      		}
	      		final Class<?> iface = (Class<?>) args[0].javaInstance();
	      		return new Function() {

	      			public LispObject execute(LispObject lispProxy) {
	      				Object proxy = Proxy.newProxyInstance(
	    	      				iface.getClassLoader(),
	    	      				new Class[] { iface },
	    	      				new LispHandler2(lispProxy, lispDefinedMethods));
	    	      		return new JavaObject(proxy);	      				
	      			}
	      			
	      		};
	      		
	      	}
	    };
  
    private static class LispHandler2 implements InvocationHandler {

  		private Map<String, Function> lispDefinedMethods;
  		private LispObject lispProxy;

	    LispHandler2(LispObject lispProxy, Map<String, Function> lispDefinedMethods) {
	    	this.lispProxy = lispProxy;
	    	this.lispDefinedMethods = lispDefinedMethods;
	    }
		
	    public Object invoke(Object proxy, Method method, Object[] args) throws ConditionThrowable {
	    	String methodName = method.getName();
	
	    	//TODO are these implemented correctly?
	    	if(methodName.equals("hashCode")) {
	    		return lispProxy.hashCode();
	    	}
	    	if (methodName.equals("equals")) {
	    		return (args[0] instanceof LispObject) && (T == lispProxy.EQ((LispObject) args[0]));
	    	}
	    	if (methodName.equals("toString")) {
	    		return lispProxy.writeToString();
	    	}	

	    	Function f = lispDefinedMethods.get(methodName);
	    	if (f != null) {
	    		try {
	    			LispObject lispArgs = NIL;
	    			if (args != null) {
	    				for (int i = args.length - 1 ; 0 <= i  ; i--) {
	    					lispArgs = lispArgs.push(new JavaObject(args[i]));
	    				}
	    			}
	    			lispArgs = lispArgs.push(lispProxy);
	    			LispObject result = evalCall(f, lispArgs, new Environment(),
	    										 LispThread.currentThread());
	    			return (method.getReturnType() == void.class ? null : result.javaInstance());
	    		} catch (ConditionThrowable t) {
	    			t.printStackTrace();
	    		}
        	}
	    	return null;
	    }
  	}	    
	    
}
