package org.armedbear.lisp;

import java.io.*;

public class ExternalizedCompiledFunction extends Lisp implements Serializable {

    
    private String functionName;
    private String className;
    private byte[] classBytes;

    public ExternalizedCompiledFunction(byte[] classBytes, String functionName, String className) {
	this.classBytes = classBytes;
	this.functionName = functionName;
	this.className = className;
    }

    protected Object readResolve() throws ObjectStreamException {
	Object o = null;
	try {
	    o = loadCompiledFunction(classBytes);
	    if(o instanceof Function) {
		return o;
	    }
	} catch(Throwable t) {
	    System.err.println("Error deserializing compiled function");
	    t.printStackTrace(System.err);
	}
	throw new InvalidClassException(o != null ? o.getClass().getName() : "null");
    }

    public String getFunctionName() {
	return functionName;
    }

    public String getClassName() {
	return className;
    }
    
}