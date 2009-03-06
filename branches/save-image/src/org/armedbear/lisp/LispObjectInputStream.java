package org.armedbear.lisp;

import java.io.*;

public class LispObjectInputStream extends ObjectInputStream {

    public LispObjectInputStream(InputStream in) throws IOException {
	super(in);
    }

    /*
    protected Class resolveClass(ObjectStreamClass desc) throws IOException, ClassNotFoundException {
	String name = desc.getName();
	if(name != null && name.contains("ABCL_GENERATED_")) {
	    try {
		byte[] bytes = (byte[]) readObject();
		return (new JavaClassLoader()).loadClassFromByteArray(null, bytes, 0, bytes.length);
	    } catch(java.io.OptionalDataException e) {
		System.out.println("AAAAA " + e.eof + " " + e.length);
		throw new ClassNotFoundException();
	    }

	}
	try {
	    return Class.forName(name, false, JavaClassLoader.getPersistentInstance());
	} catch (ClassNotFoundException ex) {
	    return super.resolveClass(desc);
	}
    }

    private void writeObject(java.io.ObjectOutputStream stream) throws java.io.IOException {
	if(getClass().getSimpleName().contains("ABCL_GENERATED_")) {
	    try {
		stream.writeObject(getf(propertyList, Symbol.CLASS_BYTES, new JavaObject(new byte[0])).javaInstance());
	    } catch(ConditionThrowable c) {
		throw new java.io.InvalidClassException(getClass().getName());
	    }
	}
	stream.defaultWriteObject();
    }
    */

}