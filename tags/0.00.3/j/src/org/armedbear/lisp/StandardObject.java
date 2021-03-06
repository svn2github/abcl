/*
 * StandardObject.java
 *
 * Copyright (C) 2003 Peter Graves
 * $Id: StandardObject.java,v 1.9 2003-10-13 14:11:52 piso Exp $
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

public class StandardObject extends LispObject
{
    // Slots.
    private LispClass cls;
    private LispObject slots; // A simple vector.

    protected StandardObject()
    {
    }

    protected StandardObject(LispClass cls, LispObject slots)
    {
        this.cls = cls;
        this.slots = slots;
    }

    public LispObject typeOf()
    {
        return cls != null ? cls.getSymbol() : Symbol.STANDARD_OBJECT;
    }

    public LispClass classOf()
    {
        return cls != null ? cls : BuiltInClass.STANDARD_OBJECT;
    }

    public LispObject typep(LispObject type) throws ConditionThrowable
    {
        if (type == Symbol.STANDARD_OBJECT)
            return T;
        if (type == BuiltInClass.STANDARD_OBJECT)
            return T;
        if (cls != null) {
            if (type == cls)
                return T;
            if (type == cls.getSymbol())
                return T;
            LispObject cpl = cls.getCPL();
            while (cpl != NIL) {
                if (type == cpl.car())
                    return T;
                if (type == ((LispClass)cpl.car()).getSymbol())
                    return T;
                cpl = cpl.cdr();
            }
        }
        return super.typep(type);
    }

    public String toString()
    {
        StringBuffer sb = new StringBuffer("#<");
        if (cls != null)
            sb.append(cls.getSymbol().getName());
        else
            sb.append("STANDARD-OBJECT");
        sb.append(" @ #x");
        sb.append(Integer.toHexString(hashCode()));
        sb.append(">");
        return sb.toString();
    }

    // ### std-instance-class
    private static final Primitive1 STD_INSTANCE_CLASS =
        new Primitive1("std-instance-class", PACKAGE_SYS, false)
    {
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            if (arg instanceof StandardObject)
                return ((StandardObject)arg).cls;
            throw new ConditionThrowable(new TypeError(arg, "standard object"));
        }
    };

    // ### %set-std-instance-class
    private static final Primitive2 _SET_STD_INSTANCE_CLASS =
        new Primitive2("%set-std-instance-class", PACKAGE_SYS, false)
    {
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            if (first instanceof StandardObject) {
                ((StandardObject)first).cls = (LispClass) second;
                return second;
            }
            throw new ConditionThrowable(new TypeError(first, "standard object"));
        }
    };

    // ### std-instance-slots
    private static final Primitive1 STD_INSTANCE_SLOTS =
        new Primitive1("std-instance-slots", PACKAGE_SYS, false)
    {
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            if (arg instanceof StandardObject)
                return ((StandardObject)arg).slots;
            throw new ConditionThrowable(new TypeError(arg, "standard object"));
        }
    };

    // ### %set-std-instance-slots
    private static final Primitive2 _SET_STD_INSTANCE_SLOTS =
        new Primitive2("%set-std-instance-slots", PACKAGE_SYS, false)
    {
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            if (first instanceof StandardObject) {
                ((StandardObject)first).slots = second;
                return second;
            }
            throw new ConditionThrowable(new TypeError(first, "standard object"));
        }
    };

    // ### allocate-std-instance
    // allocate-std-instance class slots => instance
    private static final Primitive2 ALLOCATE_STD_INSTANCE =
        new Primitive2("allocate-std-instance", PACKAGE_SYS, false)
    {
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            if (first == BuiltInClass.STANDARD_CLASS)
                return new StandardClass();
            if (first instanceof LispClass) {
                Symbol symbol = ((LispClass)first).getSymbol();
                if (symbol == Symbol.STANDARD_GENERIC_FUNCTION)
                    return new GenericFunction((LispClass)first, second);
                return new StandardObject((LispClass)first, second);
            }
            throw new ConditionThrowable(new TypeError(first, "class"));
        }
    };
}
