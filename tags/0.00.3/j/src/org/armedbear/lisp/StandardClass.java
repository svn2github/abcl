/*
 * StandardClass.java
 *
 * Copyright (C) 2003 Peter Graves
 * $Id: StandardClass.java,v 1.11 2003-10-11 18:48:46 piso Exp $
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

public class StandardClass extends LispClass
{
    private LispObject directSlots = NIL;
    private LispObject effectiveSlots = NIL;

    public StandardClass()
    {
    }

    public StandardClass(Symbol symbol, LispObject directSuperclasses)
    {
        super(symbol, directSuperclasses);
    }

    public LispObject typeOf()
    {
        return Symbol.STANDARD_CLASS;
    }

    public LispClass classOf()
    {
        return BuiltInClass.STANDARD_CLASS;
    }

    public LispObject typep(LispObject type) throws ConditionThrowable
    {
        if (type == Symbol.STANDARD_CLASS)
            return T;
        if (type == BuiltInClass.STANDARD_CLASS)
            return T;
        return super.typep(type);
    }

    public String toString()
    {
        StringBuffer sb = new StringBuffer("#<STANDARD-CLASS ");
        sb.append(symbol.getName());
        sb.append('>');
        return sb.toString();
    }

    // ### class-direct-slots
    private static final Primitive1 CLASS_DIRECT_SLOTS =
        new Primitive1("class-direct-slots", PACKAGE_SYS, false)
    {
        public LispObject execute(LispObject arg)
            throws ConditionThrowable
        {
            if (arg instanceof StandardClass)
                return ((StandardClass)arg).directSlots;
            if (arg instanceof BuiltInClass)
                return NIL;
            throw new ConditionThrowable(new TypeError(arg, "standard class"));
        }
    };

    // ### %set-class-direct-slots
    private static final Primitive2 _SET_CLASS_DIRECT_SLOTS =
        new Primitive2("%set-class-direct-slots", PACKAGE_SYS, false)
    {
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            if (first instanceof StandardClass) {
                ((StandardClass)first).directSlots = second;
                return second;
            }
            throw new ConditionThrowable(new TypeError(first, "standard class"));
        }
    };

    // ### class-slots
    private static final Primitive1 CLASS_SLOTS =
        new Primitive1("class-slots", PACKAGE_SYS, false)
    {
        public LispObject execute(LispObject arg)
            throws ConditionThrowable
        {
            if (arg instanceof StandardClass)
                return ((StandardClass)arg).effectiveSlots;
            if (arg instanceof BuiltInClass)
                return NIL;
            throw new ConditionThrowable(new TypeError(arg, "standard class"));
        }
    };

    // ### %set-class-slots
    private static final Primitive2 _SET_CLASS_SLOTS =
        new Primitive2("%set-class-slots", PACKAGE_SYS, false)
    {
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            if (first instanceof StandardClass) {
                ((StandardClass)first).effectiveSlots = second;
                return second;
            }
            throw new ConditionThrowable(new TypeError(first, "standard class"));
        }
    };
}
