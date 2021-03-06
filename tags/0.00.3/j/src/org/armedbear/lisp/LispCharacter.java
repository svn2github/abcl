/*
 * LispCharacter.java
 *
 * Copyright (C) 2002-2003 Peter Graves
 * $Id: LispCharacter.java,v 1.29 2003-09-29 16:20:55 piso Exp $
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

public final class LispCharacter extends LispObject
{
    private static final LispCharacter[] characters = new LispCharacter[128];

    static {
        for (int i = characters.length; i-- > 0;)
            characters[i] = new LispCharacter((char)i);
    }

    private final char c;

    public static LispCharacter getInstance(char c)
    {
        try {
            return characters[c];
        }
        catch (ArrayIndexOutOfBoundsException e) {
            return new LispCharacter(c);
        }
    }

    private LispCharacter(char c)
    {
        this.c = c;
    }

    public LispObject typeOf()
    {
        return Symbol.CHARACTER;
    }

    public LispClass classOf()
    {
        return BuiltInClass.CHARACTER;
    }

    public LispObject typep(LispObject type) throws ConditionThrowable
    {
        if (type == Symbol.CHARACTER)
            return T;
        if (type == BuiltInClass.CHARACTER)
            return T;
        if (type == Symbol.BASE_CHAR)
            return T;
        if (type == Symbol.STANDARD_CHAR)
            return isStandardChar();
        return super.typep(type);
    }

    public LispObject isStandardChar()
    {
        if (c >= ' ' && c < 127)
            return T;
        if (c == '\n')
            return T;
        return NIL;
    }

    public boolean eql(LispObject obj)
    {
        if (this == obj)
            return true;
        if (obj instanceof LispCharacter) {
            if (c == ((LispCharacter)obj).c)
                return true;
        }
        return false;
    }

    public boolean equal(LispObject obj)
    {
        if (this == obj)
            return true;
        if (obj instanceof LispCharacter) {
            if (c == ((LispCharacter)obj).c)
                return true;
        }
        return false;
    }

    public boolean equalp(LispObject obj)
    {
        if (this == obj)
            return true;
        if (obj instanceof LispCharacter) {
            if (c == ((LispCharacter)obj).c)
                return true;
            return Utilities.toLowerCase(c) == Utilities.toLowerCase(((LispCharacter)obj).c);
        }
        return false;
    }

    public static char getValue(LispObject obj) throws ConditionThrowable
    {
        try {
            return ((LispCharacter)obj).getValue();
        }
        catch (ClassCastException e) {
            throw new ConditionThrowable(new TypeError(obj, "character"));
        }
    }

    public final char getValue()
    {
        return c;
    }

    public int hashCode()
    {
        return c;
    }

    public final String toString()
    {
        StringBuffer sb = new StringBuffer();
        if (_PRINT_ESCAPE_.symbolValueNoThrow() != NIL) {
            sb.append("#\\");
            switch (c) {
                case 0:
                    sb.append("Null");
                    break;
                case '\t':
                    sb.append("Tab");
                    break;
                case '\n':
                    sb.append("Newline");
                    break;
                case '\f':
                    sb.append("Page");
                    break;
                case '\r':
                    sb.append("Return");
                    break;
                case ' ':
                    sb.append("Space");
                    break;
                case '\b':
                    sb.append("Backspace");
                    break;
                default:
                    sb.append(c);
                    break;
            }
        } else {
            sb.append(c);
        }
        return sb.toString();
    }

    private static final Primitive1 CHARACTER = new Primitive1("character") {
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            if (arg instanceof LispCharacter)
                return arg;
            if (arg instanceof LispString) {
                if (arg.length() == 1)
                    return ((LispString)arg).get(0);
            } else if (arg instanceof Symbol) {
                String name = arg.getName();
                if (name.length() == 1)
                    return new LispCharacter(name.charAt(0));
            }
            throw new ConditionThrowable(new TypeError());
        }
    };

    // ### whitespacep
    private static final Primitive1 WHITESPACEP =
        new Primitive1("whitespacep", PACKAGE_SYS, false) {
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            LispCharacter character = checkCharacter(arg);
            return Character.isWhitespace(character.c) ? T : NIL;
        }
    };
}
