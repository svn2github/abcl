/*
 * SimpleError.java
 *
 * Copyright (C) 2003 Peter Graves
 * $Id: SimpleError.java,v 1.8 2004-10-13 00:22:19 piso Exp $
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

public class SimpleError extends LispError
{
    public SimpleError(LispObject formatControl, LispObject formatArguments)
    {
        setFormatControl(formatControl);
        setFormatArguments(formatArguments);
    }

    public SimpleError(LispObject initArgs) throws ConditionThrowable
    {
        super(initArgs);
    }

    public SimpleError(String message)
    {
        super(message);
    }

    public LispObject typeOf()
    {
        return Symbol.SIMPLE_ERROR;
    }

    public LispObject classOf()
    {
        return BuiltInClass.SIMPLE_ERROR;
    }

    public LispObject typep(LispObject type) throws ConditionThrowable
    {
        if (type == Symbol.SIMPLE_ERROR)
            return T;
        if (type == BuiltInClass.SIMPLE_ERROR)
            return T;
        if (type == Symbol.SIMPLE_CONDITION)
            return T;
        if (type == BuiltInClass.SIMPLE_CONDITION)
            return T;
        return super.typep(type);
    }
}
