/*
 * StreamError.java
 *
 * Copyright (C) 2002-2003 Peter Graves
 * $Id: StreamError.java,v 1.9 2003-10-03 16:35:47 piso Exp $
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

public class StreamError extends LispError
{
    private Throwable cause;

    public StreamError()
    {
    }

    public StreamError(String message)
    {
        super(message);
    }

    public StreamError(Throwable cause)
    {
        super();
        this.cause = cause;
    }

    public LispObject typeOf()
    {
        return Symbol.STREAM_ERROR;
    }

    public LispClass classOf()
    {
        return BuiltInClass.STREAM_ERROR;
    }

    public LispObject typep(LispObject type) throws ConditionThrowable
    {
        if (type == Symbol.STREAM_ERROR)
            return T;
        if (type == BuiltInClass.STREAM_ERROR)
            return T;
        return super.typep(type);
    }

    public String getMessage()
    {
        if (cause != null) {
            String message = cause.getMessage();
            if (message != null && message.length() > 0)
                return message;
        }
        return "stream error";
    }
}
