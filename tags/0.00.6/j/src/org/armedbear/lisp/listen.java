/*
 * listen.java
 *
 * Copyright (C) 2004 Peter Graves
 * $Id: listen.java,v 1.2 2004-03-11 11:37:53 piso Exp $
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

// ### listen
public final class listen extends Primitive
{
    private listen()
    {
        super("listen", "&optional input-stream");
    }

    public LispObject execute() throws ConditionThrowable
    {
        Stream stream =
            checkCharacterInputStream(_STANDARD_INPUT_.symbolValue());
        return stream.listen();
    }

    public LispObject execute(LispObject arg) throws ConditionThrowable
    {
        return inSynonymOf(arg).listen();
    }

    private static final Primitive LISTEN = new listen();
}
