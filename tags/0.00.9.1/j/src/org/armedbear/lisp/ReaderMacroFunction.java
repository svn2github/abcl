/*
 * ReaderMacroFunction.java
 *
 * Copyright (C) 2004 Peter Graves
 * $Id: ReaderMacroFunction.java,v 1.1 2004-03-11 19:00:11 piso Exp $
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

public abstract class ReaderMacroFunction extends Function
{
    public ReaderMacroFunction(String name)
    {
        super(name);
    }

    public ReaderMacroFunction(String name, String arglist)
    {
        super(name, arglist);
    }

    public ReaderMacroFunction(String name, Package pkg)
    {
        super(name, pkg);
    }

    public ReaderMacroFunction(String name, Package pkg, boolean exported)
    {
        super(name, pkg, exported);
    }

    public ReaderMacroFunction(String name, Package pkg, boolean exported,
                      String arglist)
    {
        super(name, pkg, exported, arglist);
    }

    public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
    {
        Stream stream = inSynonymOf(first);
        char c = LispCharacter.getValue(second);
        return execute(stream, c);
    }

    public abstract LispObject execute(Stream stream, char c)
        throws ConditionThrowable;
}
