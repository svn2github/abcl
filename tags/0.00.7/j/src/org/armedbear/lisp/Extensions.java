/*
 * Extensions.java
 *
 * Copyright (C) 2002-2005 Peter Graves
 * $Id: Extensions.java,v 1.38 2005-05-10 18:11:32 piso Exp $
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

import java.io.File;
import java.io.IOException;
import java.net.Socket;

public final class Extensions extends Lisp
{
    // ### *ed-functions*
    public static final Symbol _ED_FUNCTIONS_ =
        exportSpecial("*ED-FUNCTIONS*", PACKAGE_EXT,
                      list1(intern("DEFAULT-ED-FUNCTION", PACKAGE_SYS)));

    // ### neq
    private static final Primitive NEQ =
        new Primitive("neq", PACKAGE_EXT, true)
    {
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            return first != second ? T : NIL;
        }
    };

    // ### memq item list => tail
    private static final Primitive MEMQ =
        new Primitive("memq", PACKAGE_EXT, true, "item list")
    {
        public LispObject execute(LispObject item, LispObject list)
            throws ConditionThrowable
        {
            while (list != NIL) {
                if (item == list.car())
                    return list;
                list = list.cdr();
            }
            return NIL;
        }
    };

    // ### memql item list => tail
    private static final Primitive MEMQL =
        new Primitive("memql", PACKAGE_EXT, true, "item list")
    {
        public LispObject execute(LispObject item, LispObject list)
            throws ConditionThrowable
        {
            while (list != NIL) {
                if (item.eql(list.car()))
                    return list;
                list = list.cdr();
            }
            return NIL;
        }
    };

    // ### special-variable-p
    private static final Primitive SPECIAL_VARIABLE_P =
        new Primitive("special-variable-p", PACKAGE_EXT, true)
    {
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            return arg.isSpecialVariable() ? T : NIL;
        }
    };

    // ### source
    private static final Primitive SOURCE =
        new Primitive("source", PACKAGE_EXT, true)
    {
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            return get(checkSymbol(arg), Symbol._SOURCE, NIL);
        }
    };

    // ### source-file-position
    private static final Primitive SOURCE_FILE_POSITION =
        new Primitive("source-file-position", PACKAGE_EXT, true)
    {
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            LispObject obj = get(checkSymbol(arg), Symbol._SOURCE, NIL);
            if (obj instanceof Cons)
                return obj.cdr();
            return NIL;
        }
    };

    // ### source-pathname
    public static final Primitive SOURCE_PATHNAME =
        new Primitive("source-pathname", PACKAGE_EXT, true)
    {
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            LispObject obj = get(checkSymbol(arg), Symbol._SOURCE, NIL);
            if (obj instanceof Cons)
                return obj.car();
            return obj;
        }
    };

    // ### exit
    private static final Primitive EXIT =
        new Primitive("exit", PACKAGE_EXT, true)
    {
        public LispObject execute() throws ConditionThrowable
        {
            exit(0);
            return LispThread.currentThread().nothing();
        }
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            int status = 0;
            if (first == Keyword.STATUS) {
                if (second instanceof Fixnum)
                    status = ((Fixnum)second).value;
            }
            exit(status);
            return LispThread.currentThread().nothing();
        }
    };

    // ### quit
    private static final Primitive QUIT =
        new Primitive("quit", PACKAGE_EXT, true)
    {
        public LispObject execute() throws ConditionThrowable
        {
            exit(0);
            return LispThread.currentThread().nothing();
        }
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            int status = 0;
            if (first == Keyword.STATUS) {
                if (second instanceof Fixnum)
                    status = ((Fixnum)second).value;
            }
            exit(status);
            return LispThread.currentThread().nothing();
        }
    };

    // ### dump-java-stack
    private static final Primitive DUMP_JAVA_STACK =
        new Primitive("dump-java-stack", PACKAGE_EXT, true)
    {
        public LispObject execute() throws ConditionThrowable
        {
            Thread.dumpStack();
            return LispThread.currentThread().nothing();
        }
    };

    // ### make-temp-file => namestring
    private static final Primitive MAKE_TEMP_FILE =
        new Primitive("make-temp-file", PACKAGE_EXT, true, "")
    {
        public LispObject execute() throws ConditionThrowable
        {
            try {
                File file = File.createTempFile("abcl", null, null);
                if (file != null)
                    return new Pathname(file.getPath());
            }
            catch (IOException e) {
                Debug.trace(e);
            }
            return NIL;
        }
    };
}
