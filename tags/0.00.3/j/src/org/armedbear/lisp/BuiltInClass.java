/*
 * BuiltInClass.java
 *
 * Copyright (C) 2003 Peter Graves
 * $Id: BuiltInClass.java,v 1.10 2003-10-11 20:40:53 piso Exp $
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

public class BuiltInClass extends LispClass
{
    private BuiltInClass(Symbol symbol)
    {
        super(symbol);
    }

    public LispObject typeOf()
    {
        return Symbol.BUILT_IN_CLASS;
    }

    public LispClass classOf()
    {
        return BuiltInClass.BUILT_IN_CLASS;
    }

    public LispObject typep(LispObject type) throws ConditionThrowable
    {
        if (type == Symbol.BUILT_IN_CLASS)
            return T;
        if (type == BuiltInClass.BUILT_IN_CLASS)
            return T;
        return super.typep(type);
    }

    public String toString()
    {
        StringBuffer sb = new StringBuffer("#<BUILT-IN-CLASS ");
        sb.append(symbol.getName());
        sb.append('>');
        return sb.toString();
    }

    private static BuiltInClass addClass(Symbol symbol)
    {
        BuiltInClass c = new BuiltInClass(symbol);
        addClass(symbol, c);
        return c;
    }

    public static final BuiltInClass CLASS_T                          = addClass(T);

    public static final BuiltInClass ARITHMETIC_ERROR                 = addClass(Symbol.ARITHMETIC_ERROR);
    public static final BuiltInClass ARRAY                            = addClass(Symbol.ARRAY);
    public static final BuiltInClass BIGNUM                           = addClass(Symbol.BIGNUM);
    public static final BuiltInClass BIT_VECTOR                       = addClass(Symbol.BIT_VECTOR);
    public static final BuiltInClass BROADCAST_STREAM                 = addClass(Symbol.BROADCAST_STREAM);
    public static final BuiltInClass BUILT_IN_CLASS                   = addClass(Symbol.BUILT_IN_CLASS);
    public static final BuiltInClass CELL_ERROR                       = addClass(Symbol.CELL_ERROR);
    public static final BuiltInClass CHARACTER                        = addClass(Symbol.CHARACTER);
    public static final BuiltInClass CLASS                            = addClass(Symbol.CLASS);
    public static final BuiltInClass COMPLEX                          = addClass(Symbol.COMPLEX);
    public static final BuiltInClass CONCATENATED_STREAM              = addClass(Symbol.CONCATENATED_STREAM);
    public static final BuiltInClass CONDITION                        = addClass(Symbol.CONDITION);
    public static final BuiltInClass CONS                             = addClass(Symbol.CONS);
    public static final BuiltInClass CONTROL_ERROR                    = addClass(Symbol.CONTROL_ERROR);
    public static final BuiltInClass DIVISION_BY_ZERO                 = addClass(Symbol.DIVISION_BY_ZERO);
    public static final BuiltInClass ECHO_STREAM                      = addClass(Symbol.ECHO_STREAM);
    public static final BuiltInClass END_OF_FILE                      = addClass(Symbol.END_OF_FILE);
    public static final BuiltInClass ERROR                            = addClass(Symbol.ERROR);
    public static final BuiltInClass FILE_ERROR                       = addClass(Symbol.FILE_ERROR);
    public static final BuiltInClass FILE_STREAM                      = addClass(Symbol.FILE_STREAM);
    public static final BuiltInClass FIXNUM                           = addClass(Symbol.FIXNUM);
    public static final BuiltInClass FLOAT                            = addClass(Symbol.FLOAT);
    public static final BuiltInClass FLOATING_POINT_INEXACT           = addClass(Symbol.FLOATING_POINT_INEXACT);
    public static final BuiltInClass FLOATING_POINT_INVALID_OPERATION = addClass(Symbol.FLOATING_POINT_INVALID_OPERATION);
    public static final BuiltInClass FLOATING_POINT_OVERFLOW          = addClass(Symbol.FLOATING_POINT_OVERFLOW);
    public static final BuiltInClass FLOATING_POINT_UNDERFLOW         = addClass(Symbol.FLOATING_POINT_UNDERFLOW);
    public static final BuiltInClass FUNCTION                         = addClass(Symbol.FUNCTION);
//     public static final BuiltInClass GENERIC_FUNCTION                 = addClass(Symbol.GENERIC_FUNCTION);
    public static final BuiltInClass HASH_TABLE                       = addClass(Symbol.HASH_TABLE);
    public static final BuiltInClass INTEGER                          = addClass(Symbol.INTEGER);
    public static final BuiltInClass LIST                             = addClass(Symbol.LIST);
    public static final BuiltInClass LOGICAL_PATHNAME                 = addClass(Symbol.LOGICAL_PATHNAME);
    public static final BuiltInClass METHOD                           = addClass(Symbol.METHOD);
    public static final BuiltInClass METHOD_COMBINATION               = addClass(Symbol.METHOD_COMBINATION);
    public static final BuiltInClass NULL                             = addClass(Symbol.NULL);
    public static final BuiltInClass NUMBER                           = addClass(Symbol.NUMBER);
    public static final BuiltInClass PACKAGE                          = addClass(Symbol.PACKAGE);
    public static final BuiltInClass PACKAGE_ERROR                    = addClass(Symbol.PACKAGE_ERROR);
    public static final BuiltInClass PARSE_ERROR                      = addClass(Symbol.PARSE_ERROR);
    public static final BuiltInClass PATHNAME                         = addClass(Symbol.PATHNAME);
    public static final BuiltInClass PRINT_NOT_READABLE               = addClass(Symbol.PRINT_NOT_READABLE);
    public static final BuiltInClass PROGRAM_ERROR                    = addClass(Symbol.PROGRAM_ERROR);
    public static final BuiltInClass RANDOM_STATE                     = addClass(Symbol.RANDOM_STATE);
    public static final BuiltInClass RATIO                            = addClass(Symbol.RATIO);
    public static final BuiltInClass RATIONAL                         = addClass(Symbol.RATIONAL);
    public static final BuiltInClass READER_ERROR                     = addClass(Symbol.READER_ERROR);
    public static final BuiltInClass READTABLE                        = addClass(Symbol.READTABLE);
    public static final BuiltInClass REAL                             = addClass(Symbol.REAL);
    public static final BuiltInClass RESTART                          = addClass(Symbol.RESTART);
    public static final BuiltInClass SEQUENCE                         = addClass(Symbol.SEQUENCE);
    public static final BuiltInClass SERIOUS_CONDITION                = addClass(Symbol.SERIOUS_CONDITION);
    public static final BuiltInClass SIMPLE_CONDITION                 = addClass(Symbol.SIMPLE_CONDITION);
    public static final BuiltInClass SIMPLE_ERROR                     = addClass(Symbol.SIMPLE_ERROR);
    public static final BuiltInClass SIMPLE_TYPE_ERROR                = addClass(Symbol.SIMPLE_TYPE_ERROR);
    public static final BuiltInClass SIMPLE_WARNING                   = addClass(Symbol.SIMPLE_WARNING);
//     public static final BuiltInClass STANDARD_CLASS                   = addClass(Symbol.STANDARD_CLASS);
//     public static final BuiltInClass STANDARD_GENERIC_FUNCTION        = addClass(Symbol.STANDARD_GENERIC_FUNCTION);
//     public static final BuiltInClass STANDARD_METHOD                  = addClass(Symbol.STANDARD_METHOD);
//     public static final BuiltInClass STANDARD_OBJECT                  = addClass(Symbol.STANDARD_OBJECT);
    public static final BuiltInClass STORAGE_CONDITION                = addClass(Symbol.STORAGE_CONDITION);
    public static final BuiltInClass STREAM                           = addClass(Symbol.STREAM);
    public static final BuiltInClass STREAM_ERROR                     = addClass(Symbol.STREAM_ERROR);
    public static final BuiltInClass STRING                           = addClass(Symbol.STRING);
    public static final BuiltInClass STRING_STREAM                    = addClass(Symbol.STRING_STREAM);
    public static final BuiltInClass STRUCTURE_CLASS                  = addClass(Symbol.STRUCTURE_CLASS);
    public static final BuiltInClass STRUCTURE_OBJECT                 = addClass(Symbol.STRUCTURE_OBJECT);
    public static final BuiltInClass STYLE_WARNING                    = addClass(Symbol.STYLE_WARNING);
    public static final BuiltInClass SYMBOL                           = addClass(Symbol.SYMBOL);
    public static final BuiltInClass SYNONYM_STREAM                   = addClass(Symbol.SYNONYM_STREAM);
    public static final BuiltInClass TWO_WAY_STREAM                   = addClass(Symbol.TWO_WAY_STREAM);
    public static final BuiltInClass TYPE_ERROR                       = addClass(Symbol.TYPE_ERROR);
    public static final BuiltInClass UNBOUND_SLOT                     = addClass(Symbol.UNBOUND_SLOT);
    public static final BuiltInClass UNBOUND_VARIABLE                 = addClass(Symbol.UNBOUND_VARIABLE);
    public static final BuiltInClass UNDEFINED_FUNCTION               = addClass(Symbol.UNDEFINED_FUNCTION);
    public static final BuiltInClass VECTOR                           = addClass(Symbol.VECTOR);
    public static final BuiltInClass WARNING                          = addClass(Symbol.WARNING);

    public static final StandardClass STANDARD_CLASS =
        new StandardClass(Symbol.STANDARD_CLASS,
                          list1(CLASS_T));
    static {
        addClass(Symbol.STANDARD_CLASS, STANDARD_CLASS);
    }

    public static final StandardClass STANDARD_OBJECT =
        new StandardClass(Symbol.STANDARD_OBJECT,
                          list1(CLASS_T));
    static {
        addClass(Symbol.STANDARD_OBJECT, STANDARD_OBJECT);
    }

    public static final StandardClass GENERIC_FUNCTION =
        new StandardClass(Symbol.GENERIC_FUNCTION,
                          list1(FUNCTION));
    static {
        addClass(Symbol.GENERIC_FUNCTION, GENERIC_FUNCTION);
    }

    static {
        ARITHMETIC_ERROR.setDirectSuperclass(ERROR);
        ARITHMETIC_ERROR.setCPL(ARITHMETIC_ERROR, ERROR, SERIOUS_CONDITION,
                                CONDITION, CLASS_T);
        ARRAY.setDirectSuperclass(CLASS_T);
        ARRAY.setCPL(ARRAY, CLASS_T);
        BIGNUM.setDirectSuperclass(INTEGER);
        BIGNUM.setCPL(BIGNUM, INTEGER, RATIONAL, REAL, NUMBER, CLASS_T);
        BIT_VECTOR.setDirectSuperclass(VECTOR);
        BIT_VECTOR.setCPL(BIT_VECTOR, VECTOR, ARRAY, SEQUENCE, CLASS_T);
        BROADCAST_STREAM.setDirectSuperclass(STREAM);
        BROADCAST_STREAM.setCPL(BROADCAST_STREAM, STREAM, CLASS_T);
        BUILT_IN_CLASS.setDirectSuperclass(CLASS);
        BUILT_IN_CLASS.setCPL(BUILT_IN_CLASS, CLASS, STANDARD_OBJECT, CLASS_T);
        CELL_ERROR.setDirectSuperclass(ERROR);
        CELL_ERROR.setCPL(CELL_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                          CLASS_T);
        CHARACTER.setDirectSuperclass(CLASS_T);
        CHARACTER.setCPL(CHARACTER, CLASS_T);
        CLASS.setDirectSuperclass(CLASS_T);
        CLASS.setCPL(CLASS, STANDARD_OBJECT, CLASS_T);
        COMPLEX.setDirectSuperclass(NUMBER);
        COMPLEX.setCPL(COMPLEX, NUMBER, CLASS_T);
        CONCATENATED_STREAM.setDirectSuperclass(STREAM);
        CONCATENATED_STREAM.setCPL(CONCATENATED_STREAM, STREAM, CLASS_T);
        CONDITION.setDirectSuperclass(STANDARD_OBJECT);
        CONDITION.setCPL(CONDITION, CLASS_T);
        CONS.setDirectSuperclass(LIST);
        CONS.setCPL(CONS, LIST, SEQUENCE, CLASS_T);
        CONTROL_ERROR.setDirectSuperclass(ERROR);
        CONTROL_ERROR.setCPL(CONTROL_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                             CLASS_T);
        DIVISION_BY_ZERO.setDirectSuperclass(ARITHMETIC_ERROR);
        DIVISION_BY_ZERO.setCPL(DIVISION_BY_ZERO, ARITHMETIC_ERROR, ERROR,
                                SERIOUS_CONDITION, CONDITION, CLASS_T);
        ECHO_STREAM.setDirectSuperclass(CLASS_T);
        ECHO_STREAM.setCPL(ECHO_STREAM, CLASS_T);
        END_OF_FILE.setDirectSuperclass(STREAM_ERROR);
        END_OF_FILE.setCPL(END_OF_FILE, STREAM_ERROR, ERROR, SERIOUS_CONDITION,
                           CONDITION, CLASS_T);
        ERROR.setDirectSuperclass(SERIOUS_CONDITION);
        ERROR.setCPL(ERROR, SERIOUS_CONDITION, CONDITION, CLASS_T);
        FIXNUM.setDirectSuperclass(INTEGER);
        FIXNUM.setCPL(FIXNUM, INTEGER, RATIONAL, REAL, NUMBER, CLASS_T);
        FILE_ERROR.setDirectSuperclass(ERROR);
        FILE_ERROR.setCPL(FILE_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                          CLASS_T);
        FILE_STREAM.setDirectSuperclass(STREAM);
        FILE_STREAM.setCPL(FILE_STREAM, STREAM, CLASS_T);
        FLOAT.setDirectSuperclass(REAL);
        FLOAT.setCPL(FLOAT, REAL, NUMBER, CLASS_T);
        FLOATING_POINT_INEXACT.setDirectSuperclass(ARITHMETIC_ERROR);
        FLOATING_POINT_INEXACT.setCPL(FLOATING_POINT_INEXACT, ARITHMETIC_ERROR,
                                      ERROR, SERIOUS_CONDITION, CONDITION,
                                      CLASS_T);
        FLOATING_POINT_INVALID_OPERATION.setDirectSuperclass(ARITHMETIC_ERROR);
        FLOATING_POINT_INVALID_OPERATION.setCPL(FLOATING_POINT_INVALID_OPERATION,
                                                ARITHMETIC_ERROR, ERROR,
                                                SERIOUS_CONDITION, CONDITION,
                                                CLASS_T);
        FLOATING_POINT_OVERFLOW.setDirectSuperclass(ARITHMETIC_ERROR);
        FLOATING_POINT_OVERFLOW.setCPL(FLOATING_POINT_OVERFLOW, ARITHMETIC_ERROR,
                                       ERROR, SERIOUS_CONDITION, CONDITION,
                                       CLASS_T);
        FLOATING_POINT_UNDERFLOW.setDirectSuperclass(ARITHMETIC_ERROR);
        FLOATING_POINT_UNDERFLOW.setCPL(FLOATING_POINT_UNDERFLOW, ARITHMETIC_ERROR,
                                        ERROR, SERIOUS_CONDITION, CONDITION,
                                        CLASS_T);
        FUNCTION.setDirectSuperclass(CLASS_T);
        FUNCTION.setCPL(FUNCTION, CLASS_T);
        GENERIC_FUNCTION.setDirectSuperclass(FUNCTION);
        GENERIC_FUNCTION.setCPL(GENERIC_FUNCTION, FUNCTION, CLASS_T);
        HASH_TABLE.setDirectSuperclass(CLASS_T);
        HASH_TABLE.setCPL(HASH_TABLE, CLASS_T);
        INTEGER.setDirectSuperclass(RATIONAL);
        INTEGER.setCPL(INTEGER, RATIONAL, REAL, NUMBER, CLASS_T);
        LIST.setDirectSuperclass(SEQUENCE);
        LIST.setCPL(LIST, SEQUENCE, CLASS_T);
        LOGICAL_PATHNAME.setDirectSuperclass(PATHNAME);
        LOGICAL_PATHNAME.setCPL(LOGICAL_PATHNAME, PATHNAME, CLASS_T);
        METHOD.setDirectSuperclass(CLASS_T);
        METHOD.setCPL(METHOD, CLASS_T);
        METHOD_COMBINATION.setDirectSuperclass(CLASS_T);
        METHOD_COMBINATION.setCPL(METHOD_COMBINATION, CLASS_T);
        NULL.setDirectSuperclass(LIST);
        NULL.setCPL(NULL, SYMBOL, LIST, SEQUENCE, CLASS_T);
        NUMBER.setDirectSuperclass(CLASS_T);
        NUMBER.setCPL(NUMBER, CLASS_T);
        PACKAGE.setDirectSuperclass(CLASS_T);
        PACKAGE.setCPL(PACKAGE, CLASS_T);
        PACKAGE_ERROR.setDirectSuperclass(ERROR);
        PACKAGE_ERROR.setCPL(PACKAGE_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                             CLASS_T);
        PARSE_ERROR.setDirectSuperclass(ERROR);
        PARSE_ERROR.setCPL(PARSE_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                           CLASS_T);
        PATHNAME.setDirectSuperclass(CLASS_T);
        PATHNAME.setCPL(PATHNAME, CLASS_T);
        PRINT_NOT_READABLE.setDirectSuperclass(ERROR);
        PRINT_NOT_READABLE.setCPL(PRINT_NOT_READABLE, ERROR, SERIOUS_CONDITION,
                                  CONDITION, CLASS_T);
        PROGRAM_ERROR.setDirectSuperclass(ERROR);
        PROGRAM_ERROR.setCPL(PROGRAM_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                             CLASS_T);
        RANDOM_STATE.setDirectSuperclass(CLASS_T);
        RANDOM_STATE.setCPL(RANDOM_STATE, CLASS_T);
        RATIO.setDirectSuperclass(RATIONAL);
        RATIO.setCPL(RATIO, RATIONAL, REAL, NUMBER, CLASS_T);
        RATIONAL.setDirectSuperclass(REAL);
        RATIONAL.setCPL(RATIONAL, REAL, NUMBER, CLASS_T);
        READER_ERROR.setDirectSuperclasses(list2(PARSE_ERROR, STREAM_ERROR));
        READER_ERROR.setCPL(READER_ERROR, PARSE_ERROR, STREAM_ERROR,ERROR,
                            SERIOUS_CONDITION, CONDITION, CLASS_T);
        READTABLE.setDirectSuperclass(CLASS_T);
        READTABLE.setCPL(READTABLE, CLASS_T);
        REAL.setDirectSuperclass(NUMBER);
        REAL.setCPL(REAL, NUMBER, CLASS_T);
        RESTART.setDirectSuperclass(CLASS_T);
        RESTART.setCPL(RESTART, CLASS_T);
        SEQUENCE.setDirectSuperclass(CLASS_T);
        SEQUENCE.setCPL(SEQUENCE, CLASS_T);
        SERIOUS_CONDITION.setDirectSuperclass(CONDITION);
        SERIOUS_CONDITION.setCPL(SERIOUS_CONDITION, CONDITION, CLASS_T);
        SIMPLE_CONDITION.setDirectSuperclass(CONDITION);
        SIMPLE_CONDITION.setCPL(SIMPLE_CONDITION, CONDITION, CLASS_T);
        SIMPLE_ERROR.setDirectSuperclass(ERROR);
        SIMPLE_ERROR.setCPL(SIMPLE_ERROR, SIMPLE_CONDITION, ERROR,
                            SERIOUS_CONDITION, CONDITION, CLASS_T);
        SIMPLE_TYPE_ERROR.setDirectSuperclasses(list2(SIMPLE_CONDITION, TYPE_ERROR));
        SIMPLE_TYPE_ERROR.setCPL(SIMPLE_TYPE_ERROR, SIMPLE_CONDITION,
                                 TYPE_ERROR, ERROR, SERIOUS_CONDITION,
                                 CONDITION, CLASS_T);
        SIMPLE_WARNING.setDirectSuperclasses(list2(SIMPLE_CONDITION, WARNING));
        SIMPLE_WARNING.setCPL(SIMPLE_WARNING, SIMPLE_CONDITION, WARNING,
                              CONDITION, CLASS_T);
        STANDARD_CLASS.setDirectSuperclass(CLASS);
        STANDARD_CLASS.setCPL(STANDARD_CLASS, CLASS, STANDARD_OBJECT, CLASS_T);
//         STANDARD_GENERIC_FUNCTION.setDirectSuperclass(GENERIC_FUNCTION);
//         STANDARD_GENERIC_FUNCTION.setCPL(STANDARD_GENERIC_FUNCTION,
//                                          GENERIC_FUNCTION, FUNCTION, CLASS_T);
//         STANDARD_METHOD.setDirectSuperclass(list2(METHOD, STANDARD_OBJECT));
//         STANDARD_METHOD.setCPL(STANDARD_METHOD, METHOD, STANDARD_OBJECT,
//                                CLASS_T);
//         STANDARD_OBJECT.setDirectSuperclass(CLASS_T);
        STANDARD_OBJECT.setCPL(STANDARD_OBJECT, CLASS_T);
        STORAGE_CONDITION.setDirectSuperclass(SERIOUS_CONDITION);
        STORAGE_CONDITION.setCPL(STORAGE_CONDITION, SERIOUS_CONDITION, CONDITION,
                                 CLASS_T);
        STREAM.setDirectSuperclass(CLASS_T);
        STREAM.setCPL(STREAM, CLASS_T);
        STREAM_ERROR.setDirectSuperclass(ERROR);
        STREAM_ERROR.setCPL(STREAM_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                            CLASS_T);
        STRING.setDirectSuperclass(VECTOR);
        STRING.setCPL(STRING, VECTOR, ARRAY, SEQUENCE, CLASS_T);
        STRING_STREAM.setDirectSuperclass(STREAM);
        STRING_STREAM.setCPL(STRING_STREAM, STREAM, CLASS_T);
        STRUCTURE_CLASS.setDirectSuperclass(CLASS);
        STRUCTURE_CLASS.setCPL(STRUCTURE_CLASS, CLASS, STANDARD_OBJECT,
                               CLASS_T);
        STRUCTURE_OBJECT.setDirectSuperclass(CLASS_T);
        STRUCTURE_OBJECT.setCPL(STRUCTURE_OBJECT, CLASS_T);
        STYLE_WARNING.setDirectSuperclass(WARNING);
        STYLE_WARNING.setCPL(STYLE_WARNING, WARNING, CONDITION, CLASS_T);
        SYMBOL.setDirectSuperclass(CLASS_T);
        SYMBOL.setCPL(SYMBOL, CLASS_T);
        SYNONYM_STREAM.setDirectSuperclass(STREAM);
        SYNONYM_STREAM.setCPL(SYNONYM_STREAM, STREAM, CLASS_T);
        TWO_WAY_STREAM.setDirectSuperclass(STREAM);
        TWO_WAY_STREAM.setCPL(TWO_WAY_STREAM, STREAM, CLASS_T);
        TYPE_ERROR.setDirectSuperclass(ERROR);
        TYPE_ERROR.setCPL(TYPE_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                          CLASS_T);
        UNBOUND_SLOT.setDirectSuperclass(CELL_ERROR);
        UNBOUND_SLOT.setCPL(UNBOUND_SLOT, CELL_ERROR, ERROR, SERIOUS_CONDITION,
                            CONDITION, CLASS_T);
        UNBOUND_VARIABLE.setDirectSuperclass(CELL_ERROR);
        UNBOUND_VARIABLE.setCPL(UNBOUND_VARIABLE, CELL_ERROR, ERROR,
                                SERIOUS_CONDITION, CONDITION, CLASS_T);
        UNDEFINED_FUNCTION.setDirectSuperclass(CELL_ERROR);
        UNDEFINED_FUNCTION.setCPL(UNDEFINED_FUNCTION, CELL_ERROR, ERROR,
                                  SERIOUS_CONDITION, CONDITION, CLASS_T);
        VECTOR.setDirectSuperclasses(list2(ARRAY, SEQUENCE));
        VECTOR.setCPL(VECTOR, ARRAY, SEQUENCE, CLASS_T);
        WARNING.setDirectSuperclass(CONDITION);
        WARNING.setCPL(WARNING, CONDITION, CLASS_T);
    }
}
