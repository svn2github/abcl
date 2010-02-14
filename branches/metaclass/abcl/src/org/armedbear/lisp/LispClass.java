/*
 * LispClass.java
 *
 * Copyright (C) 2003-2005 Peter Graves
 * $Id$
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
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce an
 * executable, regardless of the license terms of these independent
 * modules, and to copy and distribute the resulting executable under
 * terms of your choice, provided that you also meet, for each linked
 * independent module, the terms and conditions of the license of that
 * module.  An independent module is a module which is not derived from
 * or based on this library.  If you modify this library, you may extend
 * this exception to your version of the library, but you are not
 * obligated to do so.  If you do not wish to do so, delete this
 * exception statement from your version.
 */

package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

public abstract class LispClass extends StandardObject
{
  private static final EqHashTable map = new EqHashTable(256, NIL, NIL);

  public static LispClass addClass(Symbol symbol, LispClass c)
  {
    synchronized (map)
      {
        map.put(symbol, c);
      }
    return c;
  }

  public static void removeClass(Symbol symbol)
  {
    synchronized (map)
      {
        map.remove(symbol);
      }
  }

  public static LispClass findClass(Symbol symbol)
  {
    synchronized (map)
      {
        return (LispClass) map.get(symbol);
      }
  }

  public static LispObject findClass(LispObject name, boolean errorp)

  {
    final Symbol symbol = checkSymbol(name);
    final LispClass c;
    synchronized (map)
      {
        c = (LispClass) map.get(symbol);
      }
    if (c != null)
      return c;
    if (errorp)
      {
        StringBuilder sb =
          new StringBuilder("There is no class named ");
        sb.append(name.writeToString());
        sb.append('.');
        return error(new LispError(sb.toString()));
      }
    return NIL;
  }

  private final int sxhash;

  private LispObject name;
  private LispObject propertyList;
  private Layout classLayout;
  private LispObject directSuperclasses = NIL;
  private LispObject directSubclasses = NIL;
  public LispObject classPrecedenceList = NIL; // FIXME! Should be private!
  public LispObject directMethods = NIL; // FIXME! Should be private!
  public LispObject documentation = NIL; // FIXME! Should be private!
  private boolean finalized;

  protected LispClass(Layout layout)
  {
    super(layout, layout == null ? 0 : layout.getLength());
    sxhash = hashCode() & 0x7fffffff;
  }

  protected LispClass(Symbol symbol)
  {
    this(null, symbol);
  }

  protected LispClass(Layout layout, Symbol symbol)
  {
    super(layout, layout == null ? 0 : layout.getLength());
    setName(symbol);
    sxhash = hashCode() & 0x7fffffff;
  }

  protected LispClass(Layout layout,
                      Symbol symbol, LispObject directSuperclasses)
  {
    super(layout, layout == null ? 0 : layout.getLength());
    sxhash = hashCode() & 0x7fffffff;
    setName(symbol);
    setDirectSuperclasses(directSuperclasses);
  }

  @Override
  public LispObject getParts()
  {
    LispObject result = NIL;
    result = result.push(new Cons("NAME", name != null ? name : NIL));
    result = result.push(new Cons("LAYOUT",
                                  getClassLayout() != null
                                  ? getClassLayout() : NIL));
    result = result.push(new Cons("DIRECT-SUPERCLASSES",
                                  getDirectSuperclasses()));
    result = result.push(new Cons("DIRECT-SUBCLASSES", getDirectSubclasses()));
    result = result.push(new Cons("CLASS-PRECEDENCE-LIST", classPrecedenceList));
    result = result.push(new Cons("DIRECT-METHODS", directMethods));
    result = result.push(new Cons("DOCUMENTATION", documentation));
    return result.nreverse();
  }

  @Override
  public final int sxhash()
  {
    return sxhash;
  }

  public LispObject getName()
  {
    return name;
  }

  public void setName(LispObject name)
  {
    this.name = name;
  }

  @Override
  public final LispObject getPropertyList()
  {
    if (propertyList == null)
      propertyList = NIL;
    return propertyList;
  }

  @Override
  public final void setPropertyList(LispObject obj)
  {
    if (obj == null)
      throw new NullPointerException();
    propertyList = obj;
  }

  public Layout getClassLayout()
  {
    return classLayout;
  }

  public void setClassLayout(Layout layout)
  {
    classLayout = layout;
  }

  public final int getLayoutLength()
  {
    if (layout == null)
      return 0;
    return layout.getLength();
  }

  public LispObject getDirectSuperclasses()
  {
    return directSuperclasses;
  }

  public void setDirectSuperclasses(LispObject directSuperclasses)
  {
    this.directSuperclasses = directSuperclasses;
  }

  public final boolean isFinalized()
  {
    return finalized;
  }

  public final void setFinalized(boolean b)
  {
    finalized = b;
  }

  // When there's only one direct superclass...
  public final void setDirectSuperclass(LispObject superclass)
  {
    setDirectSuperclasses(new Cons(superclass));
  }

  public LispObject getDirectSubclasses()
  {
    return directSubclasses;
  }

  public void setDirectSubclasses(LispObject directSubclasses)
  {
    this.directSubclasses = directSubclasses;
  }

  public LispObject getCPL()
  {
    return classPrecedenceList;
  }

  public void setCPL(LispObject... cpl)
  {
    LispObject obj1 = cpl[0];
    if (obj1 instanceof Cons && cpl.length == 1)
      classPrecedenceList = obj1;
    else
      {
        Debug.assertTrue(obj1 == this);
        LispObject l = NIL;
        for (int i = cpl.length; i-- > 0;)
            l = new Cons(cpl[i], l);
        classPrecedenceList = l;
      }
  }

  @Override
  public LispObject typeOf()
  {
    return Symbol.CLASS;
  }

  @Override
  public LispObject classOf()
  {
    return StandardClass.CLASS;
  }

  @Override
  public LispObject typep(LispObject type)
  {
    if (type == Symbol.CLASS)
      return T;
    if (type == StandardClass.CLASS)
      return T;
    return super.typep(type);
  }

  public boolean subclassp(LispObject obj)
  {
    LispObject cpl = classPrecedenceList;
    while (cpl != NIL)
      {
        if (cpl.car() == obj)
          return true;
        cpl = ((Cons)cpl).cdr;
      }
    return false;
  }

  // ### find-class symbol &optional errorp environment => class
  private static final Primitive FIND_CLASS =
    new Primitive(Symbol.FIND_CLASS, "symbol &optional errorp environment")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
        return findClass(arg, true);
      }
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
        return findClass(first, second != NIL);
      }
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)

      {
        // FIXME Use environment!
        return findClass(first, second != NIL);
      }
    };

  // ### %set-find-class
  private static final Primitive _SET_FIND_CLASS =
    new Primitive("%set-find-class", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
        final Symbol name = checkSymbol(first);
        if (second == NIL)
          {
            removeClass(name);
            return second;
          }
        final LispClass c = checkClass(second);
        addClass(name, c);
        return second;
      }
    };

  // ### subclassp
  private static final Primitive SUBCLASSP =
    new Primitive(Symbol.SUBCLASSP, "class")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
        final LispClass c = checkClass(first);
        return c.subclassp(second) ? T : NIL;
      }
    };
}
