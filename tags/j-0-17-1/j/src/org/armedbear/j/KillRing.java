/*  
 * KillRing.java
 *
 * Copyright (C) 1998-2002 Peter Graves
 * $Id: KillRing.java,v 1.1.1.1 2002-09-24 16:09:20 piso Exp $
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

package org.armedbear.j;

import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;
 
public final class KillRing extends Ring
{
    public KillRing()
    {
        super(30);
    }

    public void promoteLastPaste()
    {
        promoteLast();
        // Work around Java bug 4213197. Call new String() to make sure the
        // string we put on the system clipboard was not generated by a
        // substring operation.
        StringSelection ss = new StringSelection(new String(peek()));
        Toolkit.getDefaultToolkit().getSystemClipboard().setContents(ss, ss);
    }
}
