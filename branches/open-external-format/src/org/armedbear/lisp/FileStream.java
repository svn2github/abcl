/*
 * FileStream.java
 *
 * Copyright (C) 2004-2006 Peter Graves
 * Copyright (C) 2008 Hideo at Yokohama
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

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import org.armedbear.lisp.util.RandomAccessCharacterFile;

public final class FileStream extends Stream
{
    private final RandomAccessCharacterFile racf;
    private final Pathname pathname;
    private final int bytesPerUnit;
    private InputStream inst;
    private OutputStream outst;
    private Reader reader;
    private Writer writer;

    public enum EolStyle {
        CR,
        CRLF,
        LF
    }
    
    static final private Symbol keywordCodePage = Packages.internKeyword("CODE-PAGE");
    
    private final static EolStyle platformEolStyle = Utilities.isPlatformWindows ? EolStyle.CRLF : EolStyle.LF;
    
    private EolStyle eolStyle = platformEolStyle;
    private char eolChar = 0;
    
    public FileStream(Pathname pathname, String namestring,
                      LispObject elementType, LispObject direction,
                      LispObject ifExists, String encoding, EolStyle eol)
        throws IOException
    {
        /* externalFormat is a LispObject of which the first char is a
         * name of a character encoding (such as :UTF-8 or :ISO-8859-1), used
         * by ABCL as a string designator, unless the name is :CODE-PAGE.
         * A real string is (thus) also allowed.
         * 
         * Then, a property list follows with 3 possible keys:
         *   :ID (values: code page numbers supported by MS-DOS/IBM-DOS/MS-Windows
         *   :EOL-STYLE (values: :CR / :LF / :CRLF [none means native])
         *   :LITTLE-ENDIAN (values: NIL / T)
         * 
         * These definitions have been taken from FLEXI-STREAMS:
         *    http://www.weitz.de/flexi-streams/#make-external-format
         */
        final File file = new File(namestring);
        String mode = null;
        if (direction == Keyword.INPUT) {
            mode = "r";
            isInputStream = true;
        } else if (direction == Keyword.OUTPUT) {
            mode = "rw";
            isOutputStream = true;
        } else if (direction == Keyword.IO) {
            mode = "rw";
            isInputStream = true;
            isOutputStream = true;
        }
        
        Debug.assertTrue(mode != null);
        RandomAccessFile raf = new RandomAccessFile(file, mode);
	
        // ifExists is ignored unless we have an output stream.
        if (isOutputStream) {
            final long length = file.isFile() ? file.length() : 0;
            if (length > 0) {
                if (ifExists == Keyword.OVERWRITE)
                    raf.seek(0);
                else if (ifExists == Keyword.APPEND)
                    raf.seek(raf.length());
                else
                    raf.setLength(0);
            }
        }
	// don't touch raf directly after passing it to racf.
	// the state will become inconsistent if you do that.
        racf = new RandomAccessCharacterFile(raf, encoding);

        this.pathname = pathname;
        this.elementType = elementType;
        if (elementType == Symbol.CHARACTER || elementType == Symbol.BASE_CHAR) {
            isCharacterStream = true;
            bytesPerUnit = 1;
	    if (isInputStream) {
		reader = racf.getReader();
	    }
	    if (isOutputStream) {
		writer = racf.getWriter();
	    }
        } else {
            isBinaryStream = true;
            int width;
            try {
                width = Fixnum.getValue(elementType.cadr());
            }
            catch (ConditionThrowable t) {
                width = 8;
            }
            bytesPerUnit = width / 8;
	    if (isInputStream) {
		inst = racf.getInputStream();
	    }
	    if (isOutputStream) {
		outst = racf.getOutputStream();
	    }
        }
        eolChar = (eol == EolStyle.CR) ? '\r' : '\n';
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.FILE_STREAM;
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.FILE_STREAM;
    }

    @Override
    public LispObject typep(LispObject typeSpecifier) throws ConditionThrowable
    {
        if (typeSpecifier == Symbol.FILE_STREAM)
            return T;
        if (typeSpecifier == BuiltInClass.FILE_STREAM)
            return T;
        return super.typep(typeSpecifier);
    }

    public Pathname getPathname()
    {
        return pathname;
    }

    @Override
    public LispObject listen() throws ConditionThrowable
    {
        try {
	    if (isInputStream) {
		return (racf.position() < racf.length()) ? T : NIL;
	    } else {
		streamNotInputStream();
	    }
        }
	catch (IOException e) {
            error(new StreamError(this, e));
        }
        // Not reached.
        return NIL;
    }

    @Override
    public LispObject fileLength() throws ConditionThrowable
    {
        final long length;
        if (isOpen()) {
            try {
                length = racf.length();
            }
            catch (IOException e) {
                error(new StreamError(this, e));
                // Not reached.
                return NIL;
            }
        } else {
            String namestring = pathname.getNamestring();
            if (namestring == null)
                return error(new SimpleError("Pathname has no namestring: " +
                                              pathname.writeToString()));
            File file = new File(namestring);
            length = file.length(); // in 8-bit bytes
        }
        if (isCharacterStream)
            return number(length);
        // "For a binary file, the length is measured in units of the
        // element type of the stream."
        return number(length / bytesPerUnit);
    }

    // Returns -1 at end of file.
    @Override
    protected int _readChar() throws ConditionThrowable
    {
        try {
            int c = reader.read();
            if (eolStyle == EolStyle.CRLF) {
                if (c == '\r') {
                    int c2 = reader.read();
                    if (c2 == '\n') {
                        ++lineNumber;
                        return c2;
                    } else {
			// '\r' was not followed by '\n'
			// we cannot depend on characters to contain 1 byte only
			// so we need to revert to the last known position.
			// The classical use case for unreadChar
			racf.unreadChar((char)c2);
		    }
                }
                return c;
            } else if (c == eolChar) {
                ++lineNumber;
                return c;
            } else {
		return c;
	    }
        }
        catch (NullPointerException e) {
            streamNotInputStream();
        }
        catch (IOException e) {
            error(new StreamError(this, e));
        }
        // Not reached.
        return -1;
    }

    @Override
    protected void _unreadChar(int n) throws ConditionThrowable
    {
        try {
            racf.unreadChar((char)n);
        }
        catch (IOException e) {
            error(new StreamError(this, e));
        }
    }

    @Override
    protected boolean _charReady() throws ConditionThrowable
    {
        return true;
    }

    @Override
    public void _writeChar(char c) throws ConditionThrowable
    {
        try {
            if (c == '\n') {
                if (eolStyle == EolStyle.CRLF)
                    writer.write('\r');
                writer.write(eolChar);
                charPos = 0;
            } else {
                writer.write(c);
                ++charPos;
            }
        }
        catch (IOException e) {
            error(new StreamError(this, e));
        }
    }


    public void _writeChars(char[] chars, int start, int end)
        throws ConditionThrowable {
	_writeChars(chars, start, end, true);
    }

    public void _writeChars(char[] chars, int start, int end, boolean maintainCharPos)
        throws ConditionThrowable
    {
        try {
	    if (eolStyle == EolStyle.LF) {
		/* we can do a little bit better in this special case */
		writer.write(chars, start, end);
		if (maintainCharPos) {
		    int lastlfpos = -1;
		    for (int i = start; i < end; i++) {
			if (chars[i] == '\n') {
			    lastlfpos = i;
			}
		    }
		    if (lastlfpos == -1) {
			charPos += end - start;
		    } else {
			charPos = end - lastlfpos;
		    }
		}
	    } else if (eolStyle == EolStyle.CRLF) {
                for (int i = start; i < end; i++) {
                    char c = chars[i];
                    if (c == '\n') {
                        writer.write('\r');
                        writer.write('\n');
                        charPos = 0;
                    } else {
                        writer.write(c);
                        ++charPos;
                    }
                }
            } else {
                for (int i = start; i < end; i++) {
                    char c = chars[i];
                    if (c == '\n') {
                        writer.write(eolChar);
                        charPos = 0;
                    } else {
                        writer.write(c);
                        ++charPos;
                    }
                }
            }
        }
        catch (IOException e) {
            error(new StreamError(this, e));
        }
    }

    @Override
    public void _writeString(String s) throws ConditionThrowable
    {
        _writeChars(s.toCharArray(), 0, s.length(), true);
    }

    @Override
    public void _writeLine(String s) throws ConditionThrowable
    {
	_writeChars(s.toCharArray(), 0, s.length(), false);
        if (eolStyle == EolStyle.CRLF)
            _writeChar('\r');
        _writeChar(eolChar);
        charPos = 0;
    }

    // Reads an 8-bit byte.
    @Override
    public int _readByte() throws ConditionThrowable
    {
        try {
            return inst.read(); // Reads an 8-bit byte.
        }
        catch (NullPointerException e) {
            streamNotInputStream();
        }
        catch (IOException e) {
            error(new StreamError(this, e));
        }
        // Not reached.
        return -1;
    }

    // Writes an 8-bit byte.
    @Override
    public void _writeByte(int n) throws ConditionThrowable
    {
        try {
            outst.write(n); // Writes an 8-bit byte.
        }
        catch (NullPointerException e) {
            streamNotOutputStream();
        }
        catch (IOException e) {
            error(new StreamError(this, e));
        }
    }

    @Override
    public void _clearInput() throws ConditionThrowable
    {
        try {
	    if (isInputStream) {
		racf.position(racf.length());
	    } else {
		streamNotInputStream();
	    }
        }
        catch (IOException e) {
            error(new StreamError(this, e));
        }
    }

    @Override
    protected long _getFilePosition() throws ConditionThrowable
    {
        try {
            long pos = racf.position();
            return pos / bytesPerUnit;
        }
        catch (IOException e) {
            error(new StreamError(this, e));
            // Not reached.
            return -1;
        }
    }

    @Override
    protected boolean _setFilePosition(LispObject arg) throws ConditionThrowable
    {
        try {
            long pos;
            if (arg == Keyword.START)
                pos = 0;
            else if (arg == Keyword.END)
                pos = racf.length();
            else {
                long n = Fixnum.getValue(arg); // FIXME arg might be a bignum
                pos = n * bytesPerUnit;
            }
            racf.position(pos);
        }
        catch (IOException e) {
            error(new StreamError(this, e));
        }
        return true;
    }

    @Override
    public void _close() throws ConditionThrowable
    {
        try {
            racf.close();
            setOpen(false);
        }
        catch (IOException e) {
            error(new StreamError(this, e));
        }
    }

    @Override
    public String writeToString() throws ConditionThrowable
    {
        return unreadableString(Symbol.FILE_STREAM);
    }

    // ### make-file-stream pathname namestring element-type direction if-exists external-format => stream
    private static final Primitive MAKE_FILE_STREAM =
        new Primitive("make-file-stream", PACKAGE_SYS, true,
                      "pathname namestring element-type direction if-exists external-format")
    {
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth,
                                  LispObject fifth, LispObject sixth)
            throws ConditionThrowable
        {
            final Pathname pathname;
            try {
                pathname = (Pathname) first;
            }
            catch (ClassCastException e) {
                return type_error(first, Symbol.PATHNAME);
            }
            final LispObject namestring;
            try {
                namestring = (AbstractString) second;
            }
            catch (ClassCastException e) {
                return type_error(second, Symbol.STRING);
            }
            LispObject elementType = third;
            LispObject direction = fourth;
            LispObject ifExists = fifth;
            LispObject externalFormat = sixth;
            
            String encoding = "ISO-8859-1";
            if (externalFormat != NIL) {
		if (externalFormat instanceof Symbol) {
		    Symbol enc = (Symbol)externalFormat; //FIXME: class cast exception to be caught
		    if (enc != NIL) {
			if (enc != keywordCodePage) {
			    encoding = enc.getName();
			}
			//FIXME: the else for the keywordCodePage to be filled in
		    }
		    //FIXME: the else for the == NIL to be filled in: raise an error...
		} else if (externalFormat instanceof AbstractString) {
		    AbstractString encName = (AbstractString) externalFormat;
		    encoding = encName.getStringValue();
		}
            }
            
            
            if (direction != Keyword.INPUT && direction != Keyword.OUTPUT &&
                direction != Keyword.IO)
                error(new LispError("Direction must be :INPUT, :OUTPUT, or :IO."));
            try {
                return new FileStream(pathname, namestring.getStringValue(),
                                      elementType, direction, ifExists,
                                      encoding, platformEolStyle);
            }
            catch (FileNotFoundException e) {
                return NIL;
            }
            catch (IOException e) {
                return error(new StreamError(null, e));
            }
        }
    };
}
