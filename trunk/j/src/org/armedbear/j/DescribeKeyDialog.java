/*
 * DescribeKeyDialog.java
 *
 * Copyright (C) 2000-2005 Peter Graves
 * $Id: DescribeKeyDialog.java,v 1.7 2005-03-03 14:05:00 piso Exp $
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

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

public final class DescribeKeyDialog extends AbstractDialog
{
    private static final String title  = "Describe Key";
    private static final String prompt = "Describe key:";

    private final Editor editor;
    private final Buffer buffer;

    private JTextField textField;
    private boolean seenKeyPressed;
    private String keyStrokeText;
    private boolean eventHandled;
    private boolean failed;
    private boolean disposed;
    private KeyMap requestedKeyMap;
    private boolean local;

    private DescribeKeyDialog(Editor editor)
    {
        super(editor, title, true); // Modal.
        this.editor = editor;
        buffer = editor.getBuffer();
        textField = new JTextField(20);
        addLabelAndTextField(new JLabel(prompt), textField);
        addVerticalStrut();
        addCancel();
        pack();
        textField.setFocusTraversalKeysEnabled(false);
        textField.requestFocus();
    }

    public void keyPressed(KeyEvent e)
    {
        final int keycode = e.getKeyCode();
        // Ignore modifier keystrokes.
        if (keycode == KeyEvent.VK_SHIFT || keycode == KeyEvent.VK_CONTROL ||
            keycode == KeyEvent.VK_ALT || keycode == KeyEvent.VK_META)
            return;
        seenKeyPressed = true;
        final int modifiers = e.getModifiers();
        Object command = describeKey(e.getKeyChar(), keycode, modifiers);
        if (eventHandled || failed) {
            KeyMapping mapping = new KeyMapping(keycode, modifiers, null);
            if (keyStrokeText == null)
                keyStrokeText = mapping.getKeyText();
            else
                keyStrokeText = keyStrokeText + " " + mapping.getKeyText();
            textField.setText(keyStrokeText);
        }
        if (command != null)
            report(command);
    }

    public void keyTyped(KeyEvent e)
    {
        if (!eventHandled) {
            final char c = e.getKeyChar();
            final int modifiers = e.getModifiers();
            if (modifiers == 0 || modifiers == InputEvent.SHIFT_MASK) {
                // Ignore whitespace key chars (e.g. Space, Shift Tab).
                if (c > ' ') {
                    FastStringBuffer sb = new FastStringBuffer('\'');
                    sb.append(c);
                    sb.append('\'');
                    if (keyStrokeText == null)
                        keyStrokeText = sb.toString();
                    else
                        keyStrokeText = keyStrokeText + " " + sb.toString();
                    textField.setText(keyStrokeText);
                }
            }
            Object command = describeKey(c, e.getKeyCode(), modifiers);
            if (!eventHandled) {
                Log.debug("failed => true");
                failed = true;
            }
            if (command != null)
                report(command);
        }
    }

    public void keyReleased(KeyEvent e)
    {
        final int keycode = e.getKeyCode();
        if (keycode == KeyEvent.VK_SHIFT || keycode == KeyEvent.VK_CONTROL ||
            keycode == KeyEvent.VK_ALT || keycode == KeyEvent.VK_META)
            return;
        if (failed && !disposed) {
            dispose();
            // Use invokeLater() so message dialog will get focus.
            Runnable r = new Runnable() {
                public void run()
                {
                    MessageDialog.showMessageDialog(
                        editor,
                        keyStrokeText + " is not mapped", title);
                }
            };
            SwingUtilities.invokeLater(r);
        }
        eventHandled = false; // Start over.
    }

    private Object describeKey(char keyChar, int keyCode, int modifiers)
    {
        if (disposed)
            return null;
        // Mask off the bits we don't care about (Java 1.4).
        modifiers &= 0x0f;
        if (keyCode == 0 && modifiers == InputEvent.SHIFT_MASK) // Shift only.
            modifiers = 0; // Ignore modifier.
        KeyMapping mapping = null;
        if (requestedKeyMap != null) {
            mapping = requestedKeyMap.lookup(keyChar, keyCode, modifiers);
        } else {
            final Mode mode = buffer.getMode();
            mapping = mode.getKeyMap().lookup(keyChar, keyCode, modifiers);
            if (mapping != null)
                local = true;
            else
                mapping = KeyMap.getGlobalKeyMap().lookup(keyChar, keyCode,
                                                          modifiers);
        }
        if (mapping == null)
            return null;
        eventHandled = true;
        Object command = mapping.getCommand();
        if (command instanceof KeyMap) {
            requestedKeyMap = (KeyMap) command;
            return null;
        }
        return command;
    }

    private void report(Object command)
    {
        final FastStringBuffer sb = new FastStringBuffer();
        sb.append(keyStrokeText);
        sb.append(" is mapped to ");
        sb.append(command);
        if (local) {
            sb.append(" (");
            final Mode mode = buffer.getMode();
            sb.append(mode.toString());
            sb.append(" mode)");
        } else
            sb.append(" (global mapping)");
        dispose();
        // Use invokeLater() so message dialog will get focus.
        Runnable r = new Runnable() {
            public void run()
            {
                MessageDialog.showMessageDialog(editor, sb.toString(),
                                                "Describe Key");
            }
        };
        SwingUtilities.invokeLater(r);
    }

    public void dispose()
    {
        disposed = true;
        super.dispose();
    }

    public static void describeKey()
    {
        DescribeKeyDialog d = new DescribeKeyDialog(Editor.currentEditor());
        d.centerDialog();
        d.show();
    }
}
