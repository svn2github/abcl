/*
 * ToolBar.java
 *
 * Copyright (C) 2000-2002 Peter Graves
 * $Id: ToolBar.java,v 1.2 2002-10-02 18:27:06 piso Exp $
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

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JToolBar;
import org.xml.sax.AttributeList;
import org.xml.sax.DocumentHandler;
import org.xml.sax.HandlerBase;
import org.xml.sax.InputSource;
import org.xml.sax.Parser;
import org.xml.sax.SAXException;

public class ToolBar extends JToolBar implements ActionListener, ToolBarConstants
{
    private static final int STYLE_DEFAULT   = 0;
    private static final int STYLE_TEXT_ONLY = 1;
    private static final int STYLE_ICON_ONLY = 2;

    private static final Preferences preferences = Editor.preferences();

    protected Frame frame;
    protected int style = STYLE_DEFAULT;

    public ToolBar(Frame frame)
    {
        this(frame, STYLE_DEFAULT);
    }

    public ToolBar(Frame frame, int style)
    {
        this.frame = frame;
        this.style = style;
        setFloatable(false);
    }

    public ToolBarButton addButton(String text, String iconFile, String methodName)
    {
        ToolBarButton button = new ToolBarButton(frame, methodName, this);
        switch (style) {
            case STYLE_DEFAULT:
                if (textEnabled())
                    button.setText(text);
                else
                    button.setToolTipText(text);
                if (iconsEnabled())
                    button.setIconFromFile(iconFile);
                button.setHorizontalTextPosition(ToolBarButton.CENTER);
                button.setVerticalTextPosition(ToolBarButton.BOTTOM);
                break;
            case STYLE_ICON_ONLY:
                button.setIconFromFile(iconFile);
                button.setToolTipText(text);
                break;
            case STYLE_TEXT_ONLY:
                button.setText(text);
                break;
        }
        // Setting the minimum button size doesn't work with Java 1.4.0-beta.
        if (!System.getProperty("java.version").startsWith("1.4")) {
            if (button.getText() != null && button.getText().length() != 0) {
                Dimension dim = button.getPreferredSize();
                if (dim.width < 40) {
                    dim.width = 40;
                    button.setMaximumSize(dim);
                    button.setMinimumSize(dim);
                }
            }
        }
        button.setRolloverEnabled(isRolloverEnabled());
        add(button);
        return button;
    }

    public ToolBarButton maybeAddInboxButton()
    {
        if (Editor.isMailEnabled())
            if (preferences.getStringProperty(Property.INBOX) != null)
                return addButton("Inbox", ICON_MAIL_INBOX, "inbox");
        return null;
    }

    public static final boolean isToolBarEnabled()
    {
        return textEnabled() || iconsEnabled();
    }

    private static final boolean textEnabled()
    {
        // Defaults to true for j's default look and feel.
        return preferences.getBooleanProperty(Property.TOOL_BAR_SHOW_TEXT,
            Editor.lookAndFeel == null);
    }

    private static final boolean iconsEnabled()
    {
        // Defaults to true in all cases.
        return preferences.getBooleanProperty(Property.TOOL_BAR_SHOW_ICONS);
    }

    public static final boolean isRolloverEnabled()
    {
        // Defaults to true for j's default look and feel.
        return preferences.getBooleanProperty(Property.TOOL_BAR_IS_ROLLOVER,
            Editor.lookAndFeel == null);
    }

    public void actionPerformed(ActionEvent e)
    {
        final Editor editor = frame.getCurrentEditor();
        editor.setFocusToDisplay();
        editor.getDispatcher().actionPerformed(e);
    }

    public static ToolBar createToolBar(Frame frame, File file)
    {
        if (file == null)
            return null;
        if (!file.isFile())
            return null;
        try {
            Parser parser = (Parser) Class.forName(
                "org.armedbear.j.aelfred.SAXDriver").newInstance();
            ToolBar toolBar = new ToolBar(frame);
            Handler handler = new Handler(toolBar);
            parser.setDocumentHandler(handler);
            InputSource inputSource = new InputSource(file.getInputStream());
            parser.parse(inputSource);
            return toolBar;
        }
        catch (Exception e) {
            Log.error(e);
            return null;
        }
    }

    private static class Handler extends HandlerBase implements DocumentHandler
    {
        private final ToolBar toolBar;

        public Handler(ToolBar toolBar)
        {
            this.toolBar = toolBar;
        }

        public void startElement(String name, AttributeList attributes)
            throws SAXException
        {
            if (name.equals("button")) {
                String label = attributes.getValue("label");
                String icon = attributes.getValue("icon");
                String command = attributes.getValue("command");
                toolBar.addButton(label, icon, command);
            } else if (name.equals("separator"))
                toolBar.addSeparator();
        }
    }
}
