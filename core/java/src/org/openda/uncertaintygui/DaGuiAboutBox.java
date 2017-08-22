/* OpenDA v2.4.1 
* Copyright (c) 2017 OpenDA Association 
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/


package org.openda.uncertaintygui;

import javax.swing.*;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.JTextComponent;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;
import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.net.URISyntaxException;

/**
 * About box for DA GUI
 */
@SuppressWarnings("serial")
public class DaGuiAboutBox extends JDialog implements PropertyChangeListener {
    private enum Option {
        Close;      // Currently, the about box has only one button
        public String toString() {
            return name();
        }
    }

    public DaGuiAboutBox(Frame owner) {
        super(owner, "DA GUI", true);
        setResizable(false);
        String aboutText = "<html><body>" +
                "<p>The DA GUI has been developed by <a href=\"http://www.deltares.nl\" target=\"new\">Deltares</a>.</p>" +
                "<p>It can be used in Data Assimilation environments like OpenDa (Deltares, TUDelft, Vortech) and DATools/UATools (Deltares) " +
                "The Data Assimilation GUI, and especially its uncertainty module, is based on " +
                "DUE, the Data Uncertainty Engine, " +
                "as developed by James D. Brown and Gerard B. M. Heuvelink " +
                " within the HarmoniRIB project (5th EU Framework Programme, EVK1-CT-2002-00109).</p>" +
                "<p>A significant part of the DAU and DAU-GUI developments were done in the " +
                "EU funded <a href=\"http://www.aquastress.net\" target=\"new\">AquaStress</a> project " +
                "(6th EU Framework Programme, GOCE-511231-2).</p>" +
                "</body></html>";
        JEditorPane ep = new JEditorPane("text/html", aboutText);
        ((HTMLDocument) ep.getDocument()).getStyleSheet().addRule("body { font-size: 12pt; font-family: Dialog; }");
        ep.setOpaque(false);
        ep.setEditable(false);
        setHyperlinkListener(ep);
        setSizes(ep, Math.max(ep.getMinimumSize().width, 400));
        JOptionPane op = new JOptionPane(ep, JOptionPane.PLAIN_MESSAGE, JOptionPane.DEFAULT_OPTION, null,
                Option.values());
        op.addPropertyChangeListener(JOptionPane.VALUE_PROPERTY, this);
        add(op);
        pack();
        setLocationRelativeTo(owner);
    }

    protected static void setHyperlinkListener(JEditorPane ep) {
        try {
            if (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.BROWSE)) {
                ep.addHyperlinkListener(new HyperlinkListener() {
                    public void hyperlinkUpdate(HyperlinkEvent e) {
                        if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED
                                && Desktop.isDesktopSupported()) {
                            Desktop desktop = Desktop.getDesktop();
                            try {
                                desktop.browse(e.getURL().toURI());
                            }
                            catch (URISyntaxException use) {
                                use.printStackTrace();
                            }
                            catch (IOException ioe) {
                                ioe.printStackTrace();
                            }
                        }
                    }
                });
                return;
            }
        }
        catch (NoClassDefFoundError ignored) {
        }
        ((HTMLEditorKit) ep.getEditorKit()).setLinkCursor(null);
    }

    public void propertyChange(PropertyChangeEvent e) {
        if (isVisible()) {
            Object v = e.getNewValue();
            if (!(v instanceof Option)) return;
            switch ((Option) v) {
                case Close:
                    dispose();
                    break;
            }
            Object s = e.getSource();
            if (s instanceof JOptionPane) ((JOptionPane) s).setValue(JOptionPane.UNINITIALIZED_VALUE);
        }
    }

    protected static void setSizes(JTextComponent c, int width) {
        Dimension max = new Dimension(width, Integer.MAX_VALUE);
        c.setSize(max);
        Dimension min = new Dimension(max.width, c.getPreferredSize().height);
        c.setMaximumSize(max);
        c.setMinimumSize(min);
        c.setPreferredSize(min);
    }
}
