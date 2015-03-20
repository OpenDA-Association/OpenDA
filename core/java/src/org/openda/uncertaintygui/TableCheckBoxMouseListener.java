/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
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
import javax.swing.table.TableColumnModel;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;

public class TableCheckBoxMouseListener implements MouseListener {
    private JTable __table;

    private void __forwardEventToButton(MouseEvent e) {
        TableColumnModel columnModel = __table.getColumnModel();
        int column = columnModel.getColumnIndexAtX(e.getX());
        int row = e.getY() / __table.getRowHeight();
        Object value;
        JCheckBox checkBox;
        MouseEvent checkBoxEvent;

        if (row >= __table.getRowCount() || row < 0 ||
                column >= __table.getColumnCount() || column < 0)
            return;

        value = __table.getValueAt(row, column);

        if (!(value instanceof JCheckBox))
            return;

        checkBox = (JCheckBox) value;

        checkBoxEvent =
                (MouseEvent) SwingUtilities.convertMouseEvent(__table, e, checkBox);
        checkBox.dispatchEvent(checkBoxEvent);
        // This is necessary so that when a button is pressed and released
        // it gets rendered properly.  Otherwise, the button may still appear
        // pressed down when it has been released.
        __table.repaint();
    }

    public TableCheckBoxMouseListener(JTable table) {
        __table = table;
    }

    public void mouseClicked(MouseEvent e) {
        __forwardEventToButton(e);
    }

    public void mouseEntered(MouseEvent e) {
        __forwardEventToButton(e);
    }

    public void mouseExited(MouseEvent e) {
        __forwardEventToButton(e);
    }

    public void mousePressed(MouseEvent e) {
        __forwardEventToButton(e);
    }

    public void mouseReleased(MouseEvent e) {
        __forwardEventToButton(e);
    }
}
