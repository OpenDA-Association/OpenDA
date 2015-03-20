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

package org.openda.uncertaintygui.tablemodels;

import java.awt.Color;
import java.awt.Component;

import javax.swing.AbstractCellEditor;
import javax.swing.BorderFactory;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.table.TableCellEditor;

/**
 * Custom table cell editor class to adjust editing of text cells.
 * This class is also used for editing cells with numbers.
 *
 */
public class TextCellEditor extends AbstractCellEditor implements TableCellEditor {

	//create textField component for editing the cell.
	private JTextField textField = new JTextField();

	//border for cell when cell is being edited.
    private Border cellBorder = BorderFactory.createEtchedBorder(Color.black,
            new Color(134, 134, 134));


	/**
	 * This method is called when a cell is being edited by the user and
	 * returns a component to handle the editing.
	 *
	 * @param table the table which contains the cell
	 * @param value the current value of the cell at (row, column)
	 * @param isSelected whether the cell is selected
	 * @param row the row of the cell
	 * @param column the column of the cell
	 * @return textField the component used for editing the cell
	 */
	public Component getTableCellEditorComponent(JTable table, Object value,
			boolean isSelected, int row, int column) {

       	this.textField.setBorder(cellBorder);

       	//set text in editor textField to current cell value.
		this.textField.setText((String) value);

		//select all text in editor textfield.
		//TODO selecting does not work. AK
		this.textField.selectAll();

		return this.textField;
	}

	/**
	 * This method is called when editing is finished and returns
	 * the edited value to be stored in the cell.
	 *
	 * @return the value of the cell after editing
	 */
	public Object getCellEditorValue() {
		//return the edited value in the editor textField.
		return this.textField.getText();
	}
}
