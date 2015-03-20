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


import javax.swing.table.TableCellRenderer;
import javax.swing.*;
import javax.swing.border.Border;


import java.awt.*;

/**
 * Custom render class for tables.
 */
public class TableRenderer implements TableCellRenderer {
    private Component component;
    private Border selectionBorder = BorderFactory.createEtchedBorder(Color.black,
            new Color(134, 134, 134));

    public TableRenderer(Component component) {
        this.component = component;
    }

    /**
     *  Returns the component used for drawing the cell.  This method is
     *  used to configure the renderer appropriately before drawing.
     *
     * @param	table		the <code>JTable</code> that is asking the
     *				renderer to draw; can be <code>null</code>
     * @param	value		the value of the cell to be rendered.  It is
     *				up to the specific renderer to interpret
     *				and draw the value.  For example, if
     *				<code>value</code>
     *				is the string "true", it could be rendered as a
     *				string or it could be rendered as a check
     *				box that is checked.  <code>null</code> is a
     *				valid value
     * @param	isSelected	true if the cell is to be rendered with the
     *				selection highlighted; otherwise false
     * @param	hasFocus	if true, render cell appropriately.  For
     *				example, put a special border on the cell, if
     *				the cell can be edited, render in the color used
     *				to indicate editing
     * @param	row	        the row index of the cell being drawn.  When
     *				drawing the header, the value of
     *				<code>row</code> is -1
     * @param	column	        the column index of the cell being drawn
     */
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {

        Component component = this.component;

        if (value instanceof JCheckBox) {
                ((JCheckBox) value).setOpaque(true);
                return renderedComponent(table, (JCheckBox) value, isSelected, row, column);
        } else if (component instanceof JTextField) {
            ((JTextField)  this.component).setOpaque(true);
            return renderedComponent((JTextField) component, table, value, isSelected, row, column);
        } else if (component instanceof JCheckBox) {
            ((JCheckBox) this.component).setOpaque(true);
            return renderedComponent((JCheckBox) component, table, value, isSelected, row, column);
        } else if (component instanceof JComboBox) {
            ((JComboBox) this.component).setOpaque(true);
            return renderedComponent((JComboBox) component, table, value, isSelected, row, column);
        }
        return component;
    }

    private Component renderedComponent(JComboBox comboBox, JTable table, Object value, boolean isSelected, int row, int column) {

        //Set border of the selected editable cells
        if (isSelected && table.isCellEditable(row, column)) {

        	comboBox.setBorder(selectionBorder);
        } else {

        	comboBox.setBorder(null);
        }

        if (value instanceof PDFContext) {
        	PDFContext pdfContext = (PDFContext) value;
        	PDFContext pdfContextInComboBox = null;
        	for (int i = 0; i < comboBox.getItemCount(); i++) {

        		PDFContext pdfContextTemp = (PDFContext) comboBox.getItemAt(i);
        		if(pdfContextTemp.toString().equals(pdfContext.toString())) {
            		pdfContextInComboBox = pdfContextTemp;
            		break;
            	}
            }
        	comboBox.setSelectedItem(pdfContextInComboBox);
        }
        else if (value instanceof VariationFunctionContext) {
        	VariationFunctionContext variationFunctionContext = (VariationFunctionContext) value;
        	VariationFunctionContext variationFunctionContextInComboBox = null;
        	for (int i = 0; i < comboBox.getItemCount(); i++) {

        		VariationFunctionContext variationFunctionContextTemp = (VariationFunctionContext) comboBox.getItemAt(i);
        		if(variationFunctionContextTemp.toString().equals(variationFunctionContext.toString())) {
            		variationFunctionContextInComboBox = variationFunctionContextTemp;
            		break;
            	}
            }
        	comboBox.setSelectedItem(variationFunctionContextInComboBox);
        }
        else if (value instanceof AutoCorrelationFunctionContext) {
        	AutoCorrelationFunctionContext autoCorrelationFunctionContext = (AutoCorrelationFunctionContext) value;
        	AutoCorrelationFunctionContext autoCorrelationFunctionContextInComboBox = null;
        	for (int i = 0; i < comboBox.getItemCount(); i++) {

        		AutoCorrelationFunctionContext autoCorrelationFunctionContextTemp = (AutoCorrelationFunctionContext) comboBox.getItemAt(i);
        		if(autoCorrelationFunctionContextTemp.toString().equals(autoCorrelationFunctionContext.toString())) {
            		autoCorrelationFunctionContextInComboBox = autoCorrelationFunctionContextTemp;
            		break;
            	}
            }
        	comboBox.setSelectedItem(autoCorrelationFunctionContextInComboBox);
        }

    	return comboBox;

    }

    private Component renderedComponent(JTable table, JCheckBox checkBox, boolean isSelected, int row, int column) {

    	Color backgroundColor;
        //Color the cells
        if (table.isCellEditable(row, column)) {
            backgroundColor = Color.white;
        } else {
            backgroundColor = new Color(0xE6E6E6); //extra light gray
        }

        Color foregroundColor = Color.black;

        //Display value in the cell
        checkBox.setAlignmentY(Component.CENTER_ALIGNMENT);
        checkBox.setAlignmentX(Component.CENTER_ALIGNMENT);
        checkBox.setBackground(backgroundColor);
        checkBox.setForeground(foregroundColor);

        checkBox.setText("");
        return checkBox;
    }

    //render JCheckbox with a supplied boolean value to indicate the selection.
    private Component renderedComponent(JCheckBox checkBox, JTable table, Object value, boolean isSelected, int row, int column) {
        //Set border of the selected editable cells
        if (isSelected && table.isCellEditable(row, column)) {
            checkBox.setBorder(selectionBorder);
        } else {
            checkBox.setBorder(null);
        }

        Color backgroundColor;
        //Color the cells
        if (table.isCellEditable(row, column)) {
            backgroundColor = Color.white;
        } else {
            backgroundColor = new Color(0xE6E6E6); //extra light gray
        }

        Color foregroundColor = Color.black;

        //Display value in the cell
        checkBox.setAlignmentY(Component.CENTER_ALIGNMENT);
        checkBox.setBackground(backgroundColor);
        checkBox.setForeground(foregroundColor);
        checkBox.setSelected(((Boolean) value));
        return checkBox;
    }

    private Component renderedComponent(JTextField field, JTable table, Object value, boolean isSelected, int row, int column) {

        //Set border of the selected editable cells
        if (isSelected && table.isCellEditable(row, column)) {
            field.setBorder(selectionBorder);
        } else {
            field.setBorder(null);
        }

        Color backgroundColor;
        //Color the cells
        if (table.isCellEditable(row, column)) {
            backgroundColor = Color.white;
        } else {
            backgroundColor = new Color(0xE6E6E6); //extra light gray
        }

        Color foregroundColor = Color.black;

        //Display value in the cell
        field.setBackground(backgroundColor);
        field.setForeground(foregroundColor);
        if (value != null) {
            field.setText(String.valueOf(value));
        } else {
            field.setText("");
        }
        return field;
    }


}
