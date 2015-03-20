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

import org.openda.uncertainties.FunctionParameter;

import javax.swing.JOptionPane;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;

import java.util.ArrayList;


/**
 * Table model for the function parameters tables.
 *
 */
public class FunctionParameterTableModel extends AbstractTableModel {

	private int numRow;
	private int numCol;
	private String[] columnNames;

	private static final int COLUMN_NAME = 0;
	public static final int COLUMN_VALUE = 1;

    private static final String ISFACTOR = "is a Factor";
    private static final String ISNOTFACTOR = "is not a Factor";

    //one of these is used to store a context object, depending on which
    //constructor (which type of context) is used to make this object.
    //The other contexts will stay null.
    private AutoCorrelationFunctionContext autoCorrelationFunctionContext = null;
	private PDFContext pdfContext = null;
	private VariationFunctionContext variationFunctionContext = null;


	public final Object[] longValues = { "longLocationName", new Double(0) };


	/**
	 * Constructors for different context objects all make a table for the parameters
	 * for the given context object.
	 *
	 * @param autoCorrelationFunctionContext
	 */
	//TODO if an interface/superclass for context objects would be made,
	//then this class could store a reference to superclass and only one
	//constructor would be needed. AK
	public FunctionParameterTableModel(AutoCorrelationFunctionContext autoCorrelationFunctionContext) {

		if (autoCorrelationFunctionContext == null) {
			throw new IllegalArgumentException("context == null");
		}
		this.autoCorrelationFunctionContext = autoCorrelationFunctionContext;

		this.numRow = 0;
		if (autoCorrelationFunctionContext.getParams().size() > 0)
			this.numRow = autoCorrelationFunctionContext.getParams().size();

		String[] columnNames = new String[2];
		columnNames[0] = "Name";
		columnNames[1] = "Value";
		this.setColumnNames(columnNames);
	}

	public FunctionParameterTableModel(VariationFunctionContext variationFunctionContext) {

		if (variationFunctionContext == null) {
			throw new IllegalArgumentException("context == null");
		}
		this.variationFunctionContext = variationFunctionContext;

		this.numRow = 0;
		if (variationFunctionContext.getParams().size() > 0)
			this.numRow = variationFunctionContext.getParams().size();

		String[] columnNames = new String[2];
		columnNames[0] = "Name";
		columnNames[1] = "Value";
		this.setColumnNames(columnNames);
	}

	public FunctionParameterTableModel(PDFContext pdfContext) {

		if (pdfContext == null) {
			throw new IllegalArgumentException("pdf == null");
		}
		this.pdfContext = pdfContext;

		this.numRow = 0;
		if (pdfContext.getParams().size() > 0)
			this.numRow = pdfContext.getParams().size();

		String[] columnNames = new String[2];
		columnNames[0] = "Name";
		columnNames[1] = "Value";
		this.setColumnNames(columnNames);
	}

	public void setColumnNames(String[] columnNames) {
		if (columnNames == null)
			throw new IllegalArgumentException("ColumnsNames == null ");

		this.numCol = columnNames.length;
		this.columnNames = columnNames;
	}

	/**
	 * Returns the number of rows in the model. A <code>JTable</code> uses
	 * this method to determine how many rows it should display. This method
	 * should be quick, as it is called frequently during rendering.
	 *
	 * @return the number of rows in the model
	 * @see #getColumnCount
	 */
	public int getRowCount() {
		return numRow;
	}

	/**
	 * Returns the number of columns in the model. A <code>JTable</code> uses
	 * this method to determine how many columns it should create and display by
	 * default.
	 *
	 * @return the number of columns in the model
	 * @see #getRowCount
	 */
	public int getColumnCount() {
		return numCol;
	}

	public String getColumnName(int col) {
		return this.columnNames[col];
	}

	public ArrayList<FunctionParameter> getPDFParams() {
		ArrayList<FunctionParameter> params = new ArrayList<FunctionParameter>();

		if (autoCorrelationFunctionContext != null) {
    		params = this.autoCorrelationFunctionContext.getParams();
		}
		else if (variationFunctionContext != null) {
    		params = this.variationFunctionContext.getParams();
		}
		else if (pdfContext != null) {
    		params = this.pdfContext.getParams();
		}

		return params;
	}

	/**
	 * Notifies all listeners that the value of the cell at
	 * <code>[row, column]</code> has been updated.
	 *
	 * @param row
	 *            row of cell which has been updated
	 * @param column
	 *            column of cell which has been updated
	 * @see javax.swing.event.TableModelEvent
	 * @see javax.swing.event.EventListenerList
	 */
	public void fireTableCellUpdated(int row, int column) {
		super.fireTableCellUpdated(row, column);
	}

	/**
	 * Forwards the given notification event to all
	 * <code>TableModelListeners</code> that registered themselves as
	 * listeners for this table model.
	 *
	 * @param e
	 *            the event to be forwarded
	 * @see #addTableModelListener
	 * @see javax.swing.event.TableModelEvent
	 * @see javax.swing.event.EventListenerList
	 */
	public void fireTableChanged(TableModelEvent e) {
		super.fireTableChanged(e);
	}

	/**
	 * Notifies all listeners that all cell values in the table's rows may have
	 * changed. The number of rows may also have changed and the
	 * <code>JTable</code> should redraw the table from scratch. The structure
	 * of the table (as in the order of the columns) is assumed to be the same.
	 *
	 * @see javax.swing.event.TableModelEvent
	 * @see javax.swing.event.EventListenerList
	 * @see javax.swing.JTable#tableChanged(javax.swing.event.TableModelEvent)
	 */
	public void fireTableDataChanged() {
		super.fireTableDataChanged();
	}

	/**
	 * Returns the value for the cell at <code>columnIndex</code> and
	 * <code>rowIndex</code>.
	 *
	 * @param rowIndex
	 *            the row whose value is to be queried
	 * @param columnIndex
	 *            the column whose value is to be queried
	 * @return the value Object at the specified cell
	 */
	public Object getValueAt(int rowIndex, int columnIndex) {

		if (columnIndex == -1) {
			return null;
		}
		if (rowIndex == -1) {
			return null;
		}
		Object obj = "";
		ArrayList<FunctionParameter> functionParameters = getPDFParams();
		if (functionParameters.size() > 0 && functionParameters.size() > rowIndex) {
			FunctionParameter currentFunctionParameter = functionParameters.get(rowIndex);
			if (currentFunctionParameter != null) {
				switch (columnIndex) {
				case COLUMN_NAME: // Name
					obj = currentFunctionParameter.getName();
					break;

				case COLUMN_VALUE:
					obj = String.valueOf(currentFunctionParameter.getValue());
					break;
				default:
					break;
				}
			}
		}

		return obj;
	}

	public boolean isCellEditable(int row, int col) {

		boolean isEditable = false;

		if (col == COLUMN_NAME) {
			isEditable = false;
		} else if (col == COLUMN_VALUE) {
			isEditable = true;
		}

		return isEditable;
	}

	public void setValueAt(Object value, int row, int col) {

		switch (col) {

			case COLUMN_NAME: // Name
				//Can't edit this
				break;

			case COLUMN_VALUE:

				try {
    				Double doubleValue = new Double(Double.parseDouble((String)value));
    				if (this.autoCorrelationFunctionContext != null) {
    	    			String paramType = autoCorrelationFunctionContext.getAutoCorrelationFunctionObject().getParams().get(row).getName();
	    	    		autoCorrelationFunctionContext.getAutoCorrelationFunctionObject().setParam(paramType, doubleValue);
    				}
    	    		else if (this.variationFunctionContext != null) {
    	    			String paramType = variationFunctionContext.getVariationFunctionObject().getParams().get(row).getName();
	    	    		variationFunctionContext.getVariationFunctionObject().setParam(paramType, doubleValue);
    				}
    	    		else if (this.pdfContext != null) {
    	    			String paramType = pdfContext.getPdfObject().getParams().get(row).getName();
	    	    		pdfContext.getPdfObject().setParam(paramType, doubleValue);
    				}
				} catch (NumberFormatException e) {
					//null because no reference to daToolsMainPanel.
	        		JOptionPane.showMessageDialog(null,
	        				"Please input a valid decimal number.",
	        				"Invalid input value",
	        				JOptionPane.WARNING_MESSAGE);
				} catch (IllegalArgumentException e) {
	        		JOptionPane.showMessageDialog(null,
	        				e.getMessage(),
	        				"Invalid input value",
	        				JOptionPane.WARNING_MESSAGE);
				}
				break;
		}
		fireTableCellUpdated(row, col);
		fireTableDataChanged();
		fireTableRowsUpdated(row, row);
	}
}
