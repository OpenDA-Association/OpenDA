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

import org.openda.uncertainties.UncertainItem;
import org.openda.uncertainties.Uncertainties;
import org.openda.uncertainties.autocorrelationfunctions.AutoCorrelationFunction;
import org.openda.uncertainties.pdfs.PDF;

import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;


public class CorrelationTableModel extends AbstractTableModel {

	//constants for the indices of the columns.
	public static final int COLUMN_NAME = 0;
	public static final int COLUMN_CORRELATION_TYPE = 1;

	//to temporarily store indices of current selected column and row.
    //used by method checkBox_actionListener.
	private int currentCol = 0;
	private int currentRow = 0;

    //to store the names of the column headers.
    private String[] columnNames;

    //long values of possible data so that column widths
    //will be initialized large enough to store long data values.
    public final Object[] longValues = {"longLocationName", "No Auto Correlation"};

    //to store a reference to a model class containing the data for the table.
    private UIModel uaUIModel = null;


    /**
     * Constructor to create a correlation table model with a reference
     * to the supplied ArrayList with uncertainties.
     *
     * @param	ArrayList<UaUncertainty> uncertainties a list of uncertainties to be
     * 			referenced by this correlation table model.
     */
    public CorrelationTableModel(UIModel uaUIModel) {

    	this.uaUIModel = uaUIModel;

        //initialize column names.
		String[] columnNames = new String[2];
		columnNames[0] = "Name";
		columnNames[1] = "Auto Correlation Type";
		setColumnNames(columnNames);
    }

	/**
	 * Returns the value for the cell at <code>columnIndex</code> and
	 * <code>rowIndex</code>.
	 *
	 * @param rowIndex    the row whose value is to be queried
	 * @param columnIndex the column whose value is to be queried
	 * @return the value Object at the specified cell
	 */
	public Object getValueAt(int rowIndex, int columnIndex) {
		if (columnIndex == -1) {
			return null;
		}
		if (rowIndex == -1) {
			return null;
		}

		currentCol = columnIndex;
		currentRow = rowIndex;
		Object obj = null;

		//get the active normal pdf with number rowIndex.
		Uncertainties uncertaintiesObject = this.getUncertaintiesObject();
		PDF activeNormalPdf = uncertaintiesObject.getActiveNormalPdf(rowIndex);
		if (activeNormalPdf != null) {
   			switch (columnIndex) {
				case COLUMN_NAME: //Name
					obj = activeNormalPdf.getUncertainItem().getId();
					break;

				case COLUMN_CORRELATION_TYPE: //auto correlation type.

					//get autocorrelation object with same Id as activePdf.
					AutoCorrelationFunction autoCorrelationFunction = uncertaintiesObject
					    .getAutoCorrelationFunction(activeNormalPdf.getUncertainItem().getId());

					if (autoCorrelationFunction != null) {
						obj = new AutoCorrelationFunctionContext(autoCorrelationFunction);
					}
					else {//if no autoCorrelationFunction exists for activePdf.
						//make empty AutoCorrelationFunctionContext object.
						obj = new AutoCorrelationFunctionContext(null);
					}
					break;

				default:
					break;
			}
		}

		return obj;
	}

    /**
     * Returns whether the cell with the supplied row and column indices
     * is editable.
     *
     * @param rowIndex the row index
     * @param colIndex the column index
     * @return true if the cell is editable
     */
	public boolean isCellEditable(int rowIndex, int colIndex) {
		currentCol = colIndex;
		currentRow = rowIndex;

		boolean returnValue = false;
		if (colIndex >= COLUMN_NAME && colIndex <= COLUMN_CORRELATION_TYPE) {
			returnValue = true;
		}
		return returnValue;
	}

    /**
     * Sets the value in the cell at <code>col</code> and
     * <code>row</code> to <code>value</code>.
     *
     * @param	value	 	the new value
     * @param	rowIndex	the row whose value is to be changed
     * @param	colIndex 	the column whose value is to be changed
     */
	public void setValueAt(Object value, int rowIndex, int colIndex) {
		this.currentCol = colIndex;
		this.currentRow = rowIndex;

		//get the activePDF with number rowIndex.
		Uncertainties uncertaintiesObject = this.getUncertaintiesObject();
		PDF activeNormalPdf = uncertaintiesObject.getActiveNormalPdf(rowIndex);
		if (activeNormalPdf != null) {
			switch (colIndex) {
				case COLUMN_NAME: //Name
					activeNormalPdf.getUncertainItem().setId((String) value);
					break;

				case COLUMN_CORRELATION_TYPE: //correlation model
					//get autocorrelation object with same Id as activePdf.
					AutoCorrelationFunction autoCorrelationFunction = uncertaintiesObject
					    .getAutoCorrelationFunction(activeNormalPdf.getUncertainItem().getId());
   					//remove current autoCorrelationFunction if present.
					if (autoCorrelationFunction != null) {
						uncertaintiesObject.removeAutoCorrelationFunction(autoCorrelationFunction);
					}

					//get correlation from cell editor combobox.
					AutoCorrelationFunctionContext autoCorrelationFunctionContextFromComboBox = (AutoCorrelationFunctionContext) value;
					AutoCorrelationFunction autoCorrelationFunctionFromComboBox = autoCorrelationFunctionContextFromComboBox.getAutoCorrelationFunctionObject();
					if (autoCorrelationFunctionFromComboBox == null) {
						//no autocorrelation set, thus do not add new autocorrelation
						//for this activePdf in uncertaintiesObject.
					}
					else {//autoCorrelationFunctionFromComboBox != null.
						//specific autocorrelation set, thus add new
						//autocorrelationfunction for this activePdf to uncertaintiesObject.

						//Every cell in correlation type column has the same celleditor, thus
						//editor returns the same model context objects for each cell,
						//so make a new model context (clone) to store in data model,
						//so that different uncertainties (rows) do not end up pointing to
						//the same model context objects.

						//make a copy (clone) of correlation from cell editor combobox.
						AutoCorrelationFunction clonedAutoCorrelationFunction = autoCorrelationFunctionFromComboBox.clone();
						//get uncertainItem from current activePdf and add to cloned object.
						UncertainItem uncertainItem = activeNormalPdf.getUncertainItem();
						clonedAutoCorrelationFunction.setUncertainItem(uncertainItem);

						//add new autoCorrelationFunction to uncertaintiesObject.
						uncertaintiesObject.addAutoCorrelation(clonedAutoCorrelationFunction);
					}
					break;

				default:
					break;
			}
		}

		//Make sure that ParameterAndGraphPanel is updated,
		//by firing an event that the table controller listens to.
		//All these three methods eventually call AbstractTableModel.fireTableChanged().
		fireTableCellUpdated(rowIndex, colIndex);
		fireTableDataChanged();
		fireTableRowsUpdated(rowIndex, rowIndex);
	}

    /**
     * Set the names of the columns for this tablemodel.
     *
     * @param columnNames an array with column names
     */
    private void setColumnNames(String[] columnNames) {
        if (columnNames==null) {
            throw new IllegalArgumentException("ColumnNames == null ") ;
        }

        this.columnNames = columnNames;
    }

    /**
     * Get the name of the column with supplied column index.
     *
     * @param col the column index
     * @return column name
     */
    public String getColumnName(int col) {
        return this.columnNames[col];
    }

    /**
     * Get the uncertaintiesObject which contains the autocorrelations for this tablemodel.
     *
     * @return UncertaintiesObject
     */
    public Uncertainties getUncertaintiesObject() {
        return this.uaUIModel.getUncertaintiesObject();
    }

    /**
     * Returns the number of rows in the model. A
     * <code>JTable</code> uses this method to determine how many rows it
     * should display.  This method should be quick, as it
     * is called frequently during rendering.
     *
     * @return the number of rows in the model
     * @see #getColumnCount
     */
    public int getRowCount() {
		//return the number of active normal distributed pdfs in uncertaintiesObject.
        Object uncertaintiesObject = this.uaUIModel.getUncertaintiesObject();
        if (uncertaintiesObject != null) {
            return this.uaUIModel.getUncertaintiesObject().activeNormalPdfCount();
        } else {
            return 0;
        }
    }

    /**
     * Returns the number of columns in the model. A
     * <code>JTable</code> uses this method to determine how many columns it
     * should create and display by default.
     *
     * @return the number of columns in the model
     * @see #getRowCount
     */
    public int getColumnCount() {
        return columnNames.length;
    }

    /**
     * Notifies all listeners that the value of the cell at
     * <code>[row, column]</code> has been updated.
     *
     * @param row    row of cell which has been updated
     * @param column column of cell which has been updated
     * @see javax.swing.event.TableModelEvent
     * @see javax.swing.event.EventListenerList
     */
    public void fireTableCellUpdated(int row, int column) {
        super.fireTableCellUpdated(row, column);
    }

    /**
     * Forwards the given notification event to all
     * <code>TableModelListeners</code> that registered
     * themselves as listeners for this table model.
     *
     * @param e the event to be forwarded
     * @see #addTableModelListener
     * @see javax.swing.event.TableModelEvent
     * @see javax.swing.event.EventListenerList
     */
    public void fireTableChanged(TableModelEvent e) {
        super.fireTableChanged(e);
    }

    /**
     * Notifies all listeners that all cell values in the table's
     * rows may have changed. The number of rows may also have changed
     * and the <code>JTable</code> should redraw the
     * table from scratch. The structure of the table (as in the order of the
     * columns) is assumed to be the same.
     *
     * @see javax.swing.event.TableModelEvent
     * @see javax.swing.event.EventListenerList
     * @see javax.swing.JTable#tableChanged(javax.swing.event.TableModelEvent)
     */
    public void fireTableDataChanged() {
        super.fireTableDataChanged();
    }

	public int getCurrentRow() {
		return currentRow;
	}
}
