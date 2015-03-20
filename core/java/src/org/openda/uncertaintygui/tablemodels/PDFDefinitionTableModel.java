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
import org.openda.uncertainties.pdfs.NormalDistribution;
import org.openda.uncertainties.pdfs.PDF;

import javax.swing.*;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;

public class PDFDefinitionTableModel extends AbstractTableModel {

	//constants for the indices of the columns.
	public static final int COLUMN_IS_ACTIVE = 0;
	public static final int COLUMN_NAME = 1;
	public static final int COLUMN_BASIC_VALUE = 2;
	public static final int COLUMN_PDF_TYPE = 3;

	//checkbox for active column. This same checkbox is added to all rows,
    //thus its mouse listener
	//fires same events for each row. Method checkBox_actionListener
	//checks which row was clicked, using int currentRow, before
    //changing active value for corresponding uncertainty object.
    //The method getValue needs to return this checkbox, so that it
    //is visible in the table.
	private JCheckBox activeColumnCheckBox = new JCheckBox();

	//to temporarily store indices of current selected column and row.
    //used by method checkBox_actionListener.
	private int currentColumn = 0;
	private int currentRow = 0;

    //to store the names of the column headers.
    private String[] columnNames;

    //long values of possible data so that column widths
    //will be initialized large enough to store long data values.
    //These objects are only used for determining column widths.
    public final Object[] longValues = {new Boolean(true),
            "longLocationName", "use actual value", "Gumbel Maximum"};

    //to store a reference to a class containing the data for the table.
    private UIModel uaUIModel = null;


    /**
     * Constructor to create a table model with a reference
     * to the supplied uaUIModel with data.
     *
     * @param	uaUIModel that has stored data to be referenced
     *          by this table model.
     */
    public PDFDefinitionTableModel(UIModel uaUIModel) {

    	this.uaUIModel = uaUIModel;

		//initialize column names.
        String[] columnNames = new String[4];
        columnNames[0] = "Active";
        columnNames[1] = "Name";
        columnNames[2] = "Basic Value";
        columnNames[3] = "PDF Type";
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

		currentColumn = columnIndex;
		currentRow = rowIndex;
		Object obj = null;

		Uncertainties uncertaintiesObject = this.getUncertaintiesObject();
		PDF pdf = uncertaintiesObject.getPdf(rowIndex);
		if (pdf != null) {
			switch (columnIndex) {

				case COLUMN_IS_ACTIVE: //checkbox
					//returns the activeColumnCheckBox after setting this checkbox's parameters
					//to match the currentUncertaintyRow's state.
					activeColumnCheckBox.setSelected(pdf.getUncertainItem().isActive());
					obj = activeColumnCheckBox;
					break;

				case COLUMN_NAME: //Name
					obj = pdf.getUncertainItem().getId();
					break;

                case COLUMN_BASIC_VALUE:
                    double basicValue = pdf.getUncertainItem().getBasicValue();

                    if (Double.compare(basicValue, Double.NaN) == 0) {
                    	//if not a number, then actual value is used.
                    	obj = "use actual value";
                    }
                    else {
                    	obj = String.valueOf(basicValue);
                    }
                    break;

				case COLUMN_PDF_TYPE: //pdf type
                    obj = new PDFContext(pdf);
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
		currentColumn = colIndex;
		currentRow = rowIndex;

		boolean returnValue = false;
		if (colIndex >= COLUMN_NAME && colIndex <= COLUMN_PDF_TYPE) {
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
		this.currentColumn = colIndex;
		this.currentRow = rowIndex;

		Uncertainties uncertaintiesObject = this.getUncertaintiesObject();
    	PDF pdf = uncertaintiesObject.getPdf(rowIndex);
    	if (pdf != null) {
			switch (colIndex) {

				case COLUMN_IS_ACTIVE: // check box
					//this column is not editable.
					//editing is done using the checkbox in this column.
					//see method PDFDefinitionTableController.checkBoxActionListener.
					break;

				case COLUMN_NAME: // Name
					pdf.getUncertainItem().setId((String) value);
					break;

				case COLUMN_BASIC_VALUE:
					String string = (String) value;
					if (string != null) {
						//remove redundant whitespace.
						String tempString = string.trim().replaceAll("\\s+", " ");
						if (tempString.equalsIgnoreCase("use actual value")
       							|| tempString.equalsIgnoreCase("actual value")
	     						|| tempString.equalsIgnoreCase("use actual")
	   		    				|| tempString.equalsIgnoreCase("actual")) {
							//set to not a number to indicate use of actual value.
							pdf.getUncertainItem().setBasicValue(Double.NaN);
						}
						else {
							//if string not use actual value, then should be decimal number.
							try {
    							Double doubleValue = new Double(Double.parseDouble(string));
								pdf.getUncertainItem().setBasicValue(doubleValue);
							} catch (NumberFormatException e) {
								JOptionPane.showMessageDialog(null,
										"Please input a valid decimal number or input 'use actual value'.",
										"Invalid input value",
										JOptionPane.WARNING_MESSAGE);
							} catch (IllegalArgumentException e) {
								JOptionPane.showMessageDialog(null, e.getMessage(),
										"Invalid input value",
										JOptionPane.WARNING_MESSAGE);
							}
						}
					}
					else {//if string == null
						JOptionPane.showMessageDialog(null, "Invalid input value",
								"Invalid input value",
								JOptionPane.WARNING_MESSAGE);
					}
					break;

				case COLUMN_PDF_TYPE: // pdf type
					//Every cell in pdf type column has the same celleditor, thus
					//editor returns the same model context objects for each cell,
					//so make a new model context (clone) to store in data model,
					//so that different uncertainties (rows) do not end up pointing to
					//the same model context objects.

					//get pdf from cell editor combobox and make a copy (clone).
					PDFContext pdfContextFromComboBox = (PDFContext) value;
					PDF pdfFromComboBox = pdfContextFromComboBox.getPdfObject();
					PDF clonedPdf = pdfFromComboBox.clone();

					//get uncertainItem from current pdf object and add to clone.
					UncertainItem uncertainItem = pdf.getUncertainItem();
					clonedPdf.setUncertainItem(uncertainItem);

					//store clone in data model.
					uncertaintiesObject.setPDF(rowIndex, clonedPdf);

					//if pdf set to anything but normal distribution.
					if (!(clonedPdf instanceof NormalDistribution)) {
						//remove autocorrelation for pdf, if present.

						//get autocorrelation object with same Id as pdf.
						AutoCorrelationFunction autoCorrelationFunction =
							this.getUncertaintiesObject()
									.getAutoCorrelationFunction(clonedPdf.getUncertainItem().getId());
						//remove autoCorrelationFunction if present.
						if (autoCorrelationFunction != null) {
							this.getUncertaintiesObject()
									.removeAutoCorrelationFunction(autoCorrelationFunction);
						}
					}

					//update only correlationTable (via parentController and correlationTableController).
					//TODO update correlationTable not possible without reference to
					//mainPanelController or CorrelationTableController in this class. AK
			        //this.parentController.getCorrelationTableController().updateCorrelationTable(this.parentController.getModel());
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
     * Get the uncertaintiesObject which contains the pdfs for this tablemodel.
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
        Object uncertaintiesObject = this.uaUIModel.getUncertaintiesObject();
        if (uncertaintiesObject != null) {
            return this.uaUIModel.getUncertaintiesObject().pdfCount();
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

    /**
     * Get the checkbox used in the pdf definition table for the isActive column.
     * @return JCheckBox activeColumnCheckBox
     */
	public JCheckBox getActiveColumnCheckBox() {
		return this.activeColumnCheckBox;
	}

	/**
	 * Get the row that was last manipulated by the user.
	 * @return int current row index
	 */
	public int getCurrentRow() {
		return this.currentRow;
	}

	/**
	 * Get the column that was last manipulated by the user.
	 * @return int current coulmn index
	 */
	public int getCurrentColumn() {
		return this.currentColumn;
	}
}
