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

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;

/**
 * Table Model for the results table.
 * User: Juzer Dhondia
 * Date: 9-jul-2007
 * Time: 11:04:46
 */
public class ResultSelectionModel extends AbstractTableModel {
    private int numRow;
    private int numCol;
    private int currentRow;

    //to store the names of the column headers.
    private String[] columnNames;

    private static final int COLUMN_IS_ACTIVE = 0;
    public static final int COLUMN_NAME = 1;
    public static final int COLUMN_DESCRIPTION = 2;


    //TODO refactor to use auToolsConfigurationObject with reference to
    //result selections instead of arraylist here. AK
    private ArrayList<SelectedResult> resultSelections;


    //long values of possible data so that column widths
    //will be initialized large enough to store long data values.
    //These objects are only used for determining column widths.
    public final Object[] longValues = {true,
            "longLocationName",
            "extremelylongVariableDescriptioninfactverylong"};

    private JCheckBox checkBoxIsActive = new JCheckBox();

    public ResultSelectionModel(ArrayList<SelectedResult> resultSelections) {

        checkBoxIsActive.addMouseListener(new MouseListener() {
            public void mouseClicked(MouseEvent e) {
                checkBox_actionListener(e);
            }

            public void mouseEntered(MouseEvent e) {
            }

            public void mouseExited(MouseEvent e) {
            }

            public void mousePressed(MouseEvent e) {
            }

            public void mouseReleased(MouseEvent e) {
            }
        });

        if (resultSelections == null) {
            resultSelections = new ArrayList();
        }
        if (resultSelections.size()<=0) {
            resultSelections.add(new SelectedResult("Enter Id here", "Enter Description here ", false)) ;
        }

        this.resultSelections = resultSelections;
        this.numRow = 1;
        if (resultSelections.size() > 0) this.numRow = resultSelections.size();
        String[] columnNames = new String[3];
        columnNames[0] = "Active";
        columnNames[1] = "Name";
        columnNames[2] = "Description";
        setColumnNames(columnNames);
    }


    public void setColumnNames(String[] columnNames) {
        if (columnNames == null)
            throw new IllegalArgumentException("ColumnsNames == null ");

        this.numCol = columnNames.length;   //
        this.columnNames = columnNames;
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
        return numRow;
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
        return numCol;
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

        if (columnIndex == -1) return null;
        currentRow = rowIndex;
        Object obj = null;

        if (resultSelections.size() > 0) {
            SelectedResult selectedResult = resultSelections.get(rowIndex);
            if (selectedResult!=null) {
                switch (columnIndex) {
                    case COLUMN_NAME: //Name
                        obj = selectedResult.getId();
                        break;

                    case COLUMN_DESCRIPTION: //decription
                        obj = selectedResult.getDescription();
                        break;

                    case COLUMN_IS_ACTIVE: //is this transformation to be included
                        checkBoxIsActive.setSelected(selectedResult.isSelected());
                        setCheckBoxText(checkBoxIsActive);
                        obj = checkBoxIsActive;
                        break;
                    default:
                        break;
                }
            }
        }

        return obj;
    }

    public boolean isCellEditable(int row, int col) {
        currentRow = row;
        return true;
    }

    public String getColumnName(int col) {
        return this.columnNames[col];
    }


    public void setValueAt(Object value, int row, int col) {
        currentRow = row;
        SelectedResult SelectedResult = resultSelections.get(row);
        if (SelectedResult!=null) {
            switch (col) {
                case COLUMN_NAME: //Name
                    SelectedResult.setId((String) value);
                    break;

                case COLUMN_DESCRIPTION: //decription
                    SelectedResult.setDescription((String) value);
                    break;
                case COLUMN_IS_ACTIVE: //check box
                    if (value instanceof JCheckBox)
                        SelectedResult.setSelected(((JCheckBox) value).isSelected());
                    else
                        SelectedResult.setSelected(value.equals(UIModel.ACTIVE));
            }
        }
        fireTableCellUpdated(row, col);
    }

    private void checkBox_actionListener(MouseEvent e) {
        JCheckBox checkBox = (JCheckBox) e.getSource();
        checkBox.setSelected(!checkBox.isSelected());
        SelectedResult SelectedResult = resultSelections.get(currentRow);
        SelectedResult.setSelected(checkBox.isSelected());
        setCheckBoxText(checkBox);
    }

    private void setCheckBoxText(JCheckBox checkBox) {
        if (checkBox.isSelected())
            checkBox.setText(UIModel.ACTIVE);
        else
            checkBox.setText(UIModel.NONACTIVE);
    }

    /*
     * JTable uses this method to determine the default renderer/
     * editor for each cell.
     */
    public Class getColumnClass(int c) {
        return getValueAt(0, c).getClass();
    }

    /**
     * add an empty row
     */
    public void addRow() {
        numRow++;
        SelectedResult SelectedResult = new SelectedResult("Enter Id here", "Enter Description here ", false) ;
        resultSelections.add(SelectedResult) ;
        fireTableRowsInserted(1, numRow);
    }

    /**
     * add an empty row
     * @param selectedRow integer
     */
    public void delRow(int selectedRow) {
        if (selectedRow>=0 ) {
            SelectedResult SelectedResult = resultSelections.get(selectedRow);
            resultSelections.remove(SelectedResult) ;
            numRow--;
            fireTableRowsDeleted(1, numRow);
        }
    }

}
