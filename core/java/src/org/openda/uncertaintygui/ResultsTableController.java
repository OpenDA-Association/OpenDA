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

import org.openda.uncertaintygui.tablemodels.*;

import javax.swing.*;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import java.awt.*;
import java.util.ArrayList;

/**
 * Controls the GUI for
 * the correlationTable and the correlationModelParameterAndGraph panel.
 */
public class ResultsTableController {

	private MainPanelController parentController;
	private ResultSelectionModel resultsTableModel;

	public ResultsTableController(MainPanelController parentController, UIModel uiModel) {

		this.parentController = parentController;
        if (uiModel.getUaToolsGuiConfiguration().getResultSelectionList() != null) {
		    this.resultsTableModel = new ResultSelectionModel(uiModel.getUaToolsGuiConfiguration().getResultSelectionList());
        } else {
            this.resultsTableModel = new ResultSelectionModel(new ArrayList<SelectedResult>(0));
        }

		//update table display.
		this.setupBasicTableProperties(parentController.getMainPanel().getResultsTable());

		//setup listeners.
		this.setupTableListeners(parentController.getMainPanel().getResultsTable());
	}

	public void updateResultsTable(UIModel dataModel) {
		//create new table model.
		this.resultsTableModel = new ResultSelectionModel(dataModel.getUaToolsGuiConfiguration().getResultSelectionList());

		//update table display.
		this.setupBasicTableProperties(parentController.getMainPanel().getResultsTable());
	}

	/**
	 * setting up the table.
	 *
	 * @param table JTable to store this.correlationTableModel
	 */
	private void setupBasicTableProperties(JTable table) {

		table.clearSelection();

		table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);


		TableRenderer cellRenderer = new TableRenderer(new JTextField());

		table.setRowSelectionAllowed(true);
		table.setColumnSelectionAllowed(false);
		table.setVisible(true);
		table.setModel(this.resultsTableModel);

		initColumnSizes(table);
		for (int i = 0; i < table.getColumnCount(); i++) {
			TableColumn column = table.getColumnModel().getColumn(i);
			column.setCellRenderer(cellRenderer);
		}

		TableColumn columnName = table.getColumnModel().getColumn(ResultSelectionModel.COLUMN_NAME);
		columnName.setCellEditor(new TextCellEditor());
		TableColumn columnDescription = table.getColumnModel().getColumn(ResultSelectionModel.COLUMN_DESCRIPTION);
		columnDescription.setCellEditor(new TextCellEditor());

    	this.resultsTableModel.fireTableDataChanged();

		table.setColumnSelectionAllowed(false);
		table.setRowSelectionAllowed(true);

		//select first row.
		table.getSelectionModel().setSelectionInterval(0, 0);
	}

	public void setupTableListeners(JTable table) {
		TableCheckBoxMouseListener tableCheckBoxMouseListener = new TableCheckBoxMouseListener(table);
		table.addMouseListener(tableCheckBoxMouseListener);
	}

	/**
	 * This method picks good column sizes. If all column headers are wider than
	 * the column's cells' contents, then just use column.sizeWidthToFit().
     * @param table Selected Results Table
     */
	public void initColumnSizes(JTable table) {

		TableColumn column;
		Component comp;
		int headerWidth;
		int cellWidth;

		TableCellRenderer headerRenderer = table.getTableHeader()
				.getDefaultRenderer();

		// TODO: move to tableModel. AK
		for (int i = 0; i < this.resultsTableModel.getColumnCount(); i++) {
			column = table.getColumnModel().getColumn(i);

			comp = headerRenderer.getTableCellRendererComponent(null, column
					.getHeaderValue(), false, false, 0, 0);
			headerWidth = comp.getPreferredSize().width;

			// /TODO: move to tableModel. AK
			comp = table.getDefaultRenderer(this.resultsTableModel.getColumnClass(i))
					.getTableCellRendererComponent(table, this.resultsTableModel.longValues[i], false,
							false, 0, i);
			cellWidth = comp.getPreferredSize().width;

			// XXX: Before Swing 1.1 Beta 2, use setMinWidth instead.
			column.setPreferredWidth(Math.max(headerWidth, cellWidth));
		}
	}
}
