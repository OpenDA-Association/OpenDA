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

import java.awt.Component;
import java.util.ArrayList;

import javax.swing.DefaultCellEditor;
import javax.swing.JComboBox;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import org.openda.uncertaintygui.tablemodels.*;
import org.openda.uncertainties.variationfunctions.*;

public class VariationPerParameterTableController {

	private MainPanelController parentController;
	private VariationPerParameterTableModel variationPerParameterTableModel;

	private ArrayList<VariationFunctionContext> variationFunctionContextList;
	private VariationFunctionContext defaultVariationFunctionContext;
	private VariationFunctionContext currentVariationFunctionContext = null;

	public VariationPerParameterTableController(MainPanelController parentController, UIModel uiModel) {

		this.parentController = parentController;
		this.variationPerParameterTableModel = new VariationPerParameterTableModel(uiModel); // empty
		this.populateModelList();

		//update table display (should be called each time that table model is changed).
		this.setupBasicTableProperties(parentController.getMainPanel().getVariationPerParameterTable());
		//setup table model listener (should be called each time that table model is changed).
		this.setupTableModelListener();

		//setup listeners.
		this.setupTableListeners(parentController.getMainPanel().getVariationPerParameterTable());
	}

	public void populateModelList() {

		//These variationFunction objects do not have an uncertainItem, but this is not
		//a problem, since they are only used within a combobox and not for anything else.
		this.variationFunctionContextList = new ArrayList<VariationFunctionContext>();
		VariationFunctionContext rangeVariationContext = new VariationFunctionContext(new RangeVariation());
		VariationFunctionContext percentageVariationContext = new VariationFunctionContext(new PercentageVariation());
		this.variationFunctionContextList.add(rangeVariationContext);
		this.variationFunctionContextList.add(percentageVariationContext);

		this.defaultVariationFunctionContext = rangeVariationContext;
	}

	public void populateTypeComboBox(JComboBox comboBox) {

		ArrayList<VariationFunctionContext> typeList;

		typeList = this.variationFunctionContextList;
		for (VariationFunctionContext context : typeList) {
			comboBox.addItem(context);
		}

		comboBox.setSelectedItem(this.defaultVariationFunctionContext);
	}

	public void updateVariationPerParameterTable(UIModel dataModel) {
		//create new table model.
		this.variationPerParameterTableModel = new VariationPerParameterTableModel(dataModel);

		//update table display (should be called each time that table model is changed).
		this.setupBasicTableProperties(parentController.getMainPanel().getVariationPerParameterTable());
		//setup table model listener (should be called each time that table model is changed).
		this.setupTableModelListener();
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
		table.setModel(this.variationPerParameterTableModel);

		initColumnSizes(table);
		for (int i = 0; i < table.getColumnCount(); i++) {
			TableColumn column = table.getColumnModel().getColumn(i);
			column.setCellRenderer(cellRenderer);
		}

		TableColumn columnName = table.getColumnModel().getColumn(VariationPerParameterTableModel.COLUMN_NAME);
		columnName.setCellEditor(new TextCellEditor());
		TableColumn columnBasicValue = table.getColumnModel().getColumn(VariationPerParameterTableModel.COLUMN_BASIC_VALUE);
		columnBasicValue.setCellEditor(new TextCellEditor());

    	setupVariationFunctionComboBoxColumn();

    	this.variationPerParameterTableModel.fireTableDataChanged();

		table.setColumnSelectionAllowed(false);
		table.setRowSelectionAllowed(true);

		//select first row.
		table.getSelectionModel().setSelectionInterval(0, 0);
	}

	public void setupTableListeners(JTable table) {
		TableCheckBoxMouseListener tableCheckBoxMouseListener = new TableCheckBoxMouseListener(table);
		table.addMouseListener(tableCheckBoxMouseListener);

		table.getSelectionModel().addListSelectionListener(new RowListener(table));
	}

	private void setupTableModelListener() {
		//add listener which responds to tableChanged that is fired
		//by variationPerParameterTableModel.setValueAt().
        this.variationPerParameterTableModel.addTableModelListener(new TableModelListener() {
        	public void tableChanged(TableModelEvent e) {
        		//update currentVariationFunctionContext.
        		Variation variationFunction = variationPerParameterTableModel.getUncertaintiesObject()
        			.getVariationFunction(variationPerParameterTableModel.getCurrentRow());
        		setCurrentVariationFunctionContext(new VariationFunctionContext(variationFunction));

        		//update panel.
        		updateVariationPerParameterSidePanel();

        		//select row of currentVariationFunctionContext.
        		int selectedRowIndex = variationPerParameterTableModel.getCurrentRow();
        		setSelectedRow(selectedRowIndex);
        	}
        });
    }

	private class RowListener implements ListSelectionListener {

		private JTable table;

		public RowListener(JTable table) {
			this.table = table;
		}

		public void valueChanged(ListSelectionEvent event) {

			if (event.getValueIsAdjusting()) {
				return;
			}

			int selectRowNumber = table.getSelectedRow();
			if (selectRowNumber < 0) {
				return;
			}

			int columnIndex = VariationPerParameterTableModel.COLUMN_VARIATION_TYPE;

			Object value = variationPerParameterTableModel.getValueAt(selectRowNumber, columnIndex);
			if (value != null) {
				currentVariationFunctionContext = (VariationFunctionContext) value;
    			updateVariationPerParameterSidePanel();
    		}

		}
	}

	public void setupVariationFunctionComboBoxColumn() {

		if (parentController.getMainPanel().getVariationPerParameterTable().getColumnModel() != null) {

			JComboBox comboBox = new JComboBox();
			JComboBox comboBoxRender = new JComboBox();
			this.populateTypeComboBox(comboBox);
			this.populateTypeComboBox(comboBoxRender);

            TableColumn column = parentController.getMainPanel()
						.getVariationPerParameterTable()
						.getColumnModel()
						.getColumn(
								VariationPerParameterTableModel.COLUMN_VARIATION_TYPE);

			column.setCellEditor(new DefaultCellEditor(comboBox));
			TableRenderer cellRenderer = new TableRenderer(comboBoxRender);
			column.setCellRenderer(cellRenderer);
		}
	}

	/**
	 * This method picks good column sizes. If all column headers are wider than
	 * the column's cells' contents, then just use column.sizeWidthToFit().
     * @param table  JTable
     */
	public void initColumnSizes(JTable table) {

		TableColumn column;
		Component comp;
		int headerWidth;
		int cellWidth;

		TableCellRenderer headerRenderer = table.getTableHeader()
				.getDefaultRenderer();

		// TODO: move to tableModel. AK
		for (int i = 0; i < this.variationPerParameterTableModel.getColumnCount(); i++) {
			column = table.getColumnModel().getColumn(i);

			comp = headerRenderer.getTableCellRendererComponent(null, column
					.getHeaderValue(), false, false, 0, 0);
			headerWidth = comp.getPreferredSize().width;

			// /TODO: move to tableModel. AK
			comp = table.getDefaultRenderer(this.variationPerParameterTableModel.getColumnClass(i))
					.getTableCellRendererComponent(table, this.variationPerParameterTableModel.longValues[i], false,
							false, 0, i);
			cellWidth = comp.getPreferredSize().width;

			// XXX: Before Swing 1.1 Beta 2, use setMinWidth instead.
			column.setPreferredWidth(Math.max(headerWidth, cellWidth));
		}
	}

    /**
     * Update the PDFDefinition Parameters Table.
     */
	private void updateVariationFunctionParametersTable() {
		// Populate the correlation model parameters table.
		FunctionParameterTableModel parameterTableModel = new FunctionParameterTableModel(this.currentVariationFunctionContext);

		parentController.getMainPanel().getvariationPerParameterSidePanel().getParametersTable()
		        .setModel(parameterTableModel);

		//setup cell editor.
    	TableColumn columnValue = parentController.getMainPanel().getvariationPerParameterSidePanel()
    	    .getParametersTable().getColumnModel().getColumn(FunctionParameterTableModel.COLUMN_VALUE);
    	columnValue.setCellEditor(new TextCellEditor());
	}

    /**
     * Update the VariationPerParameter panel for changing parameters
     * of a selected variation model.
     */
	public void updateVariationPerParameterSidePanel() {
       	updateVariationFunctionParametersTable();
    }

	public VariationPerParameterTableModel getVariationPerParameterTableModel() {
		return this.variationPerParameterTableModel;
	}

	public void setCurrentVariationFunctionContext(
			VariationFunctionContext currentVariationFunctionContext) {
		this.currentVariationFunctionContext = currentVariationFunctionContext;
	}

	public VariationFunctionContext getCurrentVariationFunctionContext() {
		return currentVariationFunctionContext;
	}

	public int getSelectedRow() {
		return parentController.getMainPanel().getVariationPerParameterTable().getSelectedRow();
	}

	public void setSelectedRow(int rowIndex) {
		//select row in table.
		parentController.getMainPanel().getVariationPerParameterTable().getSelectionModel().setSelectionInterval(rowIndex, rowIndex);
    }
}
