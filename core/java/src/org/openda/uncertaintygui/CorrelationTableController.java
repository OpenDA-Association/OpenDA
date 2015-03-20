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

import org.openda.uncertaintygui.genericplot.GraphPlot;
import org.openda.uncertaintygui.tablemodels.*;
import org.openda.uncertainties.autocorrelationfunctions.*;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import java.awt.*;
import java.util.ArrayList;

/**
 * Controls the GUI for
 * the correlationTable and the correlationModelParameterAndGraph panel.
 */
public class CorrelationTableController {

	private MainPanelController parentController;
	private CorrelationTableModel correlationTableModel;

	private ArrayList<AutoCorrelationFunctionContext> autoCorrelationFunctionList;
	private AutoCorrelationFunctionContext defaultAutoCorrelationFunctionContext;
	private AutoCorrelationFunctionContext currentAutoCorrelationFunctionContext = null;

    private final static int NUMBER_GRAPH_POINTS = 500;

	public CorrelationTableController(MainPanelController parentController, UIModel auUIModel) {

		this.parentController = parentController;
		this.correlationTableModel = new CorrelationTableModel(auUIModel);
		this.populateModelList();

		//update table display (should be called each time that table model is changed).
		this.setupBasicTableProperties(this.parentController.getMainPanel().getAutoCorrelationTable());
		//setup table model listener (should be called each time that table model is changed).
		this.setupTableModelListener();

		//setup listeners.
		this.setupTableListeners(this.parentController.getMainPanel().getAutoCorrelationTable());
	}

	public void populateModelList() {

		this.autoCorrelationFunctionList = new ArrayList<AutoCorrelationFunctionContext>();

		//These autoCorrelationFunction objects do not have an uncertainItem, but this is not
		//a problem, since they are only used within a combobox and not for anything else.
		AutoCorrelationFunctionContext notAutoCorrelatedContext = new AutoCorrelationFunctionContext(null);
		AutoCorrelationFunctionContext circularContext = new AutoCorrelationFunctionContext(new CircularCorrelation());
		AutoCorrelationFunctionContext exponentialContext = new AutoCorrelationFunctionContext(new ExponentialCorrelation());
		AutoCorrelationFunctionContext gaussianContext = new AutoCorrelationFunctionContext(new GaussianCorrelation());
		AutoCorrelationFunctionContext linearContext = new AutoCorrelationFunctionContext(new LinearCorrelation());
		AutoCorrelationFunctionContext nuggetContext = new AutoCorrelationFunctionContext(new NuggetCorrelation());
		AutoCorrelationFunctionContext pentasphericalContext = new AutoCorrelationFunctionContext(new PentasphericalCorrelation());
		AutoCorrelationFunctionContext periodicContext = new AutoCorrelationFunctionContext(new PeriodicCorrelation());
		AutoCorrelationFunctionContext sphericalContext = new AutoCorrelationFunctionContext(new SphericalCorrelation());
		this.autoCorrelationFunctionList.add(notAutoCorrelatedContext);
		this.autoCorrelationFunctionList.add(circularContext);
		this.autoCorrelationFunctionList.add(exponentialContext);
		this.autoCorrelationFunctionList.add(gaussianContext);
		this.autoCorrelationFunctionList.add(linearContext);
		this.autoCorrelationFunctionList.add(nuggetContext);
		this.autoCorrelationFunctionList.add(pentasphericalContext);
		this.autoCorrelationFunctionList.add(periodicContext);
		this.autoCorrelationFunctionList.add(sphericalContext);

		this.defaultAutoCorrelationFunctionContext = notAutoCorrelatedContext;
	}

	public void populateTypeComboBox(JComboBox comboBox) {

		ArrayList<AutoCorrelationFunctionContext> typeList;

		typeList = this.autoCorrelationFunctionList;
		for (AutoCorrelationFunctionContext context : typeList) {
			comboBox.addItem(context);
		}

		comboBox.setSelectedItem(this.defaultAutoCorrelationFunctionContext);
	}

	public void updateCorrelationTable(UIModel dataModel) {
		//create new table model.
		this.correlationTableModel = new CorrelationTableModel(dataModel);

		//update table display (should be called each time that table model is changed).
		this.setupBasicTableProperties(this.parentController.getMainPanel().getAutoCorrelationTable());
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
		table.setModel(this.correlationTableModel);

		initColumnSizes(table);
		for (int i = 0; i < table.getColumnCount(); i++) {
			TableColumn column = table.getColumnModel().getColumn(i);
			column.setCellRenderer(cellRenderer);
		}

		TableColumn columnName = table.getColumnModel().getColumn(CorrelationTableModel.COLUMN_NAME);
		columnName.setCellEditor(new TextCellEditor());

    	setupCorrelationComboBoxColumn();

    	this.correlationTableModel.fireTableDataChanged();

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
		//by correlationTableModel.setValueAt().
        this.correlationTableModel.addTableModelListener(new TableModelListener() {
        	public void tableChanged(TableModelEvent e) {
        		//(re)select row after setValueAt was called.

        		//select row of currentCorrelationContext.
        		int selectedRowIndex = correlationTableModel.getCurrentRow();
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
    		//update currentCorrelationContext after row was selected.

			if (event.getValueIsAdjusting()) {
				return;
			}

			int selectRowNumber = table.getSelectedRow();
			if (selectRowNumber < 0) {
				return;
			}

			int columnIndex = CorrelationTableModel.COLUMN_CORRELATION_TYPE;

			Object value = correlationTableModel.getValueAt(selectRowNumber, columnIndex);
			if (value != null) {
				currentAutoCorrelationFunctionContext = (AutoCorrelationFunctionContext) value;
				updateCorrelationParameterAndGraphPanel();
    		}
		}
	}

	public void setupCorrelationComboBoxColumn() {

		if (this.parentController.getMainPanel().getAutoCorrelationTable().getColumnModel() != null) {

			TableColumn column;
			JComboBox comboBox = new JComboBox();
			JComboBox comboBoxRender = new JComboBox();
			this.populateTypeComboBox(comboBox);
			this.populateTypeComboBox(comboBoxRender);

			column = this.parentController.getMainPanel()
						.getAutoCorrelationTable()
						.getColumnModel()
						.getColumn(
								CorrelationTableModel.COLUMN_CORRELATION_TYPE);

			column.setCellEditor(new DefaultCellEditor(comboBox));
			TableRenderer cellRenderer = new TableRenderer(comboBoxRender);
			column.setCellRenderer(cellRenderer);
		}
	}

	/**
	 * This method picks good column sizes. If all column headers are wider than
	 * the column's cells' contents, then just use column.sizeWidthToFit().
     * @param table The correlation table
     */
	public void initColumnSizes(JTable table) {

		TableColumn column;
		Component comp;
		int headerWidth;
		int cellWidth;

		TableCellRenderer headerRenderer = table.getTableHeader()
				.getDefaultRenderer();

		// TODO: move to tableModel. AK
		for (int i = 0; i < this.correlationTableModel.getColumnCount(); i++) {
			column = table.getColumnModel().getColumn(i);

			comp = headerRenderer.getTableCellRendererComponent(null, column
					.getHeaderValue(), false, false, 0, 0);
			headerWidth = comp.getPreferredSize().width;

			// /TODO: move to tableModel. AK
			comp = table.getDefaultRenderer(this.correlationTableModel.getColumnClass(i))
					.getTableCellRendererComponent(table, this.correlationTableModel.longValues[i], false,
							false, 0, i);
			cellWidth = comp.getPreferredSize().width;

			// XXX: Before Swing 1.1 Beta 2, use setMinWidth instead.
			column.setPreferredWidth(Math.max(headerWidth, cellWidth));
		}
	}

    /**
     * Update the Correlation model Parameters Table.
     */
	private void updateCorrelationModelParametersTable() {
		// Populate the correlation model parameters table.
		FunctionParameterTableModel parameterTableModel = new FunctionParameterTableModel(this.currentAutoCorrelationFunctionContext);

		this.parentController.getMainPanel().getAutoCorrelationParameterAndGraphPanel().getParametersTable()
		        .setModel(parameterTableModel);

		//setup cell editor.
    	TableColumn columnValue = parentController.getMainPanel().getAutoCorrelationParameterAndGraphPanel()
    	    .getParametersTable().getColumnModel().getColumn(FunctionParameterTableModel.COLUMN_VALUE);
    	columnValue.setCellEditor(new TextCellEditor());

		parameterTableModel.addTableModelListener(new TableModelListener() {
			public void tableChanged(TableModelEvent e) {
				updateCorrelationModelGraph();
			}
		});
	}

    /**
     * Update the Correlation model Graph.
     */
	private void updateCorrelationModelGraph() {
        //populate the graph.
        GraphPlot plotPanel = this.parentController.getMainPanel().getAutoCorrelationParameterAndGraphPanel().getGraphPlotPanel();
        plotPanel.clear(1);

        if (currentAutoCorrelationFunctionContext != null) {
        	//if no correlation set, then do not draw graph.
        	if (currentAutoCorrelationFunctionContext.getAutoCorrelationFunctionObject() != null) {
            	plotPanel.plotPoints(this.currentAutoCorrelationFunctionContext.getGraphPoints(NUMBER_GRAPH_POINTS), 1, true);
        	}
        }
	}

    /**
     * Update the AutoCorrelation parameter and graph Panel.
     */
	public void updateCorrelationParameterAndGraphPanel() {
    	updateCorrelationModelGraph();

       	updateCorrelationModelParametersTable();
    }

    public CorrelationTableModel getCorrelationTableModel() {
		return this.correlationTableModel;
	}

	public void setCurrentAutoCorrelationFunctionContext(AutoCorrelationFunctionContext currentAutoCorrelationFunctionContext) {
		this.currentAutoCorrelationFunctionContext = currentAutoCorrelationFunctionContext;
	}

	public AutoCorrelationFunctionContext getCurrentAutoCorrelationFunctionContext() {
		return currentAutoCorrelationFunctionContext;
	}

	public int getSelectedRow() {
		return this.parentController.getMainPanel().getAutoCorrelationTable().getSelectedRow();
	}

	public void setSelectedRow(int rowIndex) {
		//select row in table.
		this.parentController.getMainPanel().getAutoCorrelationTable().getSelectionModel().setSelectionInterval(rowIndex, rowIndex);
    }
}
