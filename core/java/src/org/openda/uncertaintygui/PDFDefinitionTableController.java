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
import org.openda.uncertainties.autocorrelationfunctions.AutoCorrelationFunction;
import org.openda.uncertainties.pdfs.*;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

public class PDFDefinitionTableController {

	private MainPanelController parentController;
	private PDFDefinitionTableModel pdfDefinitionTableModel;

	private PDFContext currentPdfContext;

    private final static int NUMBER_GRAPH_POINTS = 500;

	public PDFDefinitionTableController(MainPanelController parentController, UIModel dataModel) {

		this.parentController = parentController;
		this.pdfDefinitionTableModel = new PDFDefinitionTableModel(dataModel);

		//update table display (should be called each time that table model is changed).
		this.setupBasicTableProperties(parentController.getMainPanel().getPDFDefinitionTable());
		//setup table model listener (should be called each time that table model is changed).
		this.setupTableModelListener();

		//setup table listeners.
		this.setupTableListeners(parentController.getMainPanel().getPDFDefinitionTable());
    }

	public void populateTypeComboBox(JComboBox comboBox) {

        PDFContext normalContext = new PDFContext(new NormalDistribution());

//        comboBox.addItem(new PDFContext(new BetaDistribution()));
//        comboBox.addItem(new PDFContext(new CauchyDistribution()));
//        comboBox.addItem(new PDFContext(new ChiSquareDistribution()));
//        comboBox.addItem(new PDFContext(new ExponentialDistribution()));
//        comboBox.addItem(new PDFContext(new GammaDistribution()));
//        comboBox.addItem(new PDFContext(new GumbelMaximumDistribution()));
//        comboBox.addItem(new PDFContext(new GumbelMinimumDistribution()));
        comboBox.addItem(new PDFContext(new LognormalDistribution()));
        comboBox.addItem(new PDFContext(new NormalDistribution()));
//        comboBox.addItem(new PDFContext(new TriangularDistribution()));
//        comboBox.addItem(new PDFContext(new UniformDistribution()));
//        comboBox.addItem(new PDFContext(new WeibullDistribution()));

		comboBox.setSelectedItem(normalContext);
	}

	public void updatePDFDefinitionTable(UIModel uaUIModel) {
		//create new table model.
		this.pdfDefinitionTableModel = new PDFDefinitionTableModel(uaUIModel);

		//update table display (should be called each time that table model is changed).
		this.setupBasicTableProperties(parentController.getMainPanel().getPDFDefinitionTable());
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
		table.setModel(this.pdfDefinitionTableModel);

		initColumnSizes(table);
		for (int i = 0; i < table.getColumnCount(); i++) {
			TableColumn column = table.getColumnModel().getColumn(i);
			column.setCellRenderer(cellRenderer);
		}

		TableColumn columnName = table.getColumnModel().getColumn(PDFDefinitionTableModel.COLUMN_NAME);
		columnName.setCellEditor(new TextCellEditor());
		TableColumn columnBasicValue = table.getColumnModel().getColumn(PDFDefinitionTableModel.COLUMN_BASIC_VALUE);
		columnBasicValue.setCellEditor(new TextCellEditor());

    	setupPDFDefinitionComboBoxColumn();

		table.setColumnSelectionAllowed(false);
		table.setRowSelectionAllowed(true);

		//select first row.
		this.setSelectedRow(0);
	}

	public void setupTableListeners(JTable table) {
		TableCheckBoxMouseListener tableCheckBoxMouseListener = new TableCheckBoxMouseListener(table);
		table.addMouseListener(tableCheckBoxMouseListener);

		//add listener, which will respond when selected tablerow is changed.
		table.getSelectionModel().addListSelectionListener(new RowListener(table));
	}

	private void setupTableModelListener() {
		//add listener which responds to tableChanged that is fired
		//by pdfDefinitionTableModel.setValueAt().
        this.pdfDefinitionTableModel.addTableModelListener(new TableModelListener() {
        	public void tableChanged(TableModelEvent e) {
        		//update currentPDFContext.
        		PDF pdf = pdfDefinitionTableModel.getUncertaintiesObject().getPdf(pdfDefinitionTableModel.getCurrentRow());
       			setCurrentPDFContext(new PDFContext(pdf));

        		//update panel.
        		updatePDFDefinitionParameterAndGraphPanel();

        		//select row of currentPDFContext.
        		int selectedRowIndex = pdfDefinitionTableModel.getCurrentRow();
        		setSelectedRow(selectedRowIndex);
        	}
        });

        //add listener which responds to changes made in the checkboxes in the
        //pdf definition table.
		//the same checkbox is added to all rows, thus mouse listener
		//fires same events for each row. Method checkBox_actionListener
		//checks which row was clicked, using pdfDefinitionTableModel.currentRow.
        this.pdfDefinitionTableModel.getActiveColumnCheckBox().addMouseListener(new MouseListener() {
			public void mouseClicked(MouseEvent e) {
				checkBoxActionListener(e);
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

    }

	/**
	 * Updates active of uncertainties if checkboxes in table are clicked.
	 * This method uses currentRow to check which row was clicked, because
	 * this listener is added to all checkboxes in active column.
	 *
	 * This method will not work correctly if table rows are in different order
	 * than uncertainties in list in dataModel, but current getValueAt ensures
	 * that order is the same.
	 */
	private void checkBoxActionListener(MouseEvent e) {
		JCheckBox checkBox = (JCheckBox) e.getSource();
		checkBox.setSelected(!checkBox.isSelected());

		if (this.pdfDefinitionTableModel.getCurrentColumn() == PDFDefinitionTableModel.COLUMN_IS_ACTIVE) {
			PDF pdf = this.pdfDefinitionTableModel.getUncertaintiesObject().getPdf(this.pdfDefinitionTableModel.getCurrentRow());
			pdf.getUncertainItem().setActive(checkBox.isSelected());

			//if set to inactive.
			if (!pdf.getUncertainItem().isActive()) {
				//remove autocorrelation for deactivated pdf, if present.

				//get autocorrelation object with same Id as pdf.
				AutoCorrelationFunction autoCorrelationFunction =
					this.pdfDefinitionTableModel.getUncertaintiesObject()
							.getAutoCorrelationFunction(pdf.getUncertainItem().getId());
				//remove autoCorrelationFunction if present.
				if (autoCorrelationFunction != null) {
					this.pdfDefinitionTableModel.getUncertaintiesObject()
							.removeAutoCorrelationFunction(autoCorrelationFunction);
				}
			}

			//update only correlationTable (via parentController and correlationTableController).
	        this.parentController.getCorrelationTableController().updateCorrelationTable(this.parentController.getModel());
		}
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

			int columnIndex = PDFDefinitionTableModel.COLUMN_PDF_TYPE;
			if (columnIndex < 0) {
				return;
			}

			Object value = pdfDefinitionTableModel.getValueAt(selectRowNumber, columnIndex);
			if (value != null) {
    			currentPdfContext = (PDFContext) value;
	    		updatePDFDefinitionParameterAndGraphPanel();
    		}
		}
	}

	public void setupPDFDefinitionComboBoxColumn() {

		if (parentController.getMainPanel().getPDFDefinitionTable().getColumnModel() != null) {

			JComboBox comboBox = new JComboBox();
			JComboBox comboBoxRender = new JComboBox();
			this.populateTypeComboBox(comboBox);
			this.populateTypeComboBox(comboBoxRender);

            TableColumn column = parentController.getMainPanel()
						.getPDFDefinitionTable()
						.getColumnModel()
						.getColumn(
								PDFDefinitionTableModel.COLUMN_PDF_TYPE);

			column.setCellEditor(new DefaultCellEditor(comboBox));
			TableRenderer cellRenderer = new TableRenderer(comboBoxRender);
			column.setCellRenderer(cellRenderer);
		}
	}

	/**
	 * This method picks good column sizes. If all column headers are wider than
	 * the column's cells' contents, then just use column.sizeWidthToFit().
     * @param table JTable
     */
	public void initColumnSizes(JTable table) {

		TableColumn column;
		Component comp;
		int headerWidth;
		int cellWidth;

		TableCellRenderer headerRenderer = table.getTableHeader()
				.getDefaultRenderer();

		// TODO: move to tableModel. AK
		for (int i = 0; i < this.pdfDefinitionTableModel.getColumnCount(); i++) {
			column = table.getColumnModel().getColumn(i);

			comp = headerRenderer.getTableCellRendererComponent(null, column
					.getHeaderValue(), false, false, 0, 0);
			headerWidth = comp.getPreferredSize().width;

			// TODO: move to tableModel. AK
			comp = table.getDefaultRenderer(this.pdfDefinitionTableModel.getColumnClass(i))
					.getTableCellRendererComponent(table, this.pdfDefinitionTableModel.longValues[i], false,
							false, 0, i);
			cellWidth = comp.getPreferredSize().width;

			// XXX: Before Swing 1.1 Beta 2, use setMinWidth instead.
			column.setPreferredWidth(Math.max(headerWidth, cellWidth));
		}
	}

    /**
     * Update the PDFDefinition Parameters Table.
     */
	private void updatePDFDefinitionParametersTable() {
		// Populate the correlation model parameters table.
       	FunctionParameterTableModel parameterTableModel;
        if (this.currentPdfContext != null && this.currentPdfContext.getPdfObject() != null) {
        	parameterTableModel = new FunctionParameterTableModel(this.currentPdfContext);

        	parentController.getMainPanel().getPdfParameterAndGraphPanel().getParametersTable()
		        .setModel(parameterTableModel);

    		//setup cell editor.
        	TableColumn columnValue = parentController.getMainPanel().getPdfParameterAndGraphPanel()
        	    .getParametersTable().getColumnModel().getColumn(FunctionParameterTableModel.COLUMN_VALUE);
        	columnValue.setCellEditor(new TextCellEditor());

			parameterTableModel.addTableModelListener(new TableModelListener() {
				public void tableChanged(TableModelEvent e) {
					updatePDFDefinitionGraph();
				}
			});
        }
	}

    /**
     * Update the PDFDefinition Graph.
     */
	private void updatePDFDefinitionGraph() {
		GraphPlot plotPanel = parentController.getMainPanel().getPdfParameterAndGraphPanel().getGraphPlotPanel();

        //clear graph.
        plotPanel.clear(1);

        //plot graph.
        if (this.currentPdfContext != null && this.currentPdfContext.getPdfObject() != null) {
        	plotPanel.plotPoints(this.currentPdfContext.getGraphPoints(NUMBER_GRAPH_POINTS,
        			this.parentController.getCurrentPDFGraphType()), 1, true);
        }
	}

    /**
     * Update the PDF Definition parameter and graph Panel.
     */
	public void updatePDFDefinitionParameterAndGraphPanel() {
		updatePDFDefinitionGraph();

       	updatePDFDefinitionParametersTable();
    }

	public PDFDefinitionTableModel getPDFDefinitionTableModel() {
		return this.pdfDefinitionTableModel;
	}

	public void setCurrentPDFContext(PDFContext currentPDFContext) {
		this.currentPdfContext = currentPDFContext;
	}

	public PDFContext getCurrentPDFContext() {
		return this.currentPdfContext;
	}

	public int getSelectedRow() {
		return parentController.getMainPanel().getPDFDefinitionTable().getSelectedRow();
	}

	public void setSelectedRow(int rowIndex) {
		//select row in table.
		parentController.getMainPanel().getPDFDefinitionTable().getSelectionModel().setSelectionInterval(rowIndex, rowIndex);
    }

}
