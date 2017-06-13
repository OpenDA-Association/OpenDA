/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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
package org.openda.application.gui;

import org.openda.interfaces.IInstance;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;
import org.openda.utils.InstanceStore;
import org.openda.utils.PrintNumber;
import org.openda.utils.TreeVector;
import org.openda.utils.plotting.Figure;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;


public class OutputGui extends JPanel implements IResultWriter {
    JTable table;
    OutputTableModel tableModel = null;
	int maxNoCols = 100;
    JFrame plotWindow;
    Figure figure;
    private int defaultMaxSize = Integer.MAX_VALUE;

    public OutputGui(){

        super(new GridLayout(1, 0));

        tableModel = new OutputTableModel();
        table = new JTable(tableModel);
        table.setPreferredScrollableViewportSize(new Dimension(500, 70));
        table.setFillsViewportHeight(true);
        table.addMouseListener(new HandleColumnSelection());
        table.setColumnSelectionAllowed(true);
        table.setRowSelectionAllowed(false);

        //Create the scroll pane and add the table to it.
        JScrollPane scrollPane = new JScrollPane(table);

        //Add the scroll pane to this panel.
        add(scrollPane);
    }

    public void appendData( int iteration, double cost, IVector vector) {
        tableModel.appendData(iteration, cost, vector);
    }

     public void free(){};

    public void reset() {
        tableModel.reset();
    }

    public void fillTableFromFile() {
        String[] instances = InstanceStore.getInstances();
        for (String line: instances) {
            String[] piece = line.split("[ :;]+");

            int numberParameters = (piece.length - 4) / 2;
            int iteration;
            double cost;
            String[] name = new String[numberParameters];
            double[] value = new double[numberParameters];

            iteration = Integer.valueOf(piece[1]);
            try {
                cost = Double.valueOf(piece[3]);
            } catch (NumberFormatException ne) {
                cost = Double.NaN;
            }
            int k = 0;
            for (int i = 4; i < piece.length; i += 2) {
                name[k] = piece[i];
                value[k] = Double.valueOf(piece[i+1]);
                k ++;
            }
            TreeVector vector = new TreeVector("Parameters", name, value);
            appendData(iteration, cost, vector);
        }
    }

    
    public void putMessage(Source source, String message) {}

    
    public void putMessage(IInstance source, String message) {}


	
	public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration) {}

    public void putIterationReport(IInstance source, int iteration, double cost, IVector parameters) {
        appendData(iteration, cost, parameters);
    }

    
    public int getDefaultMaxSize() {
        return defaultMaxSize;
    }

    class OutputTableModel extends AbstractTableModel {
        private String[] columnNames = {"Iteration",
                "Cost"};
        private Object[][] data = new Object[0][0];

        public int getColumnCount() {
            return columnNames.length;
        }

        public int getRowCount() {
            return data.length;
        }

        public String getColumnName(int col) {
            return columnNames[col];
        }

        public Object getValueAt(int row, int col) {
            if (col == 0) {
                if (((Integer)data[row][col]).intValue() > 0) {
                    return data[row][col].toString();
                } else {
                    return "Optimum";
                }
            } else {
                if (((Integer) data[row][0]).intValue() > 0) {
                    return PrintNumber.printNumber(((Double) data[row][col]).doubleValue());
                } else {
                    return PrintNumber.printNumberExtended(((Double) data[row][col]).doubleValue());
                }
            }
        }

        /*
         * Don't need to implement this method unless your table's
         * editable.
         */
        public boolean isCellEditable(int row, int col) {
            return false;
        }

        public void appendData(int iteration, double cost, IVector vector) {
            if(data.length==0) {
               fillColumns(vector);
            }
            Object[] row = getData(vector);
            row[0] = new Integer(iteration);
            row[1] = new Double(cost);

            appendRow(row);
        }

        public void appendRow(Object[] obj) {
            Object[][] newData = new Object[data.length + 1][columnNames.length];
            for (int row = 0; row < data.length; row++) {
                for (int col = 0; col < columnNames.length; col++) {
                    newData[row][col] = data[row][col];
                }
            }
            int newRow = data.length;
            for (int col = 0; col < columnNames.length; col++) {
                newData[newRow][col] = obj[col];
            }
            data = newData;

            fireTableRowsInserted(data.length, data.length);
            table.repaint();
        }

        public Object[] getData(IVector vector) {
        	double[] rawData = vector.getValues();
        	Object[] data = new Object[this.columnNames.length];
        	for( int i = 0; i < (this.columnNames.length-2); i++) {
        		data[2+i] = new Double(rawData[i]); //TODO fails for larger treevectors eg. v.part1 = [1 2]
        	}
        	return data;
        }

        private void fillColumns(IVector vector) {

			ArrayList<String> parNames = null;
			if (vector instanceof ITreeVector) {
                ITreeVector treeVector = (ITreeVector) vector;
				parNames = treeVector.getSubTreeVectorIds();
			}else{
				parNames = new ArrayList<String>();
            	for(int i=0;i<vector.getSize();i++){
					parNames.add("value["+i+"]");
            	}
            }

			int fixedColumnCount = 2;
			String[] names = new String[Math.min(parNames.size() + fixedColumnCount, maxNoCols)];
			names[0] = columnNames[0];
			names[1] = columnNames[1];
			for (int i = 0; i < parNames.size() && i+fixedColumnCount<maxNoCols; i++) {
				names[i+fixedColumnCount] = parNames.get(i);
			}
			columnNames = names;
			fireTableStructureChanged();
        }

        public void reset() {
            int rows = data.length;
            data = new Object[0][0];
            fireTableRowsDeleted(0, rows);
        }

        public void plotColumn(int selectedColumn) {
            if (selectedColumn > 0) {
                if (plotWindow == null) {
                    plotWindow = new JFrame();
                    plotWindow.setSize(600,400);
                    //plotWindow.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                    plotWindow.setLayout(new BorderLayout());
                    figure = new Figure(plotWindow.getContentPane());
                    if(selectedColumn==1){ //Cost
                    	figure.fixYmin=true;
                    }else{
                        figure.fixYmin=false;
                    }
                    plotWindow.addWindowListener(new HandleWindowEvents());
                    plotWindow.setTitle("Calibration parameter");
                }
                plotWindow.setAlwaysOnTop(true);
                plotWindow.setAlwaysOnTop(false);

                double[] xData = new double[data.length];
                double[] yData = new double[data.length];
                for (int row = 0; row < data.length; row ++) {
                    xData[row] = row;
                    yData[row] = ((Double) data[row][selectedColumn]).doubleValue();
                }
                figure.plot(xData, yData,columnNames[selectedColumn]);

                figure.setAxisTitle(columnNames[selectedColumn]);
                figure.setXlabel("iteration","index of model run");
                figure.setYlabel(columnNames[selectedColumn],"");

                figure.drawnow();
                plotWindow.setVisible(true);
            }
        }
    }

    class HandleColumnSelection implements MouseListener {

        public void mouseClicked(MouseEvent e) {
            tableModel.plotColumn(table.getSelectedColumn());
        }

        public void mousePressed(MouseEvent e) {
            // Nothing to do
        }

        public void mouseReleased(MouseEvent e) {
            // Nothing to do
        }

        public void mouseEntered(MouseEvent e) {
            // Nothing to do
        }

        public void mouseExited(MouseEvent e) {
            // Nothing to do
        }
    }

    class HandleWindowEvents extends WindowAdapter {
        public void windowClosing(WindowEvent e) {
            plotWindow = null;
        }

        public void windowClosed(WindowEvent e) {
            plotWindow = null;
        }
        }
}
