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
package org.openda.application.gui;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.IVector;
import org.openda.interfaces.IInstance;
import org.openda.utils.plotting.Figure;
import org.openda.utils.Vector;

import javax.swing.*;
import java.awt.*;


public class PlotResultWriter implements IResultWriter {

	private Container displayPane;
	private JPanel frame;
	private String title = "";
	private String xlabel = "";
	private String ylabel = "";
	private String xunit ="";
	private String yunit ="";
	private String id;
	private Figure fig=null;
	private java.util.Vector<Double> stored = new java.util.Vector<Double>();
    private int defaultMaxSize = Integer.MAX_VALUE;

    public PlotResultWriter(JPanel figure, String id, String title, String xlabel, String xunit, String ylabel,String yunit) {
		this.displayPane = figure; //.getContentPane();
		this.frame = figure;
		this.title = title;
		this.xlabel = xlabel;
		this.xunit = xunit;
		this.ylabel = ylabel;
		this.yunit = yunit;
		this.id = id;
		fig = new Figure(this.displayPane);
        fig.setXlabel(xlabel,xunit);
        fig.setYlabel(ylabel,yunit);
        fig.setAxisTitle(title);
        // simple plot (opens new window or uses existing one)
        clean();
		this.displayPane.setVisible(true);
		frame.setVisible(true);

	}

    public void free(){};

    
	public void putMessage(Source source, String message) {
		if (message.startsWith("CostFunction = ")) {
			String costFunctionName = message.replace("CostFunction = ", "");
			String newTitle = costFunctionName;
			this.title = newTitle;
		}
	}

    
    public void putMessage(IInstance source, String message) {
		// do nothing
    }

//	
//	public void putValue(Source source, String id, Object result) {
//
//	}
//
//    
//    public void putValue(IInstance source, String id, Object result) {
//
//    }
//
//
//    
//	public void putValue(Source source, String id, Object result, int iteration) {
//        putValue("", id, result, iteration);
//    }

	
	public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration) {
		if (id.toLowerCase().contains(this.id)){
            double currentValue;
            if((this.id==null) || (id.equalsIgnoreCase(this.id))){
                if (result instanceof IVector){
                    currentValue = ((Vector) result).getValue(0);
                    this.stored.add(currentValue);
                } else if(result instanceof Double){
                    currentValue = (Double) result;
                    this.stored.add(currentValue);
                }
            }
            Figure fig1 = new Figure(this.displayPane);
            // simple plot (opens new window or uses existing one)
            int n = this.stored.size();
            if(n>1){
                double xVals[] = new double[n];
                double yVals[] = new double[n];
                for(int i=0;i<n;i++){
                    xVals[i] = i;
                    yVals[i] = this.stored.get(i);
                }

                fig1.addPlot(xVals,yVals,id);
                fig1.setXlabel(this.xlabel,this.xunit);
                fig1.setYlabel(this.ylabel,this.yunit);
                fig1.setAxisTitle(this.title);
                fig1.drawnow();
                fig1.wait(500);
                //this.displayPane.repaint();
                this.frame.repaint();
            }
		}
	}

//	
//    public void putValue(IInstance source, String id, Object result, int iteration) {
//        putValue("", id, result, iteration);
//    }


//    public void putValue(String source, String id, Object result, int iteration) {
//		double currentValue;
//		if((this.id==null) || (id.equalsIgnoreCase(this.id))){
//			if (result instanceof IVector){
//				currentValue = ((Vector) result).getValue(0);
//				this.stored.add(currentValue);
//			} else if(result instanceof Double){
//				currentValue = (Double) result;
//				this.stored.add(currentValue);
//			}
//		}
//		Figure fig1 = new Figure(this.displayPane);
//		// simple plot (opens new window or uses existing one)
//		int n = this.stored.size();
//		if(n>1){
//			double xVals[] = new double[n];
//			double yVals[] = new double[n];
//			for(int i=0;i<n;i++){
//				xVals[i] = i;
//				yVals[i] = this.stored.get(i);
//			}
//
//			fig1.addPlot(xVals,yVals,id);
//			fig1.setXlabel(this.xlabel,this.xunit);
//			fig1.setYlabel(this.ylabel,this.yunit);
//			fig1.setAxisTitle(this.title);
//			fig1.drawnow();
//			fig1.wait(500);
//			//this.displayPane.repaint();
//			this.frame.repaint();
//		}
//
//	}

    public void reset() {
    	this.stored = new java.util.Vector<Double>();
        clean();
    }

    private void clean() {
        double xVals[] = {0.0};
        double yVals[] = {0.0};
        fig.addPlot(xVals,yVals,"");
        fig.drawnow();
    }

    public void putIterationReport(IInstance source, int iteration, double cost, IVector parameters) {}

    
    public int getDefaultMaxSize() {
        return defaultMaxSize;
    }

    public Figure getFigure() {
        return fig;
    }
}



