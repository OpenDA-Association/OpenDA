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
package org.openda.utils.plotting;
import gov.noaa.pmel.sgt.LineAttribute;
import gov.noaa.pmel.sgt.PointAttribute;
import gov.noaa.pmel.sgt.dm.PointCollection;
import gov.noaa.pmel.sgt.dm.SGTMetaData;
import gov.noaa.pmel.sgt.dm.SimpleGrid;
import gov.noaa.pmel.sgt.dm.SimpleLine;
import gov.noaa.pmel.sgt.dm.SimplePoint;
import gov.noaa.pmel.sgt.swing.JPlotLayout;
import gov.noaa.pmel.util.SoTDomain;
import gov.noaa.pmel.util.SoTValue;
import gov.noaa.pmel.util.SoTRange;
import javax.swing.*;
import java.awt.*;
import java.beans.PropertyVetoException;
import java.util.Vector;

public class Figure{
	private JFrame figure = null;    //the window if it was constructed
	private Container paper = null;  //this is where we put the axis in, e.g figure.getContentPane()
	private JPlotLayout axis = null; // area for one plot
	// curves
	private Vector<double[]> xLineValues = new Vector<double[]>();
	private Vector<double[]> yLineValues = new Vector<double[]>();
	private Vector<String> lineLabels = new Vector<String>();
	// grids
	private Vector<double[]> xGridValues = new Vector<double[]>(); 
	private Vector<double[]> yGridValues = new Vector<double[]>(); 
	private Vector<double[]> zGridValues = new Vector<double[]>();
	private Vector<Integer>     plotType = new Vector<Integer>();
	private static int pcolorType  = 1;
	private static int contourType = 2;
	Color defaultColors[] = null;
	
	String xLabel = "";
	String yLabel = "";
	String xUnit = "";
	String yUnit = "";
	String axisTitle = "";
    public boolean fixYmin = true;
	
    /**
     * Construct a figure in a new JFrame. Use Figure(Container paper) for embedding in your own
     * gui.
     * @param title as displayed at top of window
     */
    public Figure(String title) {
		this.figure = new JFrame();
		this.figure.setTitle(title);
		this.figure.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE); //or EXIT_ON_CLOSE;
		this.figure.setSize(new Dimension(600,600));
		this.paper = this.figure.getContentPane();
		this.paper.setLayout(new BorderLayout());
		this.defaultColors = new Color[5];
		defaultColors[0] = Color.BLACK;
		defaultColors[0] = Color.BLUE;
		defaultColors[0] = Color.GREEN;
		defaultColors[0] = Color.RED;
		defaultColors[0] = Color.CYAN;
	}

    /**
     * Create a new figure inside an existing GUI.
     * @param paper container for plot typically JPanel or JFrame.getContentPane();
     */
	public Figure(Container paper){
		this.paper = paper;
		this.figure = null;
		this.paper.setLayout(new BorderLayout());
		this.defaultColors = new Color[5];
		defaultColors[0] = Color.BLACK;
		defaultColors[0] = Color.BLUE;
		defaultColors[0] = Color.GREEN;
		defaultColors[0] = Color.RED;
		defaultColors[0] = Color.CYAN;
	}

	/**
	 * Get the current container used for plotting. Eg. if you want to manipulate this
	 * manually.
	 * @return panel
	 */
	public Container getPanel() {
        return paper;
    }

	/**
	 * Clear any existing graph and add a new line plot.
	 * @param xVals
	 * @param yVals
	 * @param label
	 */
	public void plot(double[] xVals, double[] yVals, String label){
		// clear the figure
		this.xLineValues = new Vector<double[]>();
		this.yLineValues = new Vector<double[]>();
		this.lineLabels  = new Vector<String>();
		if(xVals.length!=yVals.length){
			throw new RuntimeException("Attempt to plot vectors of unequal length.");
		}
		if(xVals.length>1){
			this.xLineValues.add(xVals);
			this.yLineValues.add(yVals);
		}
		this.lineLabels.add(label);
	}

	/**
	 * Add a new line plot. Existing lines are kept.
	 * @param xVals
	 * @param yVals
	 * @param label
	 */
	public void addPlot(double[] xVals, double[] yVals, String label){
		if(xVals.length!=yVals.length){
			throw new RuntimeException("Attempt to plot vectors of unequal length.");
		}
		if(xVals.length>1){
			this.xLineValues.add(xVals);
			this.yLineValues.add(yVals);
		}
		this.lineLabels.add(label);
	}

	/**
	 * Add a color plot.
	 * @param xVals
	 * @param yVals
	 * @param zVals
	 */
	public void addPcolor(double[] xVals, double[] yVals, double[] zVals){
		this.xGridValues.add(xVals);
		this.yGridValues.add(yVals);
		this.zGridValues.add(zVals);
		this.plotType.add(Figure.pcolorType);
	}

	
	public void setAxisTitle(String title){
		this.axisTitle = title;
	}

	public void setXlabel(String label, String unit){
		this.xLabel = label;
		this.xUnit = unit;
	}

	public void setYlabel(String label, String unit){
		this.yLabel = label;
		this.yUnit  = unit;
	}
	
	/**
	 * Draw the plot after all elements are added and after options are set.
	 */
	public void drawnow(){
		if(this.xLineValues.size()>0){
			this.axis = new JPlotLayout(false,false,false,"Figure",null,false);
            this.axis.setSize(this.paper.getSize());
			this.paper.removeAll();
			this.paper.add(axis, BorderLayout.CENTER);

			this.axis.setBatch(true);
			/*
			 * Set the titles.
			 */
			this.axis.setTitles(this.axisTitle,"","");
			/*
			 * Change the title sizes from the defaults.  (0.25, 0.15)
			 */
			this.axis.setTitleHeightP(0.2, 0.2);

			// draw curves
			SimpleLine line   = null;
			SimplePoint point = null;
			SGTMetaData xMeta = null;
			SGTMetaData yMeta = null;
            double ymaxAll = Double.NEGATIVE_INFINITY; // track maxy in case we set ymin manually
            
            // plot lines
			for(int i=0;i<this.xLineValues.size();i++){
				double xvals[] = this.xLineValues.get(i);
				double yvals[] = this.yLineValues.get(i);
				if(xvals.length>1){
					line = new SimpleLine(xvals,yvals,this.lineLabels.get(i));
					xMeta = new SGTMetaData(this.xLabel, this.xUnit);
					yMeta = new SGTMetaData(this.yLabel, this.yUnit);
				line.setXMetaData(xMeta);
				line.setYMetaData(yMeta);
					// set line properties
					LineAttribute props = new LineAttribute();
					props.setStyle(LineAttribute.HEAVY);
					props.setWidth(2.0f);
					props.setColor(this.defaultColors[i % this.defaultColors.length]);

					this.axis.addData(line, props, line.getTitle());
				}else{
					point = new SimplePoint(xvals[0],yvals[0],this.lineLabels.get(i));
					xMeta = new SGTMetaData(this.xLabel, this.xUnit);
					yMeta = new SGTMetaData(this.yLabel, this.yUnit);
					point.setXMetaData(xMeta);
					point.setYMetaData(yMeta);
					// set line properties
					PointAttribute props = new PointAttribute();
					props.setMark(PointAttribute.CENTERED);
					props.setMarkHeightP(2.0f);
					props.setColor(this.defaultColors[i % this.defaultColors.length]);

					//this.axis.addData(point, props, point.getTitle());
					//PointCollection points = new PointCollection();
					//points.add(point);
					//this.axis.addData(points);	
			}
				//
				// for special case where we want to set minimum of yaxis to 0
				//
				if (fixYmin) {
					//this.axis.setYAutoRange(false);
					SoTRange xrange = line.getXRange();
					SoTRange yrange = line.getYRange();
					SoTValue ymin = yrange.getStart();
					SoTValue yminNew = new SoTValue.Double(0.0);
					SoTValue ymax = yrange.getEnd();
					if(ymax instanceof SoTValue.Double){
						double ymaxDouble = ((SoTValue.Double)ymax).getValue();
						if(ymaxDouble>ymaxAll){
							ymaxAll = ymaxDouble;
						}
					}
					System.out.println("ymin = "+ymin);
					SoTValue ymaxNew = new SoTValue.Double(ymaxAll);
					yrange.setStart(yminNew);
					yrange.setEnd(ymaxNew);
					SoTDomain domain = new SoTDomain(xrange, yrange);
					try {
						this.axis.setRange(domain);
					} catch (PropertyVetoException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					this.axis.setYAutoRange(false);
					System.out.println("ymin = "+yminNew);
				}
			}

		}else{ // pcolor TODO extend contour etc
			this.axis = new JPlotLayout(true,false,false,"Figure",null,false);
			this.axis.setSize(this.paper.getSize());
			this.paper.add(axis,BorderLayout.CENTER);

			this.axis.setBatch(true);
			/*
			 * Set the titles.
			 */
			this.axis.setTitles(this.axisTitle,"","");
			/*
			 * Change the title sizes from the defaults.  (0.25, 0.15)
			 */
			this.axis.setTitleHeightP(0.2, 0.2);

			// draw curves curve
			SimpleGrid xyzData = null;
			SGTMetaData xMeta = null;
			SGTMetaData yMeta = null;
			for(int i=0;i<this.xGridValues.size();i++){
				xyzData = new SimpleGrid(this.zGridValues.get(i),
						this.xGridValues.get(i), this.yGridValues.get(i), "");
				xMeta = new SGTMetaData(this.xLabel, "");
				yMeta = new SGTMetaData(this.yLabel, "");
				xyzData.setXMetaData(xMeta);
				xyzData.setYMetaData(yMeta);
				this.axis.addData(xyzData, xyzData.getTitle());

			}
		}

		this.axis.setBatch(false);
		if(this.figure!=null){
			//XX this.figure.pack();
			this.figure.setVisible(true);
			this.figure.repaint();
		}
		this.axis.setVisible(true);
		this.axis.repaint();
		this.axis.validate();
		
	}

	public void wait(int length){
		try{
			Thread.sleep(length);
		}catch(InterruptedException e){
			// Do nothing
		}
	}
	
}
