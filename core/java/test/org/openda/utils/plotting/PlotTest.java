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
import java.awt.*;
import javax.swing.JFrame;
import junit.framework.TestCase;

public class PlotTest extends TestCase {

	public static void tstPlot_1() {
		System.out.println("==============================================================================");
		System.out.println("Basic line plot with explicit figure handle");
		System.out.println("==============================================================================");
	
		// explicitly create a plot
		Figure fig1 = new Figure("Test 1");
		
		// simple plot (opens new window or uses existing one)
		double xVals[] = {1.0,2.0,3.0,4.0,5.0};
		double yVals[] = {1.1,0.9,0.85,0.83,0.82};
		fig1.addPlot(xVals,yVals,"decline");
		fig1.drawnow();
		fig1.wait(1000);
		
		// elementary layout
		fig1.setXlabel("x-axis","x-unit");
		fig1.setYlabel("y-axis","y-unit");
		fig1.setAxisTitle("My title");
		fig1.drawnow();
		fig1.wait(1000);
		
		// add another curve
		double xVals2[] = {1.0,2.0,3.0,4.0,5.0};
		double yVals2[] = {0.5,0.6,0.7,0.75,0.77};
		fig1.addPlot(xVals2,yVals2,"recovery");
		fig1.drawnow();
		fig1.wait(1000);

		// add another curve
		double xVals3[] = {1.0,2.0,3.0,4.0,5.0};
		double yVals3[] = {0.5,0.4,0.2,0.0,0.0};
		fig1.addPlot(xVals3,yVals3,"recovery");
		fig1.drawnow();
		fig1.wait(1000);
}
	
	public static void testPlot_2() {
		System.out.println("==============================================================================");
		System.out.println("Basic grid plot ");
		System.out.println("==============================================================================");
	
		// explicitly create a plot
		Figure fig1 = new Figure("Test 2");
		
		// simple plot (opens new window or uses existing one)
		int x_size = 41;
		int y_size = 61;
		double xmin=  0.0;
		double xmax= 10.0;
		double ymin=  0.0;
		double ymax= 10.0;
		double xVals[] = new double[x_size];
		double yVals[] = new double[y_size];
		double zVals[] = new double[x_size*y_size];
		// compute z=sin(x)*cos(y)
		for(int i=0;i<x_size;i++){xVals[i]=xmin+(double)i*(xmax-xmin)/((double)x_size);}
		for(int j=0;j<y_size;j++){yVals[j]=ymin+(double)j*(ymax-ymin)/((double)y_size);}
		for(int i=0;i<x_size;i++){
			for(int j=0;j<y_size;j++){ // use c row-ordering
				zVals[(i*y_size)+j] = Math.sin(xVals[i])*Math.cos(yVals[j]);
			}
		}
		// make plot
		fig1.addPcolor(xVals,yVals,zVals);
		fig1.drawnow();
		fig1.wait(1000);
		
		// elementary layout
		fig1.setXlabel("x-axis","x-unit");
		fig1.setYlabel("y-axis","y-unit");
		fig1.setAxisTitle("My title");
		fig1.drawnow();
		fig1.wait(1000);

	}
	
	public static void testPlot_3() {
		System.out.println("==============================================================================");
		System.out.println("plotting in your own panel , ie embedding the plot");
		System.out.println("==============================================================================");
	
		// explicitly create a plot
		JFrame frame = new JFrame();
		frame.setSize(new Dimension(600,600)); ///TODO
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		Container pane = frame.getContentPane();
		//JPanel pane = new JPanel();
		Figure fig1 = new Figure(pane);
		
		// simple plot (opens new window or uses existing one)
		double xVals[] = {1.0,2.0,3.0,4.0,5.0};
		double yVals[] = {1.1,2.2,3.3,4.4,5.5};
		fig1.addPlot(xVals,yVals,"curve 1");
		fig1.drawnow();
		frame.setVisible(true);
		fig1.wait(1000);
		
		// elementary layout
		fig1.setXlabel("x-axis","x-unit");
		fig1.setYlabel("y-axis","y-unit");
		fig1.setAxisTitle("My title");
		fig1.drawnow();
		frame.setVisible(true);
		fig1.wait(1000);
		
		// add another curve
		double xVals2[] = {1.0,2.0,3.0,4.0,5.0};
		double yVals2[] = {4.1,4.2,4.3,4.4,4.5};
		fig1.addPlot(xVals2,yVals2,"curve 2");
		fig1.drawnow();
		frame.setVisible(true);
		fig1.wait(3000);
	}
	
	public static void tstPlot_4() {
		System.out.println("==============================================================================");
		System.out.println("Basic line plot only one point - problematic in sgt.");
		System.out.println("==============================================================================");
	
		// explicitly create a plot
		Figure fig1 = new Figure("Test 3");
		
		// simple plot (opens new window or uses existing one)
		double xVals[] = {1.0};
		double yVals[] = {1.1};
		fig1.addPlot(xVals,yVals,"point"); //TODO points do not work yet
		fig1.drawnow();
		fig1.wait(3000);
		
	}
	
	public static void testPlot_5() {
		System.out.println("==============================================================================");
		System.out.println("plotting update - adding points");
		System.out.println("==============================================================================");
	
		// explicitly create a plot
		JFrame frame = new JFrame();
		frame.setSize(new Dimension(600,600)); ///TODO
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		Container pane = frame.getContentPane();
		Figure fig1 = new Figure(pane);
		frame.setVisible(true);
		// simple plot (opens new window or uses existing one)
		double xValsAll[] = {1.0,2.0,3.0,4.0,5.0,6.0,7.0};
		double yValsAll[] = {6.1,5.2,4.3,3.4,3.2,3.1,3.05};
		double xVals[];
		double yVals[];
		for(int i=1;i<=xValsAll.length;i++){
			xVals = new double[i];
			yVals = new double[i];
			for(int j=0;j<i;j++){ //copy relevant part
				xVals[j] = xValsAll[j];
				yVals[j] = yValsAll[j];
			}
			fig1.plot(xVals,yVals,"my curve");
			fig1.setXlabel("x-axis","x-unit");
			fig1.setYlabel("y-axis","y-unit");
			fig1.setAxisTitle("My title");
			fig1.drawnow();
			frame.setVisible(true);
		    //if(i==1){
		    //	frame.setVisible(true);
		    //}
			fig1.wait(1000);
		}
	}

	
	
}
