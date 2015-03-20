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
import java.util.Vector;

/**
 * Global data for plots. The screen may contain several figures, a figure may contain more
 * than one axis and an axis may contain more than one graph.
 * An axis may also be connected to an existing panel in an application.
 * @author verlaanm
 *
 */

public class Screen {
	// Figures
	private static Vector<Figure> figures = null;
	// current figure
	private static int currentFigure =0;
	
	public void delete(int figureNumber){
		
	}
	
	public void clearScreen(){
		
	}
	
	public int getCurrentFigure(){
		return Screen.currentFigure;
	}
	
	public Figure setCurrentFigure(int currentFigure){
		Figure result = null;
		if((currentFigure>=0) & (currentFigure<Screen.figures.size())){
		   Screen.figures.get(currentFigure);
		}
		return result;
	}
}
