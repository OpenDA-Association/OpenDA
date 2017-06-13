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

package org.openda.model_dflowfm;

import org.openda.exchange.DoubleExchangeItem;


/**
 * Started with a copy from model_delft3d
 * and adjusted to the differences in syntax that exist with Delft3D-FM.
 */
public class DFlowFMRougnessFileExchangeItem extends DoubleExchangeItem{

    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
    int lineNum;                          // Line number that corresponds to the parameter
    int columnNum;                        // Column nmber (starting at 0)
    private String description;

    public DFlowFMRougnessFileExchangeItem(String id, int lineNum, int columnNum, double value){
    	super(id, value);
        this.lineNum    =lineNum;
        this.columnNum  =columnNum;
    }
        
    public String toString(){
    	return "parameter "+this.getId()+"(line,column)=("+this.lineNum+","+this.columnNum+") ="+this.getValue();
    }
}

