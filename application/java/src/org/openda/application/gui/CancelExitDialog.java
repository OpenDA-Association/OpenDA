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
import javax.swing.JOptionPane;

public class CancelExitDialog{

	/**
	 * Show a dialog to ask for confirmation before closing with a running algorithm
	 * @return true if confirmed
	 */
	public static boolean show(){
		String options[] = {"Exit","Cancel : continue run"};
		int reply = JOptionPane.showOptionDialog(null,
			    "Openda is still running. Do you realy want to quit?",
			    "Exit while running",
			    JOptionPane.YES_NO_OPTION,
			    JOptionPane.WARNING_MESSAGE,
			    null,
			    options,options[1]);
		return (reply==0);
	}
	
	public static void main(String args[]){
		boolean confirmed = CancelExitDialog.show();
		if(confirmed){
			System.out.println("Exit confirmed");
		}else{
			System.out.println("Exit cancelled");
			
		}
	}
	
}
