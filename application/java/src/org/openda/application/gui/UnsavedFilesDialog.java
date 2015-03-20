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

public class UnsavedFilesDialog {

	/**
	 * Show a dialog to ask if unsaved files should be saved before continuing
	 * @return true if confirmed
	 */
	public static boolean show(){
		String options[] = {"Yes, save","No, do not save"};
		int reply = JOptionPane.showOptionDialog(null,
			    "Openda detected unsaved changes in the input files. Do you want to save now?",
			    "Unsaved changes",
			    JOptionPane.YES_NO_OPTION,
			    JOptionPane.WARNING_MESSAGE,
			    null,
			    options,options[0]);
		return (reply==0);
	}
	
	public static void main(String args[]){
		boolean confirmed = UnsavedFilesDialog.show();
		if(confirmed){
			System.out.println("Save confirmed");
		}else{
			System.out.println("Save denied");
			
		}
	}

}
