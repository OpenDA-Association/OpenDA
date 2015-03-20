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
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import org.openda.application.ApplicationRunner;
import org.openda.utils.Results;


/**
 * Catch some problems before closing a window, such as running applications and unsaved files.
 * 
 * @author verlaanm
 *
 */
public class WindowExitHandler extends WindowAdapter {

	ApplicationScreen frame=null;
	boolean autosave=false;
	
	public WindowExitHandler(ApplicationScreen frame){
		this.frame = frame;
	}
	
	public void windowClosing(WindowEvent e){
		this.tryConfirmedExit();
	}
	
	public void tryConfirmedExit(){
		// unsaved files
    	InputGui inputGui = (InputGui) frame.inputTab;
    	if(inputGui.needsSave()){
    		Results.putMessage("Unsaved files detected");
    		boolean willSave=true;
    		if(!autosave){
    			willSave = UnsavedFilesDialog.show();
    		}else{
    			Results.putMessage("Automatically save input files.");
    		}
    		if(willSave){
    			((InputGui) frame.inputTab).saveInput();
    			Results.putMessage("Input files saved.");
    		}else{
    			Results.putMessage("Unsaved files discarded.");
    		}
    	}
		
		// running application
		ControlGui controlGui = (ControlGui) frame.controlTab;
		ApplicationRunner.Status status = controlGui.getStatus();
    	if((status==ApplicationRunner.Status.RUNNING)
    			|(status==ApplicationRunner.Status.INITIALIZING)
    			|(status==ApplicationRunner.Status.INITIALIZED)
    			|(status==ApplicationRunner.Status.PAUSED)){
    		
    		boolean confirm = CancelExitDialog.show();
    		if(confirm){
    			Results.putMessage("Exit of running experiment confirmed.");
    			System.exit(0);
    		}
    		Results.putMessage("Exit for running experiment cancelled.");
    	}else{ // not running
    		System.exit(0);
    	}	
	}
	
	public void unsavedFilesCheckAndSave(){
    	InputGui inputGui = (InputGui) frame.inputTab;
    	if(inputGui.needsSave()){
    		Results.putMessage("Unsaved files detected");
    		boolean willSave=true;
    		if(!autosave){
    			willSave = UnsavedFilesDialog.show();
    		}else{
    			Results.putMessage("Automatically save input files.");
    		}
    		if(willSave){
    			((InputGui) frame.inputTab).saveInput();
    			Results.putMessage("Input files saved.");
    		}else{
    			Results.putMessage("Unsaved files discarded.");
    		}
    	}
	}

}
