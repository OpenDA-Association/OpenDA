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
import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import java.io.File;


public class FileDialog extends JFrame{

	File file = null;
	JFileChooser chooser = null;
	final static int SAVE=1;
	final static int OPEN=2;
	
	public FileDialog(String title,File start, int saveOrOpen){
		super(title);
		setSize(800,600);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		chooser = new JFileChooser(start);
        FileFilter filter = new ExtensionFileFilter("ODA", new String[] {"oda"});
        chooser.setFileFilter(filter);
        int accepted = 0;
		if(saveOrOpen==FileDialog.OPEN){
			accepted = chooser.showOpenDialog(this);
		}
		if(saveOrOpen==FileDialog.SAVE){
			accepted = chooser.showSaveDialog(this);
		}
		// Dialog returns when done
        if (accepted == JFileChooser.APPROVE_OPTION) {
		    this.file = this.chooser.getSelectedFile();
        } else {
            this.file = null;
        }
	}

	public static File openInput(File start){
		FileDialog dia = new FileDialog("Open file",start,FileDialog.OPEN);
		return dia.file;
	}

	public static File saveInput(File start){
		FileDialog dia = new FileDialog("Save file",start,FileDialog.SAVE);
		return dia.file;
	}
}
