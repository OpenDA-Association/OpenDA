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

package org.openda.model_swan.swivt;

import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import java.io.File;

public class FileBrowser extends JFrame{
    private File selectedFile;

    public FileBrowser(String title, File currentSelection, String extension, boolean directoriesOnly){
		super(title);
        selectedFile = currentSelection;
        setSize(800,600);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        JFileChooser chooser = new JFileChooser(currentSelection);
        if (directoriesOnly) {
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        }
        if (extension != null && extension.length() > 0) {
            FileFilter filter = new FileExtensionFilter(extension.toUpperCase(), extension);
            chooser.setFileFilter(filter);
        }
        if (chooser.showOpenDialog(this) ==JFileChooser.APPROVE_OPTION) {
            selectedFile = chooser.getSelectedFile();
        }
	}

    public File getSelectedFile() {
        return selectedFile;
    }
}