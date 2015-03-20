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

import javax.swing.filechooser.FileFilter;
import java.io.File;

public class ExtensionFileFilter extends FileFilter {
    String description;
    String[] extensions = null;

    public ExtensionFileFilter(String description, String extension) {
        this(description, new String[]{extension});
    }

    public ExtensionFileFilter(String description, String[] extensions) {
        if (description == null) {
            this.description = extensions[0];
        } else {
            this.description = description;
        }
        this.extensions = extensions.clone();
        toLower(this.extensions);
    }

    private void toLower(String[] array) {
        for (int i = 0, n = array.length; i < n; i++) {
            array[i] = array[i].toLowerCase();
        }
    }

    public String getDescription() {
        return description;
    }

    public boolean accept(File file) {
        if (file.isDirectory()) {
            return true;
        } else {
           String path = file.getAbsolutePath().toLowerCase();
           for (int i = 0, n = extensions.length; i < n; i++) {
               String extension = extensions[i];
               if (path.endsWith(extension) && path.charAt(path.length() - extension.length() - 1) == '.') {
                   return true;
               }
           }
       }
       return false;
    }
}
