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

package org.openda.blackbox.config;

import org.openda.utils.io.FileSupport;

import java.io.File;
import java.util.Collection;

/**
 * TODO: description
 */
public class BBCheckOutput {

    private final String fileName;
	private final String expect;
	private final AliasDefinitions aliasDefinitions;
	private final Collection<String> aliasesUsedInFileName;
	private Collection<String> aliasesUsedInExpect;

    public BBCheckOutput(String fileName, String expect, AliasDefinitions aliasDefinitions) {
        this.fileName = fileName;
        this.expect = expect;
        this.aliasDefinitions = aliasDefinitions;
        this.aliasesUsedInExpect = aliasDefinitions.getUsedAliasIds(expect);
        this.aliasesUsedInFileName = aliasDefinitions.getUsedAliasIds(fileName);
    }

    public void performCheck(File checkDir) {
        boolean success;
        String fileName = aliasDefinitions.apply(this.fileName, aliasesUsedInFileName);
        File file = new File(checkDir, fileName);
        success = performCheckOnFile(file);
        if ( !success ) {
            throw new IllegalStateException("OutputCheck failed on file: " + file.getAbsolutePath());
        }
    }

    private boolean performCheckOnFile(File file) {
        boolean result = file.exists();
        if (!result) throw new IllegalStateException("OutputCheck failed file does not exist: " + file.getAbsolutePath());
        if (expect != null) {
            String expect = this.expect;
            String findString = aliasDefinitions.apply(expect, aliasesUsedInExpect);
            boolean succes = FileSupport.FileContains(file,findString);
            if (!succes) {
                throw new IllegalStateException("OutputCheck failed: could not find '" + findString + "' in file " + file.getAbsolutePath());
            }
        }
        return result;
    }

    public String toString() {
        return aliasDefinitions.apply(fileName, aliasesUsedInFileName) + " exists" +
                (expect != null ? ", expected: " + expect : "");
    }
}
