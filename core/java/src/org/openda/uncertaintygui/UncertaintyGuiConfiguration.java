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
package org.openda.uncertaintygui;

import org.openda.uncertainties.Uncertainties;
import org.openda.uncertaintygui.tablemodels.SelectedResult;

import java.io.File;
import java.util.ArrayList;

/**
 * Uncertain GUI confiration
 */
public class UncertaintyGuiConfiguration {

    private File uncertaintiesSpecificationFile;
    private File resultSelectionFile;
    private int endMonteCarloRun;
    private int startMonteCarloRun;
    private Uncertainties uncertaintiesObject;
    private ArrayList<SelectedResult> resultSelectionList;

    public File getUncertaintiesSpecificationFile() {
        return uncertaintiesSpecificationFile;
    }

    public File getResultSelectionFile() {
        return resultSelectionFile;
    }

    public int getEndMonteCarloRun() {
        return endMonteCarloRun;
    }

    public int getStartMonteCarloRun() {
        return startMonteCarloRun;
    }

    public Uncertainties getUncertaintiesObject() {
        return uncertaintiesObject;
    }

    public void setUncertaintiesSpecificationFile(File uncertaintiesSpecificationFile) {
        this.uncertaintiesSpecificationFile = uncertaintiesSpecificationFile;
    }

    public void setResultSelectionFile(File resultSelectionFile) {
        this.resultSelectionFile = resultSelectionFile;
    }

    public void setEndMonteCarloRun(int endMonteCarloRun) {
        this.endMonteCarloRun = endMonteCarloRun;
    }

    public void setStartMonteCarloRun(int startMonteCarloRun) {
        this.startMonteCarloRun = startMonteCarloRun;
    }

    public void setUncertaintiesObject(Uncertainties uncertaintiesObject) {
        this.uncertaintiesObject = uncertaintiesObject;
    }

    public void setResultSelectionList(ArrayList<SelectedResult> resultSelections) {
        this.resultSelectionList = resultSelections;
}
    public ArrayList<SelectedResult> getResultSelectionList() {
        return resultSelectionList;
    }
}
