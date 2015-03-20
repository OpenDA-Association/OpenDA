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


package org.openda.utils.io;

import org.openda.core.io.castorgenerated.EnsembleFiltersRestartXML;
import org.openda.core.io.castorgenerated.FilterRestartXML;
import org.openda.core.io.castorgenerated.types.FilterNameXML;

import java.io.File;

/**
 * Restart settings for an OpenDA algorithm
 */
public class FilterRestartSettingsFile {

    FilterRestartXML filterRestartXML;

    public FilterRestartSettingsFile(String filterName) {
        filterRestartXML = new FilterRestartXML();
        filterRestartXML.setFilterName(FilterNameXML.valueOf(filterName));
    }

    public void setEnsembleSize(int ensembleSize) {
        switch (filterRestartXML.getFilterName().getType()) {
			case FilterNameXML.PARTICLE_TYPE:
			case FilterNameXML.ENKF_TYPE:
			case FilterNameXML.ENSR_TYPE:
				filterRestartXML.setEnsemble(new EnsembleFiltersRestartXML());
				filterRestartXML.getEnsemble().setSize(ensembleSize);
				break;
			case FilterNameXML.ENSSKF_TYPE:
				throw new IllegalStateException(this.getClass().getName() +
						"Ensemble size can not be set for filter " + filterRestartXML.getFilterName().toString());
		}
    }

	public String getFilterName() {
		return filterRestartXML.getFilterName().toString();
	}

	public int getEnsembleSize() {
		if (filterRestartXML.getEnsemble() == null) {
			throw new IllegalStateException(this.getClass().getName() +
					"Ensemble size not set for for filter " + filterRestartXML.getFilterName().toString());
		}
		return filterRestartXML.getEnsemble().getSize();
	}

	public void setComment(String comment) {
        filterRestartXML.setComment(comment);
    }

    public String getComment() {
        return filterRestartXML.getComment();
    }

    public void writeToFile(File restartFile) {
        // check consistency.
		switch (filterRestartXML.getFilterName().getType()) {
			case FilterNameXML.PARTICLE_TYPE:
			case FilterNameXML.ENKF_TYPE:
			case FilterNameXML.ENSR_TYPE:
				if (filterRestartXML.getEnsemble() == null) {
					throw new IllegalStateException(this.getClass().getName() +
							"Ensemble size not set for for filter " + filterRestartXML.getFilterName().toString());
				}
			case FilterNameXML.ENSSKF_TYPE:
				break;
		}
		CastorUtils.write(filterRestartXML, restartFile, "filterRestart", null, null);
    }

    public FilterRestartSettingsFile(String filterName, File restartFile) {
        if (restartFile == null) {
            throw new UnsupportedOperationException("argument restart file == null");
        }
        if (!restartFile.exists()) {
            throw new UnsupportedOperationException("file " + restartFile.getAbsolutePath() + " does not exist: ");
        }
        filterRestartXML = 
        	 (FilterRestartXML) CastorUtils.parse(restartFile, FilterRestartXML.class);
        
        if (filterName != null && 
            !filterName.equalsIgnoreCase(filterRestartXML.getFilterName().toString())) {
            throw new RuntimeException("Invalid filter filterName in file " + 
            		                    restartFile.getAbsolutePath());
        }
    }
}
