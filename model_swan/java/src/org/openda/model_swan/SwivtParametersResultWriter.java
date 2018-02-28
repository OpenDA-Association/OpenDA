/* OpenDA v2.4.3 
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

package org.openda.model_swan;

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IInstance;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;

import java.io.File;

/**
 * ResultWriter that produces swivt result files
 */
public class SwivtParametersResultWriter implements IResultWriter {

    /**
     * Parameters file to write.
     */
    private File optimalParametersFile;
    /**
     * Template for parameters file.
     */
    private File parameterSettingsFile;
    private int defaultMaxSize = Integer.MAX_VALUE;

    /**
     * The given parameterSettingsFile is used as a template to write
     * a new file with the optimal parameter values.
     * The name of the new file is parameterSettingsFileName with "-opt" appended.
     *
     * @param workingDir
     * @param parameterSettingsFileName relative to workingDir.
     */
    public SwivtParametersResultWriter(File workingDir, String parameterSettingsFileName) {
        String optimalParametersFileName = BBUtils.getFileNameWithoutExtension(parameterSettingsFileName) + "-opt.xml";
        parameterSettingsFile = new File(workingDir, parameterSettingsFileName);
        optimalParametersFile = new File(workingDir, optimalParametersFileName);
    }

    public void putMessage(Source source, String message) {
        // no action
    }

    public void putMessage(IInstance source, String message) {
        // no action
    }

    public void putValue(Source source, String id, Object result) {
        // no action
    }

    public void putValue(IInstance source, String id, Object result) {
        // no action
    }

    private void writeParameters(IVector result) {
        if (result == null) {
            throw new IllegalArgumentException(this.getClass().getSimpleName() + ".writeParameters: result is null.");
        }
        if (!(result instanceof ITreeVector)) {
            throw new UnsupportedOperationException(this.getClass().getSimpleName() + ".writeParameters: unexpected result type " + result.getClass());
        }
        SwanXMLParameterSettings parameterSettings = new SwanXMLParameterSettings(parameterSettingsFile);
        ITreeVector treeVector = (ITreeVector) result;
        for (String subTreeVectorId : treeVector.getSubTreeVectorIds()) {
            double parameterValue = treeVector.getSubTreeVector(subTreeVectorId).getValues()[0];
            // catch relative parameter change, eg for FRIC.cfjon.relChange
            if(subTreeVectorId.endsWith(".relChange")){
            	String baseId=subTreeVectorId.replace(".relChange", "");
            	double originalValue = (Double) parameterSettings.getValue(baseId);
            	parameterSettings.setValue(baseId, originalValue*Math.exp(parameterValue));            	
            }else{
            	parameterSettings.setValue(subTreeVectorId, parameterValue);
            }
        }
        parameterSettings.writeToFile(optimalParametersFile);
    }

    public void putValue(Source source, String id, Object result, int iteration) {
        // no action
    }

	public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration) {
        // no action
	}

	public void putValue(IInstance source, String id, Object result, int iteration) {
        // no action
    }

    public void putIterationReport(IInstance source, int iteration, double cost, IVector parameters) {
        //this assumes that the optimal parameter values are passed to this method with iteration set to -1.
        if (iteration == -1) {
            writeParameters(parameters);
        }
    }

    
    public int getDefaultMaxSize() {
        return defaultMaxSize;
    }

    public void free() {
        // for now: no action needed
        // TODO: write parameters only here.
    }
}
