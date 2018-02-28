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

package org.openda.model_wflow;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.TimeZone;

import org.openda.blackbox.config.*;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.model_wflow.io.castorgenerated.*;
import org.openda.utils.DimensionIndex;
import org.openda.utils.io.CastorUtils;

/**
 * Configuration reader for WflowModelFactoryConfig for WFLOW model.
 *
 * @author Arno Kockx
 */
public class WflowModelFactoryConfigReader {
	private final String pythonModuleName;
	private final Date startDateTime;
	private final Date endDateTime;
	private final File caseDirectory;
	private final String templateRunId;
	private final String modelConfigFileName;
	private final String cloneMapFileName;
	private final String[] inputFilePaths;
	private final String relativeModelOutputFilePath;
	private final String relativeAnalysisOutputFilePath;
    private final String[] outputExchangeItemIds;
    private String relativeScalarModelOutputFilePath = null;
    private String relativeScalarAnalysisOutputFilePath = null;
    Collection<BBStochModelVectorConfig> scalarOutputVectorCollection = new ArrayList<BBStochModelVectorConfig>();

    public WflowModelFactoryConfigReader(File configFile) {
		WflowModelFactoryConfigXML castor = (WflowModelFactoryConfigXML) CastorUtils.parse(configFile, WflowModelFactoryConfigXML.class);

		this.pythonModuleName = castor.getPythonModuleName();
		if (this.pythonModuleName == null || this.pythonModuleName.isEmpty()) {
			throw new RuntimeException(getClass().getSimpleName() + ": Configured pythonModuleName must not be an empty string.");
		}

		WflowTimeHorizonXML timeHorizonCastor = castor.getTimeHorizon();
		if (timeHorizonCastor != null) {//if timeHorizon configured.
			TimeZone timeZone = TimeUtils.createTimeZoneFromDouble(timeHorizonCastor.getTimeZoneOffset());
			this.startDateTime = getDateTimeFromCastor(timeHorizonCastor.getStartDateTime(), timeZone);
			this.endDateTime = getDateTimeFromCastor(timeHorizonCastor.getEndDateTime(), timeZone);
			if (this.startDateTime.after(this.endDateTime)) {
				throw new RuntimeException(getClass().getSimpleName() + ": Configured startDateTime > configured endDateTime.");
			}
		} else {//if no timeHorizon configured.
			this.startDateTime = null;
			this.endDateTime = null;
		}

		File configDir = configFile.getParentFile();
		this.caseDirectory = new File(configDir, castor.getCaseDirectory());
		if (!this.caseDirectory.exists()) {
			throw new RuntimeException(getClass().getSimpleName() + ": Cannot find configured case directory " + this.caseDirectory.getAbsolutePath());
		}

		this.templateRunId = castor.getTemplateRunId();
		if (this.templateRunId == null || this.templateRunId.isEmpty()) {
			throw new RuntimeException(getClass().getSimpleName() + ": Configured runId must not be an empty string.");
		}

		this.modelConfigFileName = castor.getModelConfigFileName();
		File modelConfigFile = new File(this.caseDirectory, this.modelConfigFileName);
		if (!modelConfigFile.exists()) {
			throw new RuntimeException(getClass().getSimpleName() + ": Cannot find configured model config file " + modelConfigFile.getAbsolutePath());
		}

		this.cloneMapFileName = castor.getCloneMapFileName();
		File cloneMapFile = new File(this.caseDirectory, "staticmaps/" + this.cloneMapFileName);
		if (!cloneMapFile.exists()) {
			throw new RuntimeException(getClass().getSimpleName() + ": Cannot find configured clone map file " + cloneMapFile.getAbsolutePath());
		}

		this.inputFilePaths = castor.getInputFile();
		this.relativeModelOutputFilePath = castor.getModelOutputFile();
		this.relativeAnalysisOutputFilePath = castor.getAnalysisOutputFile();
        if (castor.getOutputExchangeItemId().length>0){
            this.outputExchangeItemIds = castor.getOutputExchangeItemId();
        } else {
            this.outputExchangeItemIds = null;
        }

        ScalarModelOutput scalarModelOutput = castor.getScalarModelOutput();
        if (scalarModelOutput!=null){
            this.relativeScalarModelOutputFilePath = castor.getScalarModelOutput().getModelOutputFile();
            this.relativeScalarAnalysisOutputFilePath = castor.getScalarModelOutput().getAnalysisOutputFile();
            if (this.relativeScalarModelOutputFilePath==null && this.relativeScalarAnalysisOutputFilePath==null){
                throw new RuntimeException(getClass().getSimpleName() + ": Either (both) model or (and) analysis scalar output file(s) should be specified.");
            }
            if (this.relativeScalarModelOutputFilePath != null){
                WflowScalarModelOutput[] vectorXMLItems = castor.getScalarModelOutput().getSubVector();
                for (WflowScalarModelOutput vectorXMLItem : vectorXMLItems) {
                    //Let's reuse some functionalities of BBStochModelVector here
                    BBStochModelVectorConfig vectorConfig = parseStochModelVectorOrSubVector(
                            null, null,vectorXMLItem);
                    scalarOutputVectorCollection.add(vectorConfig);
                }
            }
        }
	}

    private static BBStochModelVectorConfig parseStochModelVectorOrSubVector(File stochModelConfigFile, BBModelConfig bbModelConfig, WflowScalarModelOutput subVectorXML) {
        String id;
        String sourceVectorId;
        DimensionIndex[] selectionIndices = null;
        BBConfigurable selectorConfig = null;
        // subvector (with sub-indices or selector)
        id = subVectorXML.getId();
        sourceVectorId = subVectorXML.getSourceVectorId();
        if (sourceVectorId == null) {
            sourceVectorId = id;
        }
        selectionIndices = parseSelectionIndices(subVectorXML.getSelection());
        return new BBStochModelVectorConfig(
                id, sourceVectorId, selectionIndices, selectorConfig);
    }

    private static DimensionIndex[] parseSelectionIndices(WflowIndicesXML indicesXML) {
        if (indicesXML != null) {
            return SelectionIndices(indicesXML.getIndex1(),
                    indicesXML.getIndex2(), indicesXML.getIndex3(), indicesXML.getBase());
        }
        return null;
    }

    private static DimensionIndex[] SelectionIndices(String index1, String index2, String index3, int dimensionBase) {
        DimensionIndex[] selectionIndices;
        int numIndices = 1;
        if (index2 != null) numIndices++;
        if (index3 != null) numIndices++;

        selectionIndices = new DimensionIndex[numIndices];

        selectionIndices[0] = new DimensionIndex(index1, dimensionBase);
        if (index2 != null) selectionIndices[1] = new DimensionIndex(index2, dimensionBase);
        if (index3 != null) selectionIndices[2] = new DimensionIndex(index3, dimensionBase);
        return selectionIndices;
    }

    public String getPythonModuleName() {
		return this.pythonModuleName;
	}

	public Date getStartDateTime() {
		return this.startDateTime;
	}

	public Date getEndDateTime() {
		return this.endDateTime;
	}

	public File getCaseDirectory() {
		return this.caseDirectory;
	}

	public String getTemplateRunId() {
		return this.templateRunId;
	}

	public String getModelConfigFileName() {
		return this.modelConfigFileName;
	}
	public String getCloneMapFileName() {
		return this.cloneMapFileName;
	}

	public String[] getRelativeInputFilePaths() {
		return this.inputFilePaths;
	}

	public String getRelativeModelOutputFilePath() {
		return this.relativeModelOutputFilePath;
	}

	public String getRelativeAnalysisOutputFilePath() {
		return this.relativeAnalysisOutputFilePath;
	}

    public String[] getOutputExchangeItemIds() {
        return this.outputExchangeItemIds;
    }

    public String getRelativeScalarModelOutputFilePath() {
        return this.relativeScalarModelOutputFilePath;
    }

    public String getRelativeScalarAnalysisOutputFilePath() {
        return this.relativeScalarAnalysisOutputFilePath;
    }

    public Collection<BBStochModelVectorConfig> getScalarOutputVectorCollection() {
        return this.scalarOutputVectorCollection;
    }

	private static Date getDateTimeFromCastor(WflowDateTimeXML castor, TimeZone timeZone) {
		return new Date(TimeUtils.getDateTimeFromCastor(castor.getDate(), castor.getTime(), timeZone));
	}
}
