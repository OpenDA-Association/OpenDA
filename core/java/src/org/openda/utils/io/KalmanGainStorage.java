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

import org.openda.blackbox.config.BBUtils;
import org.openda.core.io.castorgenerated.*;
import org.openda.costa.CtaTreeVector;
import org.openda.costa.CtaVector;
import org.openda.exchange.dataobjects.NetcdfUtils;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IDimensionIndex;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;
import org.openda.resultwriters.NetcdfResultWriterNative;
import org.openda.utils.Vector;
import org.openda.utils.*;
import ucar.ma2.Array;
import ucar.ma2.*;
import ucar.nc2.*;
import ucar.nc2.dataset.NetcdfDataset;

import java.io.*;
import java.util.*;

/**
 * Kalman gain storage object, used to write or read the kalman gain for a certain time stamp to or from file.
 */
public class KalmanGainStorage {

	public static final String STATION_DIMENSION = "station_dimension";
	public static final String STATION_ID = "station_id";
	public static final int STATION_ID_CHAR_LENGTH = 64;
	public static final String CHAR_LENGTH_ID = "char_length_id";
	public static final String PARENT_VECTOR_ID = "parentVectorId";
	public static final String TIME_STAMP = "time_stamp";
	public static final String OBSERVATION_OFFSET = "observation_offset";
	public static final Attribute TIME_STAMP_UNIT_ATT = new Attribute("units", "days");
	public static final Attribute TIME_STAMP_LONG_NAME_ATT = new Attribute("long_name", "time in MJD");
	public static final Attribute STATION_IDENTIFICATION_CODE_ATT = new Attribute("long_name", "station identification code");
	public static final Attribute TIME_SERIES_ID_ATT = new Attribute("cf_role", "timeseries_id");
	public static final Attribute OBSERVATION_OFFSET_LONG_NAME_ATT = new Attribute("long_name", "offset in days for observations");
	public static final Attribute HK_LONG_NAME_ATT = new Attribute("long_name", "HK");
	public static final Attribute KALMAN_GAIN_LONG_NAME_ATT = new Attribute("long_name", "kalman gain");
	public static final Attribute FRACTIONS_UNIT_ATT = new Attribute("units", "1");
	public static int DefaultMaxKeepVectorInXMLSize = 40;

	// set in constructor
	private File workingDir;
	private double timeStampAsMJD = Double.MIN_VALUE;;

	// properties that be can be overridden before writing
	private String storageDirPrefix = "kgStorage_";
	private String columnFilePrefix = "obsColumn_";
	private String kalmanGainStorageFileName = "kalmanGainStorage.xml";
	private int maxKeepVectorInXMLSize = DefaultMaxKeepVectorInXMLSize;
	private boolean useTimeStampInDirectoryName = true;
	private double[][] hk = null;

	public enum StorageType {
		xml, netcdf, automatic, netcdf_cf
	}
	private StorageType gainFileType = StorageType.automatic;

	// properties that be can be overridden before writing, and queried after reading
	private int stateSize = 0;
	private String comment = null;

	// properties that are filled after reading
	private String[] observationIds = null;
	private double[] observationOffsetInDays = null;
	private IVector[] kalmanGainColumns = null;


	/**
	 * Constructor for creating and writing kalman gain storage object, used to read or to write the kalman gain.
	 * The kalman gain is stored in a separate directory, with a separate file for each kalman gain column
	 * in the kalman gain matrix.
	 *
	 * @param workingDir			   The directory under which the directory for the kalman gain for the
	 *                                 specified time stamp must be created
	 * @param timeStampAsMJD		   The time stamp for which the kalman gain is valid
	 */
	public KalmanGainStorage(File workingDir, double timeStampAsMJD) {
		this.workingDir = workingDir;
		this.timeStampAsMJD = timeStampAsMJD;
	}

	/**
	 * Constructor for creating and writing kalman gain storage object, used to read or to write the kalman gain.
	 * The kalman gain is stored in a separate directory, with a separate file for each kalman gain column
	 * in the kalman gain matrix.
	 *
	 * @param workingDir			   The directory under which the directory for the kalman gain for the
	 *                                 specified time stamp must be created
	 * @param timeStampAsMJD		   The time stamp for which the kalman gain is valid
	 */
	public KalmanGainStorage(File workingDir, double timeStampAsMJD, boolean useTimeStampInDirectoryName) {
		this.workingDir = workingDir;
		this.timeStampAsMJD = timeStampAsMJD;
		this.useTimeStampInDirectoryName = false;
	}

	/**
	 * Constructor for reading kalman gain storage object, used to read the kalman gain.
	 * The kalman gain is stored with a separate file for each kalman gain column
	 * in the kalman gain matrix.
	 *
	 * @param workingDir			   The directory of the kalman gain
	 */
	public KalmanGainStorage(File workingDir) {
		this.workingDir = workingDir;
		this.useTimeStampInDirectoryName = false;
	}

	/**
	 * Set the file prefix to be used for storing the kalman gain column files.  
	 * @param storageDirPrefix The file prefix for.
	 */
	public void setStorageDirPrefix(String storageDirPrefix) {
		this.storageDirPrefix = storageDirPrefix;
	}

	/**
	 * Set the prefix to be used for the directory name for storing the kalman gain column files.
	 * @param columnFilePrefix The file prefix for.
	 */
	public void setColumnFilePrefix(String columnFilePrefix) {
		this.columnFilePrefix = columnFilePrefix;
	}
	
	/**
	 * Set the file type for storage of Kalman gain vectors
	 * @param fileType 
	 */
	public void setColumnFileType(StorageType fileType){
		this.gainFileType = fileType;
	}
	
	/**
	 * Get the file type for the storage of Kalman gain vectors
	 * @return fileType eg netcdf, xml or automatic
	 */
	public StorageType getColumnFileType(){
		return this.gainFileType;
	}

	/**
	 * Set the name of the kalman gain storage xml file
	 * @param kalmanGainStorageFileName The file name
	 */
	public void setKalmanGainStorageFileName(String kalmanGainStorageFileName) {
		this.kalmanGainStorageFileName = kalmanGainStorageFileName;
	}

	/**
	 * Set the maximum size of (tree)vectors that should be stored in the xml file instead of in separate
	 * net cdf files. The default is 40.
	 * @param maxKeepVectorInXMLSize The maximum size
	 */
	public void setMaxKeepVectorInXMLSize(int maxKeepVectorInXMLSize) {
		this.maxKeepVectorInXMLSize = maxKeepVectorInXMLSize;
	}

	/**
	 * Set a comment to be included in the xml file.
	 * @param comment The comment to be included
	 */
	public void setComment(String comment) {
		this.comment = comment;
	}

	/**
	 * Write the kalman gain matrix to xml file and, in case of large column vectors, to netdcf files.
	 * A HashMap is used with Id's for each column. This helps to find the right column matching an observation
	 * @param gainVectors each vector corresponds to a column of the the Kalman gain and matches an observation
	 * @param obsId combination of location and quantity to map observations to the right column
	 * @param obsTimeOffset For observations at a time preceding the analysis time this gives the offset in days.
	 */
	public void writeKalmanGain(HashMap<String, IVector> gainVectors,
			HashMap<String, String> obsId, HashMap<String, Double> obsTimeOffset) {
		int n=gainVectors.size();
		String[] observationIds=new String[n];
		double[] observationOffsetsInDays = new double[n];
		IVector[] kalmanGainColumns = new IVector[n];
		Set<String> gainIds = gainVectors.keySet();
		int index=0;
		for(String gainId : gainIds){
			observationIds[index] = obsId.get(gainId);
			observationOffsetsInDays[index] = obsTimeOffset.get(gainId);
			kalmanGainColumns[index] = gainVectors.get(gainId);
			index++;
		}
		writeKalmanGain(observationIds,observationOffsetsInDays,kalmanGainColumns, hk);
	}	
	
	/**
	 * Write the kalman gain matrix to xml file and, in case of large column vectors, to netdcf files.
	 *
	 * @param observationIds           The observation identifiers
	 * @param observationOffsetsInDays The time offset of the observations, expressed in days,
	 *                                 relative to the time stamp for this kalman gain
	 *                                 (0 means: same time stamp as the gain,
	 *                                 negative means before the kalman gain time stamp)
	 * @param kalmanGainColumns        The vectors to be written to the kalman gain column files.
	 * @param hk
	 */
	public void writeKalmanGain(String[] observationIds, double[] observationOffsetsInDays, IVector[] kalmanGainColumns, double[][] hk) {
		if (kalmanGainColumns == null || kalmanGainColumns.length == 0 ) {
			throw new IllegalArgumentException(this.getClass().getName() +
					": at least one kalman gain column must be provided");
		}
		if (observationIds == null || observationIds.length != kalmanGainColumns.length) {
			throw new IllegalArgumentException(this.getClass().getName() +
					": inconsistent lengths for observationIds and kalmanGainColumns");
		}
		if (observationOffsetsInDays == null || observationOffsetsInDays.length != kalmanGainColumns.length) {
			throw new IllegalArgumentException(this.getClass().getName() +
					": inconsistent lengths for observationOffsetsInDays and kalmanGainColumns");
		}

		// store observations
		File directoryForStorage = determineStorageDirectory(false);

		if (this.gainFileType == StorageType.netcdf_cf) {
			writeKalmanGainToNetcdfCF(observationIds, observationOffsetsInDays, kalmanGainColumns, directoryForStorage, hk);
			return;
		}

		long start = System.currentTimeMillis();
		// store general info
		KalmanGainStorageXML kgStorageXML = new KalmanGainStorageXML();
		kgStorageXML.setComment(comment);
		kgStorageXML.setTimeStampAsMJD(timeStampAsMJD);
		kgStorageXML.setTimeStampAsDateTime(Time.timeStampToDate(timeStampAsMJD));

		KalmanGainObservationsXML observationsXML = new KalmanGainObservationsXML();
		kgStorageXML.setObservations(observationsXML);
		for (int i = 0; i < observationIds.length; i++) {
			KalmanGainObservationXML observationXML = new KalmanGainObservationXML();
			observationsXML.addObservation(observationXML);

			// store id and time
			observationXML.setId(observationIds[i]);
			observationXML.setTimeOffsetInDays(observationOffsetsInDays[i]);

			// Store the values in the xml files if the vectors are small, otherwise write netCdf-file
			KalmanGainObservationXMLChoice observationXMLChoice = new KalmanGainObservationXMLChoice();
			observationXML.setKalmanGainObservationXMLChoice(observationXMLChoice);
			boolean writeToXml = (this.gainFileType==StorageType.xml)
				||( (this.gainFileType==StorageType.automatic) && (kalmanGainColumns[i].getSize()<=this.maxKeepVectorInXMLSize) );
			if (writeToXml) {
				if(kalmanGainColumns[i] instanceof ITreeVector) {
					TreeVectorXML treeVectorXML = TreeVectorWriter.createTreeVectorXML((ITreeVector) kalmanGainColumns[i], false);
					observationXMLChoice.setTreeVector(treeVectorXML);
				} else {
					String vectorString = TreeVectorWriter.createVectorXML(kalmanGainColumns[i]);
					observationXMLChoice.setVector(vectorString);
				}
			} else {
				// netCdf-file
				String netcdfFileName = columnFilePrefix + String.valueOf(i+1) + ".nc";
				observationXMLChoice.setFileName(netcdfFileName);
				if (!kgStorageXML.hasStateSize()) {
					kgStorageXML.setStateSize(kalmanGainColumns[i].getSize());
				}
				netcdfFileName = new File(directoryForStorage, netcdfFileName).getAbsolutePath();
				writeKalmanGainColumnToNetCdfFile(netcdfFileName, kalmanGainColumns[i]);
			}
		}
		File kgStorageXmlFile = new File(directoryForStorage, kalmanGainStorageFileName);
		CastorUtils.write(kgStorageXML, kgStorageXmlFile, "opendaKalmanGainStorage", null, null);
		long timePassed = System.currentTimeMillis() - start;
		Results.putMessage("Writing kalman gain to xml took: " + timePassed);
	}

	private void writeKalmanGainToNetcdfCF(String[] observationIds, double[] observationOffsetsInDays, IVector[] kalmanGainColumns, File directoryForStorage, double[][] hk) {
		NetcdfFileWriter netcdfFileWriter = null;
		try {
			File file = new File(directoryForStorage, kalmanGainStorageFileName);
			netcdfFileWriter = NetcdfFileWriter.createNew(NetcdfFileWriter.Version.netcdf3, file.getAbsolutePath());
			addGlobalAttributes(netcdfFileWriter);
			Variable timeStampVariable = createTimeStampVariable(netcdfFileWriter);
			Dimension stationDimension = netcdfFileWriter.addDimension(null, STATION_DIMENSION, observationIds.length);
			netcdfFileWriter.addDimension(null, CHAR_LENGTH_ID, STATION_ID_CHAR_LENGTH);
			Variable observationOffsetVariable = createObservationOffsetVariable(netcdfFileWriter);
			Variable hkVariable = hk != null ? createHKVariable(netcdfFileWriter) : null;
			Variable stationVariable = createStationVariable(netcdfFileWriter);

			KalmanGainVariableData[] kalmanGainVariableData = getKalmanGainVariableData(kalmanGainColumns, netcdfFileWriter, stationDimension);
			netcdfFileWriter.create();

			writeStationData(observationIds, netcdfFileWriter, stationVariable);
			writeTimeStampData(netcdfFileWriter, timeStampVariable);
			writeObservationOffsetData(observationOffsetsInDays, netcdfFileWriter, observationOffsetVariable);
			if (hkVariable != null) writeHK(hk, netcdfFileWriter, hkVariable);

			for (int observationIndex = 0; observationIndex < kalmanGainColumns.length; observationIndex++) {

				ITreeVector kalmanGainColumnForObservation = (ITreeVector) kalmanGainColumns[observationIndex];
				for (KalmanGainVariableData kalmanGainVariableDatum : kalmanGainVariableData) {
					Variable variable = kalmanGainVariableDatum.getVariable();
					ITreeVector dataVector = getDataVector(kalmanGainColumnForObservation, kalmanGainVariableDatum);
					ArrayList<Dimension> dimensions = kalmanGainVariableDatum.getDimensions();
					Array values = null;
					if (dimensions.size() == 1) {
						values = getData1D(dataVector, dimensions);
					} else if (dimensions.size() == 2) {
						values = getData2D(dataVector, dimensions);
					}
					int[] origin = new int[dimensions.size() + 1];
					origin[0] = observationIndex;
					try {
						netcdfFileWriter.write(variable, origin, values);
					} catch (IOException ioe) {
						throw new RuntimeException(ioe);
					}
				}
			}
		} catch (Exception e) {
			throw new RuntimeException(e.getMessage(), e);
		} finally {
			try {
				if (netcdfFileWriter != null) netcdfFileWriter.close();
			} catch (IOException e) {
				Results.putMessage("Unable to close netcdf cf file due to " + e.getMessage());
			}
		}
	}

	private Array getData2D(ITreeVector dataVector, ArrayList<Dimension> dimensions) {
		int firstDimensionLength = dimensions.get(0).getLength();
		int secondDimensionLength = dimensions.get(1).getLength();
		ArrayDouble.D3 arrayDoubleD3 = new ArrayDouble.D3(1, firstDimensionLength, secondDimensionLength);
		for (int j = 0; j < firstDimensionLength; j++) {
			for (int k = 0; k < secondDimensionLength; k++) {
				arrayDoubleD3.set(0, j, k, dataVector.getValue(k + j * secondDimensionLength));
			}
		}
		return arrayDoubleD3;
	}

	private Array getData1D(ITreeVector dataVector, ArrayList<Dimension> dimensions) {
		ArrayDouble.D2 arrayDoubleD2 = new ArrayDouble.D2(1, dimensions.get(0).getLength());
		for (int j = 0; j < dataVector.getSize(); j++) {
			arrayDoubleD2.set(0, j, dataVector.getValue(j));
		}
		return arrayDoubleD2;
	}

	private void writeObservationOffsetData(double[] observationOffsetsInDays, NetcdfFileWriter netcdfFileWriter, Variable observationOffsetVariable) throws IOException, InvalidRangeException {
		ArrayDouble.D1 observationOffsetArray = new ArrayDouble.D1(observationOffsetsInDays.length);
		for (int i = 0; i < observationOffsetsInDays.length; i++) {
			observationOffsetArray.set(i, observationOffsetsInDays[i]);
		}
		netcdfFileWriter.write(observationOffsetVariable, observationOffsetArray);
	}

	private void writeHK(double[][] hk, NetcdfFileWriter netcdfFileWriter, Variable hkVariable) throws IOException, InvalidRangeException {
		ArrayDouble.D2 hkArray = new ArrayDouble.D2(hk.length, hk.length);
		for (int i = 0; i < hk.length; i++) {
			for (int j = 0; j < hk.length; j++) {
				hkArray.set(i, j, hk[i][j]);
			}
		}
		netcdfFileWriter.write(hkVariable, hkArray);
	}

	private void writeStationData(String[] observationIds, NetcdfFileWriter netcdfFileWriter, Variable stationVariable) throws IOException, InvalidRangeException {
		ArrayChar.D2 stationArray = new ArrayChar.D2(observationIds.length, STATION_ID_CHAR_LENGTH);
		for (int i = 0; i < observationIds.length; i++) {
			String observationId = observationIds[i];
			stationArray.setString(i, observationId);
		}
		netcdfFileWriter.write(stationVariable, stationArray);
	}

	private void writeTimeStampData(NetcdfFileWriter netcdfFileWriter, Variable timeStampVariable) throws IOException, InvalidRangeException {
		ArrayDouble.D1 timeStampArray = new ArrayDouble.D1(1);
		timeStampArray.setDouble(0, timeStampAsMJD);
		netcdfFileWriter.write(timeStampVariable, timeStampArray);
	}

	private KalmanGainVariableData[] getKalmanGainVariableData(IVector[] kalmanGainColumns, NetcdfFileWriter netcdfFileWriter, Dimension stationDimension) {
		TreeVector kalmanGainColumn = (TreeVector) kalmanGainColumns[0];
		ArrayList<String> subTreeVectorIds = kalmanGainColumn.getSubTreeVectorIds();
		KalmanGainVariableData[] kalmanGainVariableData = new KalmanGainVariableData[subTreeVectorIds.size()];
		for (int subTreeIndex = 0; subTreeIndex < subTreeVectorIds.size(); subTreeIndex++) {
			ArrayList<String> vectorIds = new ArrayList<>();
			String variableId = null;
			String subTreeVectorId = subTreeVectorIds.get(subTreeIndex);
			vectorIds.add(subTreeVectorId);
			ITreeVector subTreeVector = kalmanGainColumn.getSubTreeVector(subTreeIndex);
			IDimensionIndex[] dimensionIndices = subTreeVector.getDimensionIndices();
			int[] shape = null;
			ITreeVector dataVector = null;
			if (dimensionIndices != null) {
				// 1D data
				shape = new int[dimensionIndices.length];
				for (int i = 0; i < dimensionIndices.length; i++) {
					shape[i] = dimensionIndices[i].getSize();
				}
				dataVector = subTreeVector;
				variableId = subTreeVectorId;
			} else {
				// 2D data
				ArrayList<String> subSubTreeVectorIds = subTreeVector.getSubTreeVectorIds();
				for (String subSubTreeVectorId : subSubTreeVectorIds) {
					ITreeVector subSubTreeVector = subTreeVector.getSubTreeVector(subSubTreeVectorId);
					IDimensionIndex[] subDimensionIndices = subSubTreeVector.getDimensionIndices();
					if (subDimensionIndices != null) {
						shape = new int[subDimensionIndices.length];
						for (int i = 0; i < subDimensionIndices.length; i++) {
							shape[i] = subDimensionIndices[i].getSize();
						}
						dataVector = subSubTreeVector;
						vectorIds.add(subSubTreeVectorId);
						variableId = subSubTreeVectorId;
						break;
					} else {
						throw new RuntimeException("Shape not supported for writing kalman gain to netcdf_cf for vector " + subSubTreeVectorId);
					}
				}
			}
			assert shape != null;
			kalmanGainVariableData[subTreeIndex] = createKalmanGainVariableData(netcdfFileWriter, stationDimension, vectorIds, variableId, shape, dataVector);
		}
		return kalmanGainVariableData;
	}

	private KalmanGainVariableData createKalmanGainVariableData(NetcdfFileWriter netcdfFileWriter, Dimension stationDimension, ArrayList<String> vectorIds, String variableId, int[] shape, ITreeVector dataVector) {
		ArrayList<Dimension> dimensions = new ArrayList<>();
		dimensions.add(stationDimension);
		for (int i = 0; i < shape.length; i++) {
			Dimension dim = netcdfFileWriter.addDimension(null, variableId + "_dimension_" + i, shape[i]);
			dimensions.add(dim);
		}
		Variable variable = netcdfFileWriter.addVariable(null, variableId, DataType.DOUBLE, dimensions);
		if (vectorIds.size() > 1) {
			String parentVectorId = vectorIds.get(0);
			variable.addAttribute(new Attribute(PARENT_VECTOR_ID, parentVectorId));
		}
		variable.addAttribute(KALMAN_GAIN_LONG_NAME_ATT);
		variable.addAttribute(FRACTIONS_UNIT_ATT);
		// Remove station dimension again because stations data are written one by one
		Dimension removed = dimensions.remove(0);
		assert removed.getShortName().equals(STATION_DIMENSION);
		return new KalmanGainVariableData(variableId, dimensions, variable, dataVector, vectorIds);
	}

	private Variable createStationVariable(NetcdfFileWriter netcdfFileWriter) {
		Variable stationVariable = netcdfFileWriter.addVariable(null, STATION_ID, DataType.CHAR, STATION_DIMENSION + ' ' + CHAR_LENGTH_ID);
		stationVariable.addAttribute(STATION_IDENTIFICATION_CODE_ATT);
		stationVariable.addAttribute(TIME_SERIES_ID_ATT);
		return stationVariable;
	}

	private Variable createObservationOffsetVariable(NetcdfFileWriter netcdfFileWriter) {
		Variable observationOffsetVariable = netcdfFileWriter.addVariable(null, OBSERVATION_OFFSET, DataType.DOUBLE, STATION_DIMENSION);
		observationOffsetVariable.addAttribute(OBSERVATION_OFFSET_LONG_NAME_ATT);
		observationOffsetVariable.addAttribute(TIME_STAMP_UNIT_ATT);
		return observationOffsetVariable;
	}

	private Variable createHKVariable(NetcdfFileWriter netcdfFileWriter) {
		Variable observationOffsetVariable = netcdfFileWriter.addVariable(null, "HK", DataType.DOUBLE, STATION_DIMENSION + ' ' + STATION_DIMENSION);
		observationOffsetVariable.addAttribute(HK_LONG_NAME_ATT);
		observationOffsetVariable.addAttribute(FRACTIONS_UNIT_ATT);
		return observationOffsetVariable;
	}

	private Variable createTimeStampVariable(NetcdfFileWriter netcdfFileWriter) {
		Dimension timeStampDimension = netcdfFileWriter.addDimension(null, "time_stamp_dimension", 1);
		ArrayList<Dimension> dimensionList = new ArrayList<>();
		dimensionList.add(timeStampDimension);
		Variable timeStampVariable = netcdfFileWriter.addVariable(null, TIME_STAMP, DataType.DOUBLE, dimensionList);
		timeStampVariable.addAttribute(TIME_STAMP_LONG_NAME_ATT);
		timeStampVariable.addAttribute(TIME_STAMP_UNIT_ATT);
		return timeStampVariable;
	}

	private void addGlobalAttributes(NetcdfFileWriter netcdfFileWriter) {
		netcdfFileWriter.addGroupAttribute(null, new Attribute("title", "Kalman gain data"));
		NetcdfUtils.addGeneralGlobalAttributes(netcdfFileWriter);
		if (comment != null && !comment.isEmpty()) netcdfFileWriter.addGroupAttribute(null, new Attribute("Comment", comment));
	}

	private ITreeVector getDataVector(ITreeVector kalmanGainColumnForObservation, KalmanGainVariableData kalmanGainVariableDatum) {
		String variableId = kalmanGainVariableDatum.getVariableId();
		boolean contains = kalmanGainColumnForObservation.getSubTreeVectorIds().contains(variableId);
		if (contains) return kalmanGainColumnForObservation.getSubTreeVector(variableId);
		ArrayList<String> vectorIds = kalmanGainVariableDatum.getVectorIds();
		String parentVectorId = vectorIds.get(0);

		TreeVector treeVector = (TreeVector) kalmanGainColumnForObservation;
		for (int i = 0; i < treeVector.getSubTreeVectorIds().size() ; i++) {
			ITreeVector subTreeVector = treeVector.getSubTreeVector(i);
			if (!parentVectorId.equals(subTreeVector.getId())) continue;
			if (subTreeVector.getSubTreeVectorIds().contains(variableId)) return subTreeVector.getSubTreeVector(variableId);
		}
		throw new IllegalStateException("Program error: Cannot find variable " + variableId + " in Kalman Gain Storage.");
	}

	/**
	 * Get comment string that was read from the XML file.
	 * @return The comment string
	 */
	public String getComment() {
		return comment;
	}

	/**
	 * Get the observation identifiers that were read when the kalman gain storage object was created.
	 * @return The observation identifiers
	 */
	public String[] getObservationIds() {
		return this.observationIds;
	}

	/**
	 * Get the time offset of the observations.
	 * @return The time offsets
	 */
	public double[] getObservationOffsetInDays() {
		return this.observationOffsetInDays;
	}

	public IVector[] getKalmanGainColumns() {
		if (this.kalmanGainColumns == null) {
			// the values were not in the file
		}
		return this.kalmanGainColumns;
	}

	public double[][] getHk() {
		return hk;
	}

	public void setHk(double[][] hk) {
		this.hk = hk;
	}

	/**
	 * Read the kalman gain matrix from xml file and, in case of large column vectors, to netdcf files.
	 */

    public void readKalmanGain(){
        this.readKalmanGain(null);
    }

    public void readKalmanGain(IVector templateTreeVector) {

		// read general info
		File directoryForStorage = determineStorageDirectory(true);

		if (this.gainFileType == StorageType.netcdf_cf) {
			readKalmanGainFromNetcdfCF(directoryForStorage);
			return;
		}

		File kgStorageXmlFile = new File(directoryForStorage, kalmanGainStorageFileName);
		if (!kgStorageXmlFile.exists()) {
			throw new RuntimeException("Kalman Gain Storage XML file not found: "
					+ kgStorageXmlFile.getAbsolutePath());
		}

		KalmanGainStorageXML kgStorageXML = (KalmanGainStorageXML) CastorUtils.parse(kgStorageXmlFile, KalmanGainStorageXML.class);
		comment = kgStorageXML.getComment();
		timeStampAsMJD = kgStorageXML.getTimeStampAsMJD();
		if (kgStorageXML.hasStateSize()) {
			stateSize = kgStorageXML.getStateSize();
		}

		// read observations
		KalmanGainObservationsXML observationsXML = kgStorageXML.getObservations();
		observationIds = new String[observationsXML.getObservationCount()];
		observationOffsetInDays = new double[observationsXML.getObservationCount()];
		kalmanGainColumns = new IVector[observationsXML.getObservationCount()];

		for (int i = 0; i < observationIds.length; i++) {
			KalmanGainObservationXML observationXML = observationsXML.getObservation(i);

			// read id and time
			observationIds[i] = observationXML.getId();
			observationOffsetInDays[i] = observationXML.getTimeOffsetInDays();

			// Get the values in the xml files if the vectors are small, otherwise read netCdf-file
			KalmanGainObservationXMLChoice observationXMLChoice = observationXML.getKalmanGainObservationXMLChoice();
			if (observationXMLChoice.getTreeVector() != null) {
				kalmanGainColumns[i] = TreeVectorReader.parseTreeVector(null, observationXMLChoice.getTreeVector());
			} else if (observationXMLChoice.getVector() != null) {
				kalmanGainColumns[i] = new Vector(TreeVectorReader.parseValuesFromSpaceSeparatedString(
						observationXMLChoice.getVector()));
			} else {
				// netCdf-file
				if (stateSize == -1) {
					throw new RuntimeException("Kalman gain can not be read from NetCDF yet if state size is not known");
				}
				String netcdfFileName = new File(directoryForStorage, observationXMLChoice.getFileName()).getAbsolutePath();
				kalmanGainColumns[i] = readKalmanGainColumnFromNetCdfFile(netcdfFileName, stateSize, templateTreeVector);
			}
		}
	}

	private void readKalmanGainFromNetcdfCF(File directoryForStorage) {
		NetcdfFile netcdfFile;
		try {
			File file = new File(directoryForStorage, kalmanGainStorageFileName);
			netcdfFile = NetcdfDataset.openDataset(file.getAbsolutePath());
			List<Variable> variables = netcdfFile.getVariables();
			Variable stationVariable = netcdfFile.findVariable(STATION_ID);
			int stationLength = stationVariable.getShape(0);
			TreeVector[] kalmanGainColumns = new TreeVector[stationLength];
			for (int i = 0; i < stationLength; i++) {
				kalmanGainColumns[i] = new TreeVector("state", "State From Black Box Stoch Model Instance");
			}
			readVariables(variables, stationLength, kalmanGainColumns);
			this.kalmanGainColumns = kalmanGainColumns;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	private void readVariables(List<Variable> variables, int stationLength, TreeVector[] kalmanGainColumns) throws IOException, InvalidRangeException {
		for (Variable variable : variables) {

			String shortName = variable.getShortName();
			if (STATION_ID.equals(shortName)) {
				this.observationIds = variable.getDataType() == DataType.STRING ? read1DimensionalStringArray(variable) : read2DCharVariable(variable);
				continue;
			}
			if (OBSERVATION_OFFSET.equals(shortName)) {
				this.observationOffsetInDays = (double[]) variable.read().get1DJavaArray(double.class);
				continue;
			}
			if (TIME_STAMP.equals(shortName)) {
				this.timeStampAsMJD = ((double[]) variable.read().get1DJavaArray(double.class))[0];
				continue;
			}
			if ("HK".equals(shortName)) {
				this.hk = (double[][]) variable.read().copyToNDJavaArray();
				continue;
			}
			int[] shape = variable.getShape();
			int[] shapeForRead = variable.getShape();
			shapeForRead[0] = 1;
			int[] originForRead = new int[shape.length];

			for (int i = 0; i < stationLength; i++) {
				originForRead[0] = i;
				if (shape.length == 2) {
					read1DimensionalArray(variable, shortName, shape, shapeForRead, originForRead, kalmanGainColumns[i]);
					continue;
				}
				if (shape.length == 3) {
					read2DimensionalArray(variable, shortName, shape, shapeForRead, originForRead, kalmanGainColumns[i]);
				}
			}
		}
	}

	private String[] read1DimensionalStringArray(Variable variable) throws IOException {
		Object[] array = (Object[]) variable.read().get1DJavaArray(String.class);
		String[] strings = new String[array.length];
		for (int i = 0; i < array.length; i++) {
			strings[i] = (String) array[i];
		}
		return strings;
	}

	static String[] read2DCharVariable(Variable variable) throws IOException {
		if (variable.getDimensions().size() != 2) {
			throw new IllegalArgumentException("variable must have 2 dimensions.");
		}

		Array read = variable.read();
		char[] chars = (char[]) read.copyTo1DJavaArray();
		int stringCount = variable.getDimension(0).getLength();
		int charCount = variable.getDimension(1).getLength();

		//convert chars to Strings.
		String[] strings = new String[stringCount];
		for (int n = 0; n < stringCount; n++) {
			StringBuilder builder = new StringBuilder(charCount);
			for (int k = 0; k < charCount; k++) {
				builder.append(chars[n * charCount + k]);
			}
			strings[n] = builder.toString().trim();
		}
		return strings;
	}

	private void read2DimensionalArray(Variable variable, String shortName, int[] shape, int[] shapeForRead, int[] originForRead, TreeVector kalmanGainColumns) throws IOException, InvalidRangeException {
		Array read = variable.read(originForRead, shapeForRead);
		double[] doubleArray = (double[]) read.get1DJavaArray(Double.TYPE);
		DimensionIndex[] dimensionIndices = {new DimensionIndex(shape[1]), new DimensionIndex(shape[2])};
		Vector doubleVector = new Vector(doubleArray);
		Attribute parentVectorIdAttribute = variable.findAttribute(PARENT_VECTOR_ID);
		TreeVector parentVector = new TreeVector(parentVectorIdAttribute.getStringValue());
		TreeVector vector = new TreeVector(shortName, doubleVector, dimensionIndices);
		parentVector.addChild(vector);
		kalmanGainColumns.addChild(parentVector);
	}

	private void read1DimensionalArray(Variable variable, String shortName, int[] shape, int[] shapeForRead, int[] originForRead, TreeVector kalmanGainColumns) throws IOException, InvalidRangeException {
		Array read = variable.read(originForRead, shapeForRead);
		double[] doubleArray = (double[]) read.get1DJavaArray(Double.TYPE);
		DimensionIndex dimensionIndex = new DimensionIndex(shape[1]);
		Vector doubleVector = new Vector(doubleArray);
		TreeVector vector = new TreeVector(shortName, doubleVector, new DimensionIndex[]{dimensionIndex});
		kalmanGainColumns.addChild(vector);
	}

	private void writeKalmanGainColumnToNetCdfFile(String netcdfFileName, IVector kalmanGainColumn) {
		NetcdfResultWriterNative.writeToNetCdf(netcdfFileName, kalmanGainColumn);
	}

    private IVector readKalmanGainColumnFromNetCdfFile(String netcdfFileName, int columnSize, IVector templateTreeVector) {
        if (templateTreeVector != null && templateTreeVector instanceof CtaTreeVector){
            CtaTreeVector kalmanGainColumn = (CtaTreeVector) templateTreeVector.clone();
            kalmanGainColumn.TVimport(netcdfFileName);
            return  kalmanGainColumn;
        } else {
            CtaVector ctaVector = new CtaVector(columnSize);
            CtaTreeVector kalmanGainColumn = new CtaTreeVector("Kalman gain vector", "KGvec", ctaVector);
            kalmanGainColumn.Vimport(netcdfFileName);
            return  kalmanGainColumn;
        }

    }

	private File determineStorageDirectory(boolean neededForReading) {
		String timeStampString;
		File storageDirectory;
		if (this.useTimeStampInDirectoryName) {
			timeStampString = TimeUtils.mjdToString(this.timeStampAsMJD);
			storageDirectory = new File(workingDir, storageDirPrefix + timeStampString);
		} else {
			storageDirectory = workingDir;
		}
		if (neededForReading) {
			if (!storageDirectory.exists()) {
				throw new RuntimeException("Kalman Gain Storage directory not found: "
						+ storageDirectory.getAbsolutePath());
			}
		} else {
			if (storageDirectory.exists()) {
				if (!storageDirectory.isDirectory()) {
					throw new RuntimeException("Storage dir. " + storageDirectory.getAbsolutePath()
							+ " already exists as a file.");
				} else {
					BBUtils.deleteDirectory(storageDirectory);
				}
			}
			storageDirectory.mkdir();
		}
		return storageDirectory;
	}

	/*
	 * fields that will become obsolete
	 */
	private int storageType = 0;
	private int nStats = 0;
	public static final int storeTxt = 0;
	public static final int storeNetcdf = 1;
	private File timeStepDir = null;

	/**
	 * This constructor will become obsolete
	 * @param workingDir The algorithm's working dir
	 * @param configString "netcdf" or "ascii"
	 * @param timeStep The algorithm's current time stamp
	 * @param obsIds The observation ids
	 * @deprecated
	 */
	public KalmanGainStorage(File workingDir, String configString, int timeStep, String[] obsIds) {
		this.observationIds = obsIds;
		this.nStats = obsIds.length;
		if (configString.equalsIgnoreCase("ascii")) {
			this.storageType = storeTxt;
		} else if (configString.equalsIgnoreCase("netcdf")) {
			this.storageType = storeNetcdf;
		} else {
			throw new RuntimeException(
					"KalmanGainStorage: unknown storage type: " + configString);
		}
		String dirName = new StringBuilder().append("tstep_").append(timeStep).toString();
		this.timeStepDir = new File(workingDir, dirName);
		if (!this.timeStepDir.isDirectory()) {
			if (this.timeStepDir.exists()) {
				BBUtils.deleteDirectory(this.timeStepDir);
			}
			this.timeStepDir.mkdir();
		}
	}


	/**
	 * This read method will become obsolete
	 * @param usrObsId observation id ("all" means all)
	 * @param Kmat Matrix to be filled
	 * @deprecated
	 */
	public void readKGain(String usrObsId, Matrix Kmat) {
		stateSize = Kmat.getNumberOfRows();
		//	double[][] K = new double[stateSize][nStats];

		int[] stationList;

		if (usrObsId.equalsIgnoreCase("all")) {
			stationList = new int[nStats];
			for (int iStat = 0; iStat < nStats; iStat++) {
				stationList[iStat] = iStat;
			}
		} else {
			stationList = new int[1];
			for (int iStat = 0; iStat < nStats; iStat++) {
				if (observationIds[iStat].equalsIgnoreCase(usrObsId)) {
					stationList[0] = iStat;
				}
			}
		}

		for (int aStationList : stationList) {
			//double[] vals = new double[stateSize];

			if (storageType == storeTxt) {
				CsvReader csvReader;
				String sfilename = new StringBuilder().append("kg").append(observationIds[aStationList]).append(".txt").toString();

				try {
					csvReader = new CsvReader(new File(timeStepDir, sfilename));
					csvReader.setColumnSeparatorChar(' ');
					String[] s;
					int j = 0;
					while ((s = csvReader.readCSVLineTrimElements()) != null) {
						Kmat.setValue(j, aStationList, Double.parseDouble(s[0]));
						j++;
					}
					csvReader.close();


				}
				catch (FileNotFoundException e) {
					throw new IllegalArgumentException("No file found");
				} catch (IOException e) {
					e.printStackTrace();
				}
			} else if (storageType == storeNetcdf) {
				String ncfilename = new StringBuilder().append("kg").append(observationIds[aStationList]).append(".nc").toString();
				String netcdfname = new File(timeStepDir, ncfilename).getAbsolutePath();
				CtaVector ctavec = new CtaVector(stateSize);
				CtaTreeVector nckalman = new CtaTreeVector("Kalman gain vector", "KGvec", ctavec);
				nckalman.Vimport(netcdfname);
				for (int j = 0; j < stateSize; j++) {
					Kmat.setValue(j, aStationList, nckalman.getValue(j));
				}
			}
		}
	}

	/**
	 * This write method will become obsolete
	 * @param usrObsId observation id ("all" means all)
	 * @param Kmat Matrix to be written
	 * @deprecated
	 */
	public void writeKGain(String usrObsId, Matrix Kmat) {
		double[][] K = Kmat.asArray();
		stateSize = K.length / nStats;
		int[] stationList;


		if (usrObsId.equalsIgnoreCase("all")) {
			stationList = new int[nStats];
			for (int iStat = 0; iStat < nStats; iStat++) {
				stationList[iStat] = iStat;
			}
		} else {
			stationList = new int[1];
			for (int iStat = 0; iStat < nStats; iStat++) {
				if (observationIds[iStat].equalsIgnoreCase(usrObsId)) {
					stationList[0] = iStat;
				}
			}
		}
		for (int aStationList : stationList) {
			double[] vals = new double[stateSize];
			for (int j = 0; j < stateSize; j++) {
				vals[j] = K[j][aStationList];
			}
			if (storageType == storeTxt) {
				String sfilename = new StringBuilder().append("kg").append(observationIds[aStationList]).append(".txt").toString();
				BufferedOutputStream outputStream;
				try {
					outputStream = new BufferedOutputStream(new FileOutputStream(new File(timeStepDir, sfilename)));
					PrintStream printStream;
					printStream = new PrintStream(outputStream);
					for (int j = 0; j < stateSize; j++) {
						printStream.println(vals[j]);
					}
					printStream.close();
				} catch (FileNotFoundException e) {
					throw new IllegalArgumentException("No file found");
				}
			} else if (storageType == storeNetcdf) {
				String ncfilename = new StringBuilder().append("kg").append(observationIds[aStationList]).append(".nc").toString();
				IResultWriter netcdfWriter = new NetcdfResultWriterNative(timeStepDir, ncfilename);
				Results.reset();
				Results.addResultWriter(netcdfWriter);
				IVector vecKG = new CtaVector(stateSize);
				vecKG.setValues(vals);
				CtaTreeVector tvKG = new CtaTreeVector("KGvec", "KGvec", vecKG);
                Results.putValue("KGvec", tvKG, tvKG.getSize(), "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.Instance);
				netcdfWriter.free();
				Results.reset();
			}
		}
	}

	public double getTimeStampAsMjd(){
		return timeStampAsMJD;
	}

	private class KalmanGainVariableData {
		private final String variableId;
		private final ArrayList<Dimension> dimensions;
		private final Variable variable;
		private final ITreeVector dataVector;
		private ArrayList<String> vectorIds;

		public KalmanGainVariableData(String variableId, ArrayList<Dimension> dimensions, Variable variable, ITreeVector dataVector, ArrayList<String> vectorIds) {
			this.variableId = variableId;
			this.dimensions = dimensions;
			this.variable = variable;
			this.dataVector = dataVector;
			this.vectorIds = vectorIds;
		}

		public String getVariableId() {
			return variableId;
		}

		public ArrayList<Dimension> getDimensions() {
			return dimensions;
		}

		public Variable getVariable() {
			return variable;
		}

		public ITreeVector getDataVector() {
			return dataVector;
		}

		public ArrayList<String> getVectorIds() {
			return vectorIds;
		}
	}
}
