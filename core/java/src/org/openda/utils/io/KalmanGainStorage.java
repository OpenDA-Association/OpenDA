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
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;
import org.openda.resultwriters.NetcdfResultWriterNative;
import org.openda.utils.Matrix;
import org.openda.utils.Results;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import java.io.*;
import java.util.HashMap;
import java.util.Set;

/**
 * Kalman gain storage object, used to write or read the kalman gain for a certain time stamp to or from file.
 */
public class KalmanGainStorage {

	public static int DefaultMaxKeepVectorInXMLSize = 40;

	// set in constructor
	private File workingDir;
	private double timeStampAsMJD = Double.MIN_VALUE;;

	// properties that be can be overridden before writing
	private String storageDirPrefix = "kgStorage_";
	private String columnFilePrefix = "obsColumn_";
	private String kalmanGainStorageXmlFileName = "kalmanGainStorage.xml";
	private int maxKeepVectorInXMLSize = DefaultMaxKeepVectorInXMLSize;
	private boolean useTimeStampInDirectoryName = true;

	public enum StorageType {
		xml, netcdf, automatic
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
	 * @param kalmanGainStorageXmlFileName The file name
	 */
	public void setKalmanGainStorageXmlFileName(String kalmanGainStorageXmlFileName) {
		this.kalmanGainStorageXmlFileName = kalmanGainStorageXmlFileName;
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
		writeKalmanGain(observationIds,observationOffsetsInDays,kalmanGainColumns);
	}	
	
	/**
	 * Write the kalman gain matrix to xml file and, in case of large column vectors, to netdcf files.
	 * @param observationIds The observation identifiers
	 * @param observationOffsetsInDays The time offset of the observations, expressed in days,
	 *                                 relative to the time stamp for this kalman gain
	 *                                 (0 means: same time stamp as the gain,
	 *                                  negative means before the kalman gain time stamp)
	 * @param kalmanGainColumns The vectors to be written to the kalman gain column files.
	 */
	public void writeKalmanGain(String[] observationIds, double[] observationOffsetsInDays, IVector[] kalmanGainColumns) {
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

		// store general info
		KalmanGainStorageXML kgStorageXML = new KalmanGainStorageXML();
		kgStorageXML.setComment(comment);
		kgStorageXML.setTimeStampAsMJD(timeStampAsMJD);
		kgStorageXML.setTimeStampAsDateTime(Time.timeStampToDate(timeStampAsMJD));

		// store observations
		File directoryForStorage = determineStorageDirectory(false);
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
		File kgStorageXmlFile = new File(directoryForStorage, kalmanGainStorageXmlFileName);
		CastorUtils.write(kgStorageXML, kgStorageXmlFile, "opendaKalmanGainStorage", null, null);
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

	/**
	 * Read the kalman gain matrix from xml file and, in case of large column vectors, to netdcf files.
	 */

    public void readKalmanGain(){
        this.readKalmanGain(null);
    }

    public void readKalmanGain(IVector templateTreeVector) {

		// read general info
		File directoryForStorage = determineStorageDirectory(true);
		File kgStorageXmlFile = new File(directoryForStorage, kalmanGainStorageXmlFileName);
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
}
