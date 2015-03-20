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

package org.openda.blackbox.wrapper;

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.ITime;
import org.openda.uncertainties.pdfs.NormalDistribution;
import org.openda.utils.DistributedCounter;
import org.openda.utils.Time;

import java.io.*;

/**
 * ARMA Noise Model
 */
public class ArmaNoiseModel {

    private static DistributedCounter instanceCount = new DistributedCounter(0);

    private int stateVectorSize = Integer.MIN_VALUE;
    private double[] armaConstants = null;
    private boolean useRandomSeed = false;

    NormalDistribution normalDistribution = null;

    private int instanceNumber = Integer.MIN_VALUE;
    private int realizationCounter = 0;

    // state vector for each previous arma step,
    // and 1 for the current state (index 0)
    private double[][] noiseStateVector = null;

    private ITime currentTime = null;
    File logFile = null;
    BufferedWriter logFileWriter = null;

	public ArmaNoiseModel(File workingDir,
                          int stateVectorSize,
                          double[] armaConstants,
                          double stdDev,
                          boolean useRandomSeed,
                          boolean doLogging) {
        this(workingDir, stateVectorSize, armaConstants, doLogging);
        this.useRandomSeed = useRandomSeed;

        double mean = 0d;
        int stddvIsFactor = 0;
        normalDistribution = new NormalDistribution(mean, stdDev, stddvIsFactor);
    }

    public ArmaNoiseModel(File workingDir,
                          int stateVectorSize,
                          double[] armaConstants,
                          boolean doLogging) {

        // store state vector size and arma constants
        this.stateVectorSize = stateVectorSize;
        if (armaConstants != null) {
            this.armaConstants = armaConstants;
        } else {
            this.armaConstants = new double[0];
        }

        // create a state vector for each previous arma step, and 1 for the current state
        this.noiseStateVector = new double[this.armaConstants.length + 1][];
        for (int i = 0; i < this.armaConstants.length + 1; i++) {
            noiseStateVector[i] = new double[this.stateVectorSize];
        }

        if (workingDir != null && doLogging) {
            FileWriter logFileWriter;
            try {
                // create log file
                File nonExistingLogFile = new File(workingDir, "ArmaNoiseModel-log.txt");
                if (nonExistingLogFile.exists()) {
                    for (int i = 1; i < 100 && nonExistingLogFile.exists(); i++) {
                        nonExistingLogFile = new File(workingDir, "ArmaNoiseModel-log-" + i + ".txt");
                    }
                }
                logFile = nonExistingLogFile;
                logFileWriter = new FileWriter(logFile);
                this.logFileWriter = new BufferedWriter(logFileWriter);
                this.logFileWriter.flush();
            } catch (IOException e) {
                throw new RuntimeException(this.getClass().getName() + ": Error creating log file " +
                        logFile.getAbsolutePath() + ": " + e.getMessage());
            }
        }
		instanceCount.inc();
        instanceNumber = instanceCount.val();
    }


	public static void resetInstanceCounter() {
		instanceCount = new DistributedCounter(0);
	}

	public void compute(ITime fromTime, ITime targetTime) {

        checkTimeStamp(fromTime, "updateNoise");

        // move (t+1)-values to t-values, t-values to (t-1)-values, etc.
        for (int i = this.armaConstants.length; i >= 1; i--) {
            noiseStateVector[i] = noiseStateVector[i - 1];
        }

        // compute the new value at t+1
        double[] tPlusOneValues = new double[this.stateVectorSize];
        for (int prevStep = 0; prevStep < this.armaConstants.length; prevStep++) {
            for (int j = 0; j < this.stateVectorSize; j++) {
                tPlusOneValues[j] += armaConstants[prevStep] * noiseStateVector[prevStep + 1][j];
            }
        }
        noiseStateVector[0] = tPlusOneValues;

        try {
            logStateVector("NoiseStateVector after compute " + fromTime, noiseStateVector[0]);
        } catch (IOException e) {
            createWriteToLogFileException(e);
        }
        currentTime = targetTime;
    }

    public void updateNoise(ITime timeStamp) {

        checkTimeStamp(timeStamp, "updateNoise");

        double[] noise = new double[this.stateVectorSize];
        for (int i = 0; i < this.stateVectorSize; i++) {
            noise[i] = normalDistribution.getRealization(determineNextSeed());
        }

        try {
            logStateVector("Update noise for time stamp " + timeStamp, noise);
        } catch (IOException e) {
            createWriteToLogFileException(e);
        }

        // add noise to last computed NoiseModel statevector
        for (int i = 0; i < this.stateVectorSize; i++) {
            noiseStateVector[0][i] += noise[i];
        }
        try {
            logStateVector("NoiseStateVector after noise " + timeStamp, noiseStateVector[0]);
        } catch (IOException e) {
            createWriteToLogFileException(e);
        }
    }

    public double[] getNoiseStateVector(ITime timeStamp) {

        checkTimeStamp(timeStamp, "getNoiseStateVector");

        double[] stateVector = new double[stateVectorSize];
        System.arraycopy(this.noiseStateVector[0], 0, stateVector, 0, stateVectorSize);
        return stateVector;
    }

    public void setNoiseStateVector(ITime timeStamp, double[] stateVector) {

        checkTimeStamp(timeStamp, "setNoiseStateVector");

        System.arraycopy(stateVector, 0, this.noiseStateVector[0], 0, stateVectorSize);
        try {
            logStateVector("NoiseStateVector set " + timeStamp, noiseStateVector[0]);
        } catch (IOException e) {
            createWriteToLogFileException(e);
        }
    }

    public void addWhiteNoise(ITime timeStamp, double[] whiteNoise) {

        checkTimeStamp(timeStamp, "addWhiteNoise");

        int noiseStateIndex = noiseStateVector.length - 1;
        System.arraycopy(whiteNoise, 0, this.noiseStateVector[noiseStateIndex], 0, stateVectorSize);
        try {
            logStateVector("White noise added " + timeStamp, noiseStateVector[noiseStateIndex]);
        } catch (IOException e) {
            createWriteToLogFileException(e);
        }
    }

    private void logStateVector(String message, double[] stateVector) throws IOException {
        if (logFileWriter != null) {
            try {
                logFileWriter.write(message + ":");
                logFileWriter.newLine();
                for (double aStateVector : stateVector) {
                    logFileWriter.write("" + aStateVector);
                    logFileWriter.newLine();
                }
                logFileWriter.flush();
            } catch (IOException e) {
                throw new IOException("Problem in writing log message to logStateVector file.:" + e.getMessage());
            }
        }
    }

    //
    // Functions to store and retrieve Noise Model State File
    //

    // Strings Noise Model State File

    private final String fileVersionText = "Version ";
    private final String fileVersion = "1.1";
    private final String saveVersionString =
            "nl.wldelft.da.tools.stochmodel.NoiseModel Saved State File, "
                    + fileVersionText + fileVersion;
    private final String dateTimeText = "TimeStamp: ";
    private final String saveHistoryCountString = "History Count: ";
    private final String saveStateVectorSizeString = "Values per time step Count: ";
    private final String saveArmaConstString = "ArmaConstant: ";
    private final String saveCurrentValuesString = "Current Values: ";
    private final String savePreviousValuesString = "Values for previous step: ";

    /**
     * Save the current state of the Noise Model to file.
     *
     * @param timeStamp Time stamp for which the state is valid.
     * @param stateFile File to write the state to.
     */
    public void saveState(ITime timeStamp, File stateFile) {

        if (currentTime != null) {
            if (timeStamp.getMJD() > currentTime.getMJD()) {
                // TODO: invoke save state inside sequence manager
//                throw new RuntimeException(this.getClass().getName() + ": Invalid timeStap (" +
//                        timeStamp.toString() + ") in saveState");
            }
        }

        try {

            BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(stateFile));

            bufferedWriter.write(saveVersionString);
            bufferedWriter.newLine();

            bufferedWriter.write(dateTimeText + timeStamp.toString());
            bufferedWriter.newLine();

            bufferedWriter.write(saveStateVectorSizeString + stateVectorSize);
            bufferedWriter.newLine();

            bufferedWriter.write(saveHistoryCountString + armaConstants.length);
            bufferedWriter.newLine();

            for (int i = 0; i < armaConstants.length; i++) {
                bufferedWriter.write(saveArmaConstString + (i + 1) + " " + armaConstants[i]);
                bufferedWriter.newLine();
            }

            bufferedWriter.write(saveCurrentValuesString);
            bufferedWriter.newLine();
            for (int v = 0; v < noiseStateVector[0].length; v++) {
                bufferedWriter.write(String.valueOf(noiseStateVector[0][v]));
                bufferedWriter.newLine();
            }

            for (int i = 0; i < armaConstants.length; i++) {
                int prevTimeStepIndicator = i + 1;
                bufferedWriter.write(savePreviousValuesString + prevTimeStepIndicator);
                bufferedWriter.newLine();
                for (int v = 0; v < noiseStateVector[prevTimeStepIndicator].length; v++) {
                    bufferedWriter.write(String.valueOf(noiseStateVector[prevTimeStepIndicator][v]));
                    bufferedWriter.newLine();
                }
            }

            bufferedWriter.close();

        } catch (IOException e) {
            throw new RuntimeException(this.getClass().getName() +
                    ": Error writing StateFile " + stateFile.getAbsolutePath() + ":" + e.getMessage());
        }
    }

    /**
     * Read the state of the Noise Model from file.
     *
     * @param stateFile File from which the state must be read.
     */
    public void loadState(File stateFile) {
        loadState(null, stateFile, false);
    }

    /**
     * Read the state of the Noise Model from file.
     *
     * @param timeStamp Time stamp for which the state must be read.
     * @param stateFile File from which the state must be read.
     */
    public void loadState(ITime timeStamp, File stateFile) {
        loadState(timeStamp, stateFile, true);
    }

    /**
     * Read the state of the Noise Model from file.
     *
     * @param timeStamp   Time stamp for which the state must be read.
     * @param stateFile   File from which the state must be read.
     * @param doTimeCheck Indicates if time stamp in file must be checked. If so, the time stamp in the file must
     *                    be equal to <code>timeStamp</code>) or must contain a <em>cold start</em> string.
     */
    public void loadState(ITime timeStamp, File stateFile, boolean doTimeCheck) {
        BufferedReader bufferedReader;
        try {
            bufferedReader = new BufferedReader(new FileReader(stateFile));
            String line = bufferedReader.readLine();
            String savedFileVersion = line.substring(line.indexOf(fileVersionText) + fileVersionText.length());
            if (!savedFileVersion.equals(fileVersion)) {
                throw new IllegalStateException("Unknown Delta Forces Model file version ("
                        + savedFileVersion + ") in file " + stateFile);
            }
            line = bufferedReader.readLine();
            String savedDateTime = line.substring(line.indexOf(":") + 2);
            if (doTimeCheck) {
                if (timeStamp == null) {
                    throw new RuntimeException("Requested time stamp == null, so could not validate time stamp in " +
                            stateFile.getAbsolutePath());
                } else {
                    if (!BBUtils.isColdStart(savedDateTime)) {
                        if (timeStamp != null && savedDateTime.equals(Time.timeStampToDate(timeStamp).toString())) {
                            throw new RuntimeException("Invalid time stamp (" +
                                    savedDateTime + " instead of " + timeStamp +
                                    ") in Delta Forces Model state in file " + stateFile);
                        }
                    }
                }
            }
            int savedStateVectorSize = parseCount(bufferedReader.readLine());
            int savedHistoryStepCount = parseCount(bufferedReader.readLine());

            int historyStepCount = armaConstants.length;

            if (savedStateVectorSize != stateVectorSize) {
                throw new RuntimeException(this.getClass().getName() +
                        ": Error reading state file: Invalid stateVectorSize, expected: " + stateVectorSize +
                        " (file " + stateFile.getAbsolutePath() + ")");
            }

            if (savedHistoryStepCount != historyStepCount) {
                throw new RuntimeException(this.getClass().getName() +
                        ": Error reading state file: Invalid historyStepCount, expected: " + historyStepCount +
                        " (file " + stateFile.getAbsolutePath() + ")");
            }

            for (int i = 0; i < armaConstants.length; i++) {
                line = bufferedReader.readLine();
                if (line == null) {
                    throw new RuntimeException(this.getClass().getName() +
                            ": Error reading state file: Not enough previous timeStep values (file " + stateFile.getAbsolutePath() + ")");
                }
                String[] subStrings = line.split(" ");
                if (Integer.valueOf(subStrings[1]) != i + 1) {
                    throw new RuntimeException(this.getClass().getName() +
                            ": Error reading state file: Invalid previous timeStep indicator, expected: " + i + 1 +
                            " (file " + stateFile.getAbsolutePath() + ")");
                }
                if (Double.compare(Double.valueOf(subStrings[2]), armaConstants[i]) != 0) {
                    throw new RuntimeException(this.getClass().getName() +
                            ": Error reading state file: Invalid arma Constant " + (i + 1) +
                            ", expected: " + armaConstants[i] +
                            " (file " + stateFile.getAbsolutePath() + ")");
                }
            }

            line = bufferedReader.readLine();
            if (!line.equals(saveCurrentValuesString)) {
                throw new RuntimeException(this.getClass().getName() +
                        ": Error reading state file: Unexpected line: " + line +
                        " (file " + stateFile.getAbsolutePath() + ")");
            }
            for (int v = 0; v < noiseStateVector[0].length; v++) {
                line = bufferedReader.readLine();
                noiseStateVector[0][v] = Double.valueOf(line);
            }

            for (int i = 0; i < armaConstants.length; i++) {
                int prevTimeStepIndicator = i + 1;
                line = bufferedReader.readLine();
                if (!line.equals(savePreviousValuesString + prevTimeStepIndicator)) {
                    throw new RuntimeException(this.getClass().getName() +
                            ": Error reading state file: Unexpected line: " + line +
                            " (file " + stateFile.getAbsolutePath() + ")");
                }
                for (int v = 0; v < noiseStateVector[prevTimeStepIndicator].length; v++) {
                    line = bufferedReader.readLine();
                    noiseStateVector[prevTimeStepIndicator][v] = Double.valueOf(line);
                }
            }


            bufferedReader.close();

        } catch (Exception e) {
            throw new RuntimeException(this.getClass().getName() +
                    ": Could not read noise state file " + stateFile.getAbsolutePath());
        }
    }

    private int parseCount(String line) {
        String countStartString = ": ";
        int countStartStringIndex = line.indexOf(countStartString);
        if (countStartStringIndex < 0) {
            throw new IllegalStateException("Cannot find \"" + countStartString + "\" in line " + line);
        }
        return Integer.valueOf(line.substring(countStartStringIndex + 2));
    }

    private void checkTimeStamp(ITime timeStamp, String methodName) {
        if (currentTime != null) {
            if (!epsilonCompare(timeStamp.getMJD(), currentTime.getMJD())) {
//                throw new RuntimeException(this.getClass().getName() + ": Invalid time stamp (" +
//                        Time.timeStampToDate(timeStamp).toString() +
//                        ") in " + methodName + " (expected: " + Time.timeStampToDate(currentTime).toString() + ")");
            }
        }
    }

    private boolean epsilonCompare(double d1, double d2) {
        final double epsilon = 1.e-7;
        if ((d1 + epsilon > d2) && (d2 + epsilon > d1)) {
            return true;
        }
        return false;
    }

    private void createWriteToLogFileException(IOException e) {
        throw new RuntimeException(this.getClass().getName() + ": Error writing to log file " +
                logFile.getAbsolutePath() + ": " + e.getMessage());
    }

    private long determineNextSeed() {
        long seed;
        if (useRandomSeed) {
            seed = ((Double) (Math.random() * 10000.0)).longValue();
        } else {
            seed = (long) this.instanceNumber * 100 + (long) (++this.realizationCounter);
        }
        return seed;
    }

    public int getNoiseStateVectorSize() {
        return stateVectorSize;
    }

    public int getNextRealizationCounter() {
        return ++this.realizationCounter;
    }

	public void finish() {
		if (logFileWriter != null) {
			try {
				logFileWriter.close();
			} catch (IOException e) {
				throw new RuntimeException(this.getClass().getName() +
						": Could not close log file " + logFile.getAbsolutePath());
			}
		}
	}
}
