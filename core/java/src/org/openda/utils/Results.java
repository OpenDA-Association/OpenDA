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

package org.openda.utils;

import org.openda.interfaces.IAlgorithm;
import org.openda.interfaces.IInstance;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.IVector;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Results Module
 */
public class Results {

    private static List<IResultWriter> resultWriters = new ArrayList<IResultWriter>();
    private static List<ConfiguredResultWriter> configuredResultWriters = new ArrayList<ConfiguredResultWriter>();

    private static IAlgorithm.Type algorithmType = IAlgorithm.Type.Unknown;

    private static int currentStep = 0;
    private static int currentOuterIter = 0;
    private static int currentInnerIter = 0;
    private static int currentInstance = 0;
    private static boolean runningInGui = false;
    private static File registerFile = null;
    private static File workingDir = null;
    private static boolean delete = true;
    private static String lastMessage = "";

    public static void reset() {
    	free();
        resultWriters = new ArrayList<IResultWriter>();
        configuredResultWriters = new ArrayList<ConfiguredResultWriter>();
        currentStep = 0;
        currentOuterIter = 0;
        currentInnerIter = 0;
        currentInstance = 0;
        runningInGui = false;
    }

    public static void setRunningInGui(boolean inGui) {
        runningInGui = inGui;
    }

    public static void setAlgorithmType(IAlgorithm.Type type) {
        algorithmType = type;
    }

    public static void addResultWriter(IResultWriter aResultWriter) {
        resultWriters.add(aResultWriter);
    }

    public static void addResultWriter(IResultWriter aResultWriter, ResultSelectionConfig resultSelectionConfig) {
		if (resultSelectionConfig == null) {
			addResultWriter(aResultWriter);
		} else {
			ConfiguredResultWriter configuredResultWriter = new ConfiguredResultWriter(aResultWriter, resultSelectionConfig);
			configuredResultWriters.add(configuredResultWriter);
		}
	}

    /**
     * Put a progression message.
     *
     * @param message the message to be put to the progression monitor
     */
    public static void putProgression(String message) {
        if (!runningInGui) {
            if (message.length() > 1) {
                System.out.println(message);
            } else {
                System.out.print(message);
            }
        }
        // Source of a progression message is always the algorithm
        IResultWriter.Source source = IResultWriter.Source.Algorithm;
        if (message.length() > 1) {
            for (ConfiguredResultWriter configuredResultWriter : configuredResultWriters) {
                if (configuredResultWriter.getResultWriterConfig().doWrite(source)) {
                    configuredResultWriter.getResultWriter().putMessage(source, message);
                }
            }
            for (IResultWriter resultWriter : resultWriters) {
                resultWriter.putMessage(source, message);
            }
        }
    }

    public static void putIterationReport(IInstance source, int iteration, double cost, IVector parameters) {

       for (ConfiguredResultWriter configuredResultWriter : configuredResultWriters) {
           if (source == null || configuredResultWriter.getResultWriterConfig().doWrite(source)) {
               configuredResultWriter.getResultWriter().putIterationReport(source, iteration, cost, parameters);
           }
       }
       for (IResultWriter resultWriter : resultWriters) {
           resultWriter.putIterationReport(source, iteration, cost, parameters);
       }
    }

    public static void putProgression(IInstance source, String message) {
        if (!runningInGui) {
            if (message.length() > 1) {
                System.out.println(message);
            } else {
                System.out.print(message);
            }
        }
        // Source of a progression message is always the algorithm
        if (message.length() > 1) {
            for (ConfiguredResultWriter configuredResultWriter : configuredResultWriters) {
                if (configuredResultWriter.getResultWriterConfig().doWrite(source)) {
                    configuredResultWriter.getResultWriter().putMessage(source, message);
                }
            }
            for (IResultWriter resultWriter : resultWriters) {
                resultWriter.putMessage(source, message);
            }
        }
    }


    /**
     * Put a message from the algorithm
     *
     * @param message The message to be put
     */
    public static void putMessage(String message) {
        // no result source specified, assume algorithm
        putMessage(IResultWriter.Source.Algorithm, message);
    }

    /**
     * Put a message.
     *
     * @param source  the producer of the message (algorithm, model or observer)
     * @param message the message to be put
     */
    public static void putMessage(IResultWriter.Source source, String message) {
    	lastMessage=message;
        for (ConfiguredResultWriter configuredResultWriter : configuredResultWriters) {
            if (configuredResultWriter.getResultWriterConfig().doWrite(source)) {
                configuredResultWriter.getResultWriter().putMessage(source, message);
            }
        }
        for (IResultWriter resultWriter : resultWriters) {
            resultWriter.putMessage(source, message);
        }
    }

    /**
     * Put a message.
     *
     * @param source  the producer of the message (algorithm, model or observer)
     * @param message the message to be put
     */
    public static void putMessage(IInstance source, String message) {
    	lastMessage=message;
        for (ConfiguredResultWriter configuredResultWriter : configuredResultWriters) {
            if (configuredResultWriter.getResultWriterConfig().doWrite(source)) {
                configuredResultWriter.getResultWriter().putMessage(source, message);
            }
        }
        for (IResultWriter resultWriter : resultWriters) {
            resultWriter.putMessage(source, message);
        }
    }


	public static void putValue(String id, Object result, int size, String context, IResultWriter.OutputLevel outputLevel,
								IResultWriter.MessageType type) {
		//TODO: remove source
		IResultWriter.Source source = IResultWriter.Source.Algorithm;
		for (ConfiguredResultWriter configuredResultWriter : configuredResultWriters) {
            int defaultMaxSize = configuredResultWriter.getResultWriter().getDefaultMaxSize();
			int writeCounter = configuredResultWriter.getResultWriterConfig().nextWriteCounter(id, size,
					defaultMaxSize, outputLevel, context, type);
			if (writeCounter >= 0) {
				configuredResultWriter.getResultWriter().putValue(source, id, result, outputLevel, context, writeCounter);
				checkCounter(type, writeCounter);
			}
		}
		//TODO: find out why the following is necessary. It seems that it is needed for the case when selection
		//is completely not specified in the input file.
		for (IResultWriter resultWriter : resultWriters) {
			resultWriter.putValue(source, id, result, outputLevel, context, determineCounter(type));
		}
	}

    public static boolean hasWriters() {
        return resultWriters.size() > 0;
    }

    public static void putProgression(IResultWriter.MessageType type) {

        String messageString = composeMessageString(type);

        for (ConfiguredResultWriter configuredResultWriter : configuredResultWriters) {
            if (configuredResultWriter.getResultWriterConfig().doWrite(IResultWriter.Source.Algorithm)) {
                configuredResultWriter.getResultWriter().putMessage(IResultWriter.Source.Algorithm, messageString);
            }
        }
        for (IResultWriter resultWriter : resultWriters) {
            resultWriter.putMessage(IResultWriter.Source.Algorithm, messageString);
        }
    }

    private static void checkCounter(IResultWriter.MessageType type, int counter) {
        switch (type) {
            case Step:
                if (counter > currentStep) currentStep = counter;
                break;
            case OuterIteration:
                if (counter > currentOuterIter) currentOuterIter = counter;
                break;
            case InnerIteration:
                if (counter > currentInnerIter) currentInnerIter = counter;
                break;
            case Instance:
                if (counter > currentInstance) currentInstance = counter;
                break;
        }
    }

    private static int determineCounter(IResultWriter.MessageType type) {
        int counter = 0;
        switch (type) {
            case Step:
                counter = currentStep;
                break;
            case OuterIteration:
                counter = currentOuterIter;
                break;
            case InnerIteration:
                counter = currentInnerIter;
                break;
            case Instance:
                counter = currentInstance;
                break;
        }
        return counter;
    }

    private static String composeMessageString(IResultWriter.MessageType type) {
        String messageString = "";
        switch (type) {
            case Step:
                messageString = "Step " + currentStep++;
                break;
            case OuterIteration:
                messageString = "Outer iter " + currentOuterIter++;
                break;
            case InnerIteration:
                messageString = "Inner iter " + currentInnerIter++;
                break;
            case Instance:
                messageString = "Instance " + currentInstance++;
                break;
        }

        if (algorithmType == IAlgorithm.Type.Filtering) {
            messageString = "";
            if (currentStep > 0) {
                messageString += "Step " + currentStep;
            }
            if (currentInstance > 0) {
                messageString += messageString.isEmpty() ? "" : ", ";
                messageString += "Outer iter " + currentInstance;
            }
        } else if (algorithmType == IAlgorithm.Type.Calibration) {
            messageString = "";
            if (currentOuterIter > 0) {
                messageString += "Inner iter " + currentOuterIter;
            }
            if (currentInnerIter > 0) {
                messageString += messageString.isEmpty() ? "" : ", ";
                messageString += "Inner iter " + currentInnerIter;
            }
        }
        return messageString;
    }

    /**
     * TODO: description
     */
    public static class ConfiguredResultWriter {
        private IResultWriter resultWriter;
        private ResultSelectionConfig resultSelectionConfig;

        public ConfiguredResultWriter(IResultWriter resultWriter, ResultSelectionConfig resultSelectionConfig) {
            this.resultWriter = resultWriter;
            this.resultSelectionConfig = resultSelectionConfig;
        }

        public IResultWriter getResultWriter() {
            return resultWriter;
        }

        public ResultSelectionConfig getResultWriterConfig() {
            return resultSelectionConfig;
        }
    }

    public static void setWorkingDir(File dir) {
        workingDir = dir;
    }

    public static File getWorkingDir() {
        return workingDir;
    }

    public static void registerInstanceX(File instance){
        if (registerFile == null) {
            registerFile = new File(workingDir, "register_cases");
        }
        if (delete) {
            delete = false;
            registerFile.delete();
            File presentationFile = new File(workingDir, "register_presentations");
            presentationFile.delete();
        }
        try {
            FileWriter writer = new FileWriter(registerFile, true);
 //           writer.write(RelativePath.getRelativePath(workingDir, instance) + "\n");
            writer.close();
        } catch (IOException e) {
            Results.putMessage("Error: " + e.getMessage());
        }
    }

    // TODO: remove
    public static void registerCostX(int index, double cost) {
        if (registerFile == null) {
            Results.putMessage("Error: registerCost called before registerInstance!");
            return; // This should not happen!
        }
        try {
            FileWriter writer = new FileWriter(registerFile, true);
            writer.write("Iteration " + Integer.toString(index) +
                ": cost " + "\n");
            //": cost " + PrintNumber.printNumberExtended(cost) + "\n");
            writer.close();
        } catch (IOException e) {
            Results.putMessage("Error: " + e.getMessage());
        }
    }

    public static void resetRegisterFile() {
        delete = true;
    }

    /**
     * Release all writers
     */
    public static void free(){
    	for (ConfiguredResultWriter configuredResultWriter : configuredResultWriters) {
    		configuredResultWriter.getResultWriter().free();
    	}
    	for (IResultWriter resultWriter : resultWriters) {
    		resultWriter.free();
    	}
    }
    
    /**
     * Returns last message written to Results, by any source. Returns empty string if no message has been
     * written since loading the Results class.
     * @return message
     */
    public static String getLastMessage(){
    	return lastMessage;
    }
}
