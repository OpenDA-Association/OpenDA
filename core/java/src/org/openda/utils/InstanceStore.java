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

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IInstance;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.IVector;

import java.io.*;
import java.util.ArrayList;

public class InstanceStore implements IResultWriter {

    private static String instancesFile = null;
    private static File registerFile = null;
    private static boolean delete = true;
    private int defaultMaxSize = Integer.MAX_VALUE;

    public InstanceStore(File inputFile, boolean delete) {
        if (inputFile != null) {
            this.instancesFile = BBUtils.getFileNameWithoutExtension(inputFile.getAbsolutePath());
        }
        this.delete = delete;
    }


    public void free(){};
    
    public void putMessage(Source source, String message) {
        // Nothing to do
    }

    public void putMessage(IInstance source, String message) {
        // Nothing to do
    }

	public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration) {
        // Nothing to do
	}

    public void putIterationReport(IInstance source, int iteration, double cost, IVector parameters) {
        if (source instanceof IModelInstance) {
            IModelInstance modelInstance = (IModelInstance) source;
            File modelRunDir = modelInstance.getModelRunDir();
            if (modelRunDir!= null) {
                registerInstance(modelRunDir);
            } else {
                registerInstance(new File("dummy"));
            }
            registerCost(iteration, cost, parameters);
        }
    }

    
    public int getDefaultMaxSize() {
        return defaultMaxSize;
    }

    public static void setInputFile(File input) {
        instancesFile = BBUtils.getFileNameWithoutExtension(input.getAbsolutePath());
    }

    private static void registerInstance(File instance) {
        if (registerFile == null) {
            registerFile = new File( instancesFile + ".orp");
        }
        if (delete) {
            delete = false;
            registerFile.delete();
            File presentationFile = new File(instancesFile + ".oap");
            presentationFile.delete();
        }
        try {
            FileWriter writer = new FileWriter(registerFile, true);
            File parentFile = registerFile.getParentFile();
            String outputFileName = RelativePath.getRelativePath(parentFile, instance); 
            writer.write( outputFileName+ "\n");
            writer.close();
        } catch (IOException e) {
            Results.putMessage("Error: " + e.getMessage());
        }
    }

    private static void registerCost(int index, double cost, IVector parameters) {
        if (registerFile == null) {
            Results.putMessage("Error: registerCost called before registerInstance!");
            return; // This should not happen!
        }
        try {
            FileWriter writer = new FileWriter(registerFile, true);
            writer.write("Iteration " + Integer.toString(index) +
                    ": cost " + PrintNumber.printNumberExtended(cost) + ";" + printParameters(parameters) + "\n");
            writer.close();
        } catch (IOException e) {
            Results.putMessage("Error: " + e.getMessage());
        }
    }

    private static String printParameters(IVector parameters) {
        String printString = "";

        if (parameters instanceof TreeVector) {
			TreeVector parametersAsTreeVector = (TreeVector) parameters;
			ArrayList<String> parNames = parametersAsTreeVector.getSubTreeVectorIds();
			for (int p = 0; p < parNames.size(); p++) {
				String parName = parNames.get(p);
				if (p>0) printString += "; ";
				printString += parName;
				double[] values = parametersAsTreeVector.getSubTreeVector(parName).getValues();
				for (int i = 0; i < values.length; i++) {
					printString += " " + PrintNumber.printNumberExtended(values[i]);
				}
			}
        }
        return printString;
    }

    public static void resetRegisterFile() {
        delete = true;
    }

    public static String[] getInstances() {
        String [] instances = new String[0];
		File orpFile;
		if (instancesFile == null) {
            return instances;
        } else {
			orpFile = new File(instancesFile + ".orp");
			if (!orpFile.exists()) {
				return instances;
			}
		}
        try {
			LineNumberReader reader = new LineNumberReader(new FileReader(orpFile));
            String line = null;
            while ((line = reader.readLine()) != null) {
                line = reader.readLine();
                if (line == null) {
                    continue;
                }
                String [] newInstances = new String[instances.length+1];
                for (int i = 0; i < instances.length; i ++) {
                    newInstances[i] = instances[i];
                }
                newInstances[instances.length] = line;
                instances = newInstances;
            }
            reader.close();
        } catch (IOException e) {
            Results.putMessage("Error: " + e.getMessage());
       }

       return instances;
    }

}
