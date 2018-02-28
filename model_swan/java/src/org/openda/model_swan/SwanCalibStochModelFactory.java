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
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;
import org.openda.model_swan.SwanCalibWrapper;
import org.openda.model_swan.SwivtParametersResultWriter;
import org.openda.model_swan.io.SwanCalibStochModelConfigReader;
import org.openda.uncertainties.UncertaintyEngine;
import org.openda.utils.Results;

import java.io.File;

/**
 * Model factory for Swan Calibration StochModel.
 *
 * Code copied and adapted from DaModelFactory.
 */
public class SwanCalibStochModelFactory implements IStochModelFactory {

    private int lastModelInstanceNumber;
    private SwanCalibStochModelConfig.SwanCalibComponentConfig modelConfiguration;
    private UncertaintyEngine uncertaintyEngine;
    private Object currentModelInstance;
    private File configFile = null;
    private File dirToBeCloned = null;

    public void initialize(File configDir, String[] arguments) {

        if (arguments.length != 1) {
            throw new RuntimeException("Initialize function expects 1 argument: openda config file");
        }
        configFile = new File(configDir, arguments[0]);

        SwanCalibStochModelConfigReader configReader = new SwanCalibStochModelConfigReader(configFile);
        SwanCalibStochModelConfig configurationStoch = configReader.getSwanCalibStochModelConfig();

        SwanCalibStochModelConfig.SwanCalibComponentConfig uncertaintyConfiguration = configurationStoch.getUncertaintyConfig();
        uncertaintyEngine = (UncertaintyEngine) getObjectInstance(uncertaintyConfiguration.getClassName());
        uncertaintyEngine.initialize(uncertaintyConfiguration.getWorkingDir(), uncertaintyConfiguration.getArguments());

        modelConfiguration = configurationStoch.getModelConfig();

        lastModelInstanceNumber = 0;
        currentModelInstance = null;
    }

    public IStochModelInstance getInstance(OutputLevel outputLevel) {

        dirToBeCloned = modelConfiguration.getWorkingDir();
        String[] arguments = modelConfiguration.getArguments();
        if (arguments.length == 2) {

            File regularConfigFile = new File(modelConfiguration.getWorkingDir(), modelConfiguration.getArguments()[0]);
            File secondArgumentAsDir = new File(modelConfiguration.getWorkingDir(), modelConfiguration.getArguments()[1]);
            if (regularConfigFile.exists() && secondArgumentAsDir.isDirectory()) {
                // the directory to be cloned is specified by the second argument
                dirToBeCloned = secondArgumentAsDir;
                arguments = new String[]{arguments[0]};
            }
        }
        
        boolean firstInstance = lastModelInstanceNumber == 0;
        if (firstInstance) {
        	BBUtils.removeExistingModelInstanceDirectories(dirToBeCloned);
        }

        File instanceDir = new File(dirToBeCloned.getAbsolutePath() + lastModelInstanceNumber++);
        if (instanceDir.getAbsolutePath().equals(dirToBeCloned.getAbsolutePath())) {
            throw new RuntimeException("Instance dir. is equalt to template dir. (" +
                    instanceDir + "), could not clone work directory");
        }
        BBUtils.makeDirectoryClone(dirToBeCloned, instanceDir);

        currentModelInstance = getObjectInstance(modelConfiguration.getClassName());

        if (currentModelInstance instanceof IStochModelInstance) {
            boolean stochComponentsHaveBeenSet = false;

            if (currentModelInstance instanceof SwanCalibWrapper) {
            	SwanCalibWrapper swanWrapper = (SwanCalibWrapper) currentModelInstance;
                swanWrapper.initialize(instanceDir, configFile.getParentFile(), arguments);
                swanWrapper.setStochasticComponents(uncertaintyEngine);

                if (firstInstance) {
                    File swivtParametersFile = swanWrapper.getSwanCalibWrapperConfig().getSwivtParametersFile();
                    SwivtParametersResultWriter resultWriter = new SwivtParametersResultWriter(
                            swivtParametersFile.getParentFile(), swivtParametersFile.getName());
                    Results.addResultWriter(resultWriter);
                }

                stochComponentsHaveBeenSet = true;
            }

            if (!stochComponentsHaveBeenSet) {
                throw new RuntimeException("Unknown ModelInstance class: " + modelConfiguration.getClassName());
            }

            return (IStochModelInstance) currentModelInstance;
        } else {
            throw new RuntimeException("Class " +  modelConfiguration.getClassName() +
                    " does not implement the " + IStochModelInstance.class.getName() + " interface.");
        }
    }

    public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
    	throw new UnsupportedOperationException("getPostprocessorInstance not implemented for " + getClass().getName());
    }

	public void finish() {
		// no action needed (yet)
	}

	private static Object getObjectInstance(String className) {
        Object instance;
        try {
            Class aClass = Class.forName(className);
            instance = aClass.newInstance();
        } catch (ClassNotFoundException e) {
            throw new RuntimeException("Error instantiating class " + className + ": " + e.getMessage());
        } catch (InstantiationException e) {
            throw new RuntimeException("Error instantiating class " + className + ": " + e.getMessage());
        } catch (IllegalAccessException e) {
            throw new RuntimeException("Error instantiating class " + className + ": " + e.getMessage());
        }
        return instance;
    }

    public static File getAndCheckFilePath(File configFile, String fileName) {
        return getAndCheckFilePath(configFile, fileName, true, false);
    }

    public static File getAndCheckFilePath(File dir, String fileName, boolean maybeNull, boolean mustExist) {
        File file = null;
        if (fileName == null) {
            if (!maybeNull) {
                throw new IllegalArgumentException("required file's name can not be null, configFile: " +
                        dir.getAbsolutePath());
            }
        } else {
            file = new File(dir, fileName);
            if (!file.exists() && mustExist) {
                throw new IllegalArgumentException("Can not find file:\n\t" +
                        file.getAbsolutePath() + "\n\t(config file: " + dir.getAbsolutePath() + ")");
            }
        }
        return file;
    }
}
