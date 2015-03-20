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
package org.openda.examples.simplef90model;

import com.sun.jna.Native;
import com.sun.jna.ptr.DoubleByReference;
import com.sun.jna.ptr.IntByReference;
import org.openda.interfaces.ITime;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;

/**
 * Thin 'wrapping class' around the simple model's native dll.
 * The only purposes of the class is to translate java arguments into f90 arguments (and vice versa)
 */
public class SimpleModelDLL {

    // Exchange item itentifiers
    // These must be the same as in the ones the FortranDLL (see top of m_simple_model)
    // One might consider to get them from the DLL, eg.:
    //         waterlevel_on_grid = nativeDLL.getIntegerIdentifier("waterlevel on grid");
    public static int gravity = 1;
    public static final int waterlevel_on_grid = 2;
    public static int friction_on_grid = 3;
    public static int discharge_on_laterals = 4;

    // Flag indicating whether the native code has been initialized or not 
    private static boolean dllYetToBeInitialized = true;
    // Flag indicating whether we are searching for dll or so 
    public static final boolean RUNNING_ON_WINDOWS = System.getProperty("os.name").startsWith("Windows");
    
    // handle to native library
    private static ISimpleFortranNativeDLL nativeDLL;

    // store the model instance identifier for this instance.
    private int myModelInstanceId = -1;

    // keep track the model instance identifier that is currently in memory.
    private static int currentModelInstance = -1;

    /**
     * Initialize the dummy library
     *
     * @param modelInstanceParentDir  Directory path that serves as parent for the model instances
     * @param simpleFortranDll        Full path specification of the native DLL
     * @param modelTemplateDir        Directory with the template model
     * @param schematizationFile      Schematization file name
     */
    public static void initialize(File simpleFortranDll, File modelInstanceParentDir, File modelTemplateDir, String schematizationFile) {

        // Initialize the DLL, if not done yet
        if (dllYetToBeInitialized) {

            // determine full paths for parent dir and model template dir
            String modelInstanceParentDirPath;
            String modelTemplateDirPath;
            try {
                modelInstanceParentDirPath = modelInstanceParentDir.getCanonicalPath();
            } catch (IOException e) {
                throw new RuntimeException("Could not create canonical path for " + modelInstanceParentDir.getAbsolutePath());
            }
            try {
                modelTemplateDirPath = modelTemplateDir.getCanonicalPath();
            } catch (IOException e) {
                throw new RuntimeException("Could not create canonical path for " + modelTemplateDir.getAbsolutePath());
            }

            String nativeDllPath = simpleFortranDll.getAbsolutePath();
            File nativeDll = new File(nativeDllPath);
            if (!nativeDll.exists()) {
                throw new RuntimeException("Native DLL/SO does not exist: " + simpleFortranDll.getAbsolutePath());
            }
            
            if(SimpleModelDLL.RUNNING_ON_WINDOWS){ //TODO create a better test here
                nativeDLL = (ISimpleFortranNativeDLL) Native.loadLibrary(nativeDllPath, ISimpleFortranNativeDLL.class);
            	
            }else{
            	// For now assumes that gfortran is used for linux and ifort for windows
            	GfortranFunctionMapper gfortranMapper = new GfortranFunctionMapper();
            	HashMap<String, String> gfortranMap = gfortranMapper.getMap();
                nativeDLL = (ISimpleFortranNativeDLL) Native.loadLibrary(nativeDllPath, ISimpleFortranNativeDLL.class,gfortranMap);            	
            }
            
            //nativeDLL = (ISimpleFortranNativeDLL) Native.loadLibrary(nativeDllPath, ISimpleFortranNativeDLL.class);
            nativeDLL.m_simple_model_mp_init_(modelInstanceParentDirPath, modelTemplateDirPath, schematizationFile,
                    modelInstanceParentDirPath.length(), modelTemplateDirPath.length(), schematizationFile.length());
            dllYetToBeInitialized = false;
        }else{
        	SimpleModelDLL.currentModelInstance = -1;
        }
    }

    public static void free() {
        // TODO: check how DLL can be unloaded
        // nativeDLL.destroy();
    }

    
    /**
     * Initialize the dummy library
     *
     * @param modelInstanceDir Directory path containing the model instance
     * @return                 An instance of the native model
     */
    public static SimpleModelDLL getInstance(File modelInstanceDir) {
        return new SimpleModelDLL(modelInstanceDir);
    }

    /**
     * Initialize the dummy library
     *
     * @param modelInstanceDir        Directory path containing the model instance
     */
    private SimpleModelDLL (File modelInstanceDir) {

        // determine full model instance path
        String modelInstanceDirPath;
        try {
            modelInstanceDirPath = modelInstanceDir.getCanonicalPath();
        } catch (IOException e) {
            throw new RuntimeException("Could not create canonical path for " + modelInstanceDir.getAbsolutePath());
        }

        // initialize the model instance
        startModelInstanceAccess();
        currentModelInstance = myModelInstanceId = nativeDLL.m_simple_model_mp_get_model_instance_(modelInstanceDirPath, modelInstanceDirPath.length());
        endModelInstanceAccess();
        if (currentModelInstance <= 0) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid result from nativeDLL.GET_MODEL_INSTANCE() call, retVal= "
                    + currentModelInstance);
        }
    }

    public double getDeltaT() {
        DoubleByReference deltaT = new DoubleByReference();
        int retVal = nativeDLL.m_simple_model_mp_get_delta_t_(deltaT);
        if (retVal != 0) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid result from dll.GET_DELTA_T call, retVal= " + retVal);
        }
        return deltaT.getValue();
    }

    public double getStartTime() {
        DoubleByReference startTime = new DoubleByReference();
        int retVal = nativeDLL.m_simple_model_mp_get_start_time_(startTime);
        if (retVal != 0) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid result from dll.GET_START_TIME call, retVal= " + retVal);
        }
        return startTime.getValue();
    }

    public double getEndTime() {
        DoubleByReference endTime = new DoubleByReference();
        int retVal = nativeDLL.m_simple_model_mp_get_end_time_(endTime);
        if (retVal != 0) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid result from dll.GET_END_TIME call, retVal= " + retVal);
        }
        return endTime.getValue();
    }

    public double getCurrentTime() {
        DoubleByReference currentTime = new DoubleByReference();
        startModelInstanceAccess();
        int retVal = nativeDLL.m_simple_model_mp_get_current_time_(new IntByReference(myModelInstanceId), currentTime);
        endModelInstanceAccess();
        if (retVal != 0) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid result from dll.GET_CURRENT_TIME call, retVal= " + retVal);
        }
        return currentTime.getValue();
    }

    private int getValuesCount(int exchangeItemId) {
        int valuesCount = nativeDLL.m_simple_model_mp_get_values_count_(new IntByReference(exchangeItemId));
        if (valuesCount < 0) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid result from dll.GET_VALUES_COUNT call, valuesCount= " + valuesCount);
        }
        return valuesCount;
    }

    private int getValuesCount(int exchangeItemId, int locationIndex, ITime startTime, ITime endTime) {
        int valuesCount = nativeDLL.m_simple_model_mp_get_values_count_for_time_span_(
                new IntByReference(exchangeItemId), new IntByReference(locationIndex),
                new DoubleByReference(startTime.getMJD()), new DoubleByReference(endTime.getMJD()));
        if (valuesCount < 0) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid result from dll.GET_VALUES_COUNT_FOR_TIMESPAN call, valuesCount= " + valuesCount);
        }
        return valuesCount;
    }

    public double getValue(int exchangeItemId) {
        if (exchangeItemId != gravity) {
            throw new RuntimeException("Invalid exchange item identifier (" + exchangeItemId +
                    ") for getValue call for single double");
        }
        return getValues(exchangeItemId)[0];
    }

    public double[] getValues(int exchangeItemId) {
        return getValues(exchangeItemId, 0, getValuesCount(exchangeItemId) - 1);
    }

    public double[] getValues(int exchangeItemId, int startIndex, int endIndex) {
        int valuesCount = getValuesCount(exchangeItemId);
        double[] values = new double[valuesCount];
        startModelInstanceAccess();
        int retVal = nativeDLL.m_simple_model_mp_get_values_(
                new IntByReference(myModelInstanceId),
                new IntByReference(exchangeItemId),
                new IntByReference(startIndex + 1), new IntByReference(endIndex + 1),
                values);
        endModelInstanceAccess();
        if (retVal != 0) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid result from dll.GET_VALUES call, retVal= " + retVal);
        }
        return values;
    }

    public double[] getValues(int exchangeItemId, int locationIndex, ITime startTime, ITime endTime) {
        int valuesCount = getValuesCount(exchangeItemId, locationIndex, startTime, endTime);
        double[] values = new double[valuesCount];
        startModelInstanceAccess();
        int retVal = nativeDLL.m_simple_model_mp_get_values_for_time_span_(
                new IntByReference(myModelInstanceId),
                new IntByReference(exchangeItemId), new IntByReference(locationIndex),
                new DoubleByReference(startTime.getMJD()), new DoubleByReference(endTime.getMJD()),
                new IntByReference(valuesCount), values);
        endModelInstanceAccess();
        if (retVal != 0) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid result from dll.GET_VALUES call, retVal= " + retVal);
        }
        return values;
    }

    public void setValue(int exchangeItemId, double value) {
        if (exchangeItemId != gravity) {
            throw new RuntimeException("Invalid exchange item identifier (" + exchangeItemId +
                    ") for getValue call for single double");
        }
        setValues(exchangeItemId, new double[]{value});
    }

    public void setValues(int exchangeItemId, double[] values) {
        setValues(exchangeItemId, values, 0, getValuesCount(exchangeItemId) - 1);
    }

    public void setValues(int exchangeItemId, double[] values, int startIndex, int endIndex) {
        int valuesCount = getValuesCount(exchangeItemId);
        if (valuesCount != values.length) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid #values in setValues(exchangeItemId=" +
                    exchangeItemId + "). #Values=" + values.length + ", #expected=" + valuesCount);
        }
        startModelInstanceAccess();
        int retVal = nativeDLL.m_simple_model_mp_set_values_(
                new IntByReference(myModelInstanceId),
                new IntByReference(exchangeItemId),
                new IntByReference(startIndex + 1), new IntByReference(endIndex + 1),
                values);
        endModelInstanceAccess();
        if (retVal != 0) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid result from dll.SET_VALUES call, retVal= " + retVal);
        }
    }

    public void setValues(int exchangeItemId, double[] values, int locationIndex, ITime startTime, ITime endTime) {
        int valuesCount = getValuesCount(exchangeItemId, locationIndex, startTime, endTime);
        if (valuesCount != values.length) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid #values in setValues(exchangeItemId=" +
                    exchangeItemId + "). #Values=" + values.length + ", #expected=" + valuesCount);
        }
        startModelInstanceAccess();
        int retVal = nativeDLL.m_simple_model_mp_set_values_for_time_span_(
                new IntByReference(myModelInstanceId),
                new IntByReference(exchangeItemId), new IntByReference(locationIndex),
                new DoubleByReference(startTime.getMJD()), new DoubleByReference(endTime.getMJD()),
                new IntByReference(valuesCount), values);
        endModelInstanceAccess();
        if (retVal != 0) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid result from dll.SET_VALUES call, retVal= " + retVal);
        }
    }

    public void compute(ITime fromTime, ITime toTime) {
        startModelInstanceAccess();
        int retVal = nativeDLL.m_simple_model_mp_compute_(
                new DoubleByReference(fromTime.getMJD()),
                new DoubleByReference(toTime.getMJD()));
        endModelInstanceAccess();
        if (retVal != 0) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid result from dll.COMPUTE call, retVal= " + retVal);
        }
    }

    public double[] applyObservationTangent(double[] deltaStateValues,
                                            int[] arrayIdentifiersForObsPoints, int[] ObsPointIndicesInArray) {
        // allocate space for result (#observation)
        double[] result = new double[arrayIdentifiersForObsPoints.length];
        // let computational core apply observation tangent.
        int retVal = nativeDLL.m_simple_model_mp_apply_obs_tangent(deltaStateValues.length, deltaStateValues,
                        arrayIdentifiersForObsPoints.length, arrayIdentifiersForObsPoints, ObsPointIndicesInArray,
                        result);
        if (retVal != 0) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid result from dll.APPLYOBSERVATIONTANGENT call, retVal= " + retVal);
        }
        return result;
    }

    public double[] applyObservationAdjoint(int[] arrayIdentifiersForObsPoints, int[] ObsPointIndicesInArray,
                                            double[] lambdaY) {
        // allocate space for result (state size)
        double[] lambdaX = new double[nativeDLL.m_simple_model_mp_get_state_size_()];
        // let computational core apply observation tangent.
        int retVal = nativeDLL.m_simple_model_mp_apply_obs_adjoint(arrayIdentifiersForObsPoints.length,
                arrayIdentifiersForObsPoints, ObsPointIndicesInArray, lambdaY,
                nativeDLL.m_simple_model_mp_get_state_size_(), lambdaX);
        if (retVal != 0) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid result from dll.APPLYOBSERVATIONTANGENT call, retVal= " + retVal);
        }
        return lambdaX;
    }

    public void finish() {
        startModelInstanceAccess();
        int retVal = nativeDLL.m_simple_model_mp_finish_(new IntByReference(myModelInstanceId));
        endModelInstanceAccess();
        if (retVal != 0) {
            nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
            throw new RuntimeException("Invalid result from dll.FINALIZE call, retVal= " + retVal);
        }
    }

    private void startModelInstanceAccess() {

        // store currently active model instance
        if (currentModelInstance >= 0) {
            if (currentModelInstance != myModelInstanceId) {
                // Model Instance switch, save current instance
                int retVal = nativeDLL.m_simple_model_mp_save_instance_(new IntByReference(currentModelInstance));
                if (retVal != 0) {
                    nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
                    throw new RuntimeException("Error saving model instance " + retVal);
                }
            }
        }

        // load required model instance
        if (myModelInstanceId >= 0) {
            if (myModelInstanceId != currentModelInstance) {
                // Model Instance switch, restore required instance
                int retVal = nativeDLL.m_simple_model_mp_restore_instance_(new IntByReference(myModelInstanceId));
                if (retVal != 0) {
                    nativeDLL.m_simple_model_mp_finish_(new IntByReference(currentModelInstance));
                    throw new RuntimeException("Error restoring model instance " + retVal);
                }
            }
        }

        // store id of currently active model instance
        currentModelInstance = myModelInstanceId;
    }

    private void endModelInstanceAccess() {
        // no action (yet)
    }
}
