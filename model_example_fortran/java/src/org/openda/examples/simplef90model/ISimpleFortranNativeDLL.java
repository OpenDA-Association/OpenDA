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
import com.sun.jna.Library;
import com.sun.jna.ptr.DoubleByReference;
import com.sun.jna.ptr.IntByReference;

/**
 * Specification of the native model dll. In this example it is a Fortran DLL, so:
 * - all arguments go by reference
 * - at the end of a method with (a) string argument(s), the string length(s) are given (#char.s)
 */
public interface ISimpleFortranNativeDLL extends Library {

    /**
     * Initialize the DLL
     *
     * @param modelInstanceParentDirPath     Directory path that serves as parent for the model instances
     * @param modelTemplateDirPath           Directory with the template model
     * @param schematizationFilePath         Schematization file name
     * @param modelInstanceParentDirPathLen  Length of modelInstanceDir string
     * @param modelTemplateDirPathLen        Length of modelTemplateDir string
     * @param schematizationFilePathLen      Length of schematizationFile string
     */
    void m_simple_model_mp_init_(String modelInstanceParentDirPath,
                                 String modelTemplateDirPath, String schematizationFilePath,
                                 int modelInstanceParentDirPathLen,
                                 int modelTemplateDirPathLen, int schematizationFilePathLen);

    /**
     * Get a model instance
     *
     * @param modelInstancePath     (Output) Full directory path containing the newly created model instance
     * @param modelInstancePathLen  Length of modelInstancePath string
     * @return <0: error; >=0: an integer representing the model instance
     */
    int m_simple_model_mp_get_model_instance_(String modelInstancePath, int modelInstancePathLen);

    /**
     * Save the current model instance
     *
     * @param modelInstanceId The identifier of the model instance to be saved (== the current instance)
     * @return 0: success, /=0: error
     */
    int m_simple_model_mp_save_instance_(IntByReference modelInstanceId);

    /**
     * Restore a model instance
     *
     * @param modelInstanceId The identifier of the model instance to be restored
     * @return 0: success, /=0: error
     */
    int m_simple_model_mp_restore_instance_(IntByReference modelInstanceId);

    /**
     * Get the model's start time
     *
     * @param time (Out:) the model's start time
     * @return 0: success, /=0: error
     */
    int m_simple_model_mp_get_start_time_(DoubleByReference time);

    /**
     * Get the model's start time
     *
     * @param time (Out:) the model's end time
     * @return 0: success, /=0: error
     */
    int m_simple_model_mp_get_end_time_(DoubleByReference time);

    /**
     * Get the model's time step size
     *
     * @param time (Out:) the model's time step size
     * @return 0: success, /=0: error
     */
    int m_simple_model_mp_get_delta_t_(DoubleByReference time);

    /**
     * Get the model's current time
     *
     * @param modelInstanceId (In:)  The the model instance identifier
     * @param time            (Out:) the model's current time
     * @return 0: success, /=0: error
     */
    int m_simple_model_mp_get_current_time_(IntByReference modelInstanceId, DoubleByReference time);

    /**
     * Get the number of values for time dependent exchange item (e.g. a boundary condition),
     * for a certain time span. The number of values is not instance dependent.
     * This function is used to be able to set noise the boundary conditions.
     *
     * @param exchangeItemId (In:) Integer identifying the exchange item (in this case: discharge_on_laterals)
     * @param locationIndex  (In:) Location index. Value == n means: the n-th lateral
     * @param beginTime      (In:) Begin of the time span
     * @param endTime        (In:) End of the time span
     * @return <0: error, >=0: the number of values for the boundary condition in the specified time span.
     */
    int m_simple_model_mp_get_values_count_for_time_span_(IntByReference exchangeItemId,
                                                         IntByReference locationIndex,
                                                         DoubleByReference beginTime,
                                                         DoubleByReference endTime);

    /**
     * Get the values for time dependent exchange item (e.g. a boundary condition),
     * for a certain time span.
     *
     * @param modelInstanceId (In:)  The the model instance identifier
     * @param exchangeItemId  (In:)  Integer identifying the exchange item (in this case: discharge_on_laterals)
     * @param locationIndex   (In:)  Location index. Value == n means: the n-th lateral
     * @param beginTime       (In:)  Begin of the time span
     * @param endTime         (In:)  End of the time span
     * @param valueCountRef   (In:)  Expected number of values
     * @param values          (Out:) The retrieved values
     * @return 0: success, /=0: error
     */
    int m_simple_model_mp_get_values_for_time_span_(IntByReference modelInstanceId,
                                                    IntByReference exchangeItemId, IntByReference locationIndex,
                                                    DoubleByReference beginTime, DoubleByReference endTime,
                                                    IntByReference valueCountRef, double[] values);

    /**
     * Set the values for time dependent exchange item (e.g. a boundary condition),
     * for a certain time span.
     *
     * @param modelInstanceId (In:)  The the model instance identifier
     * @param exchangeItemId  (In:)  Integer identifying the exchange item (in this case: discharge_on_laterals)
     * @param locationIndex   (In:)  Location index. Value == n means: the n-th lateral
     * @param beginTime       (In:)  Begin of the time span
     * @param endTime         (In:)  End of the time span
     * @param valueCountRef   (In:)  Expected number of values
     * @param values          (In:)  The values to be set
     * @return 0: success, /=0: error
     */
    int m_simple_model_mp_set_values_for_time_span_(IntByReference modelInstanceId,
                                                    IntByReference exchangeItemId, IntByReference locationIndex,
                                                    DoubleByReference beginTime, DoubleByReference endTime,
                                                    IntByReference valueCountRef, double[] values);

    /**
     * Get the number of values for a spatial exchange item (e.g. a quantity on the full grid).
     *
     * @param exchangeItemId (In:) Integer identifying the exchange item
     * @return <0: error, >=0: the number of values for the exchange item.
     */
    int m_simple_model_mp_get_values_count_(IntByReference exchangeItemId);

    /**
     * Get the values for an exchange item that is defined on a spatial 'domain'
     * (e.g. a quantity on the full 1D or 2D grid).
     *
     * @param modelInstanceId (In:)  The the model instance identifier
     * @param exchangeItemId  (In:)  Integer identifying the exchange item
     * @param startIndex      (In:)  Start index in the (1d) domain
     * @param endIndex        (In:)  End index in the (1d) domain
     * @param values          (Out:) The retrieved values
     * @return 0: success, /=0: error
     */
    int m_simple_model_mp_get_values_(IntByReference modelInstanceId, IntByReference exchangeItemId,
                                      IntByReference startIndex, IntByReference endIndex,
                                      double[] values);

    /**
     * Set the values for an exchange item that is defined on a spatial 'domain'
     * (e.g. a quantity on the full 1D or 2D grid).
     *
     * @param modelInstanceId (In:)  The the model instance identifier
     * @param exchangeItemId  (In:)  Integer identifying the exchange item
     * @param startIndex      (In:)  Start index in the (1d) domain
     * @param endIndex        (In:)  End index in the (1d) domain
     * @param values          (In:)  The values to be set
     * @return 0: success, /=0: error
     */
    int m_simple_model_mp_set_values_(IntByReference modelInstanceId, IntByReference exchangeItemId,
                                      IntByReference startIndex, IntByReference endIndex,
                                      double[] values);

    /**
     * @param fromTimeStamp Time stamp to compute from (usually equal to model's current time)
     * @param toTimeStamp   Time stamp to compute to
     * @return 0: success, <0: error
     */
    int m_simple_model_mp_compute_(DoubleByReference fromTimeStamp, DoubleByReference toTimeStamp);

    /**
     * Finalize a model instance
     *
     * @param modelInstanceId The identifier of the model instance to be finalized
     * @return 0: success, <0: error
     */
    int m_simple_model_mp_finish_(IntByReference modelInstanceId);

    /**
     * @return The size of the model state
     */
    int m_simple_model_mp_get_state_size_();

    /**
     * @param stateSize State size
     * @param deltaStateValues Delta on the state
     * @param observationCount #observations
     * @param arrayIdentifiersForObsPoints Identifiers specifing the array (e.g. water level array)
     *                                     to get the predicted value for the observation location 
     * @param obsPointIndicesInArray in    Related indices
     * @param resultValues                 Resulting array of observationCount values, where result[i] is
     *                                     arrayDefinedByIdentifier[indexInArray[i]]
     * @return Array of values containing the delta on the observations
     */
    int m_simple_model_mp_apply_obs_tangent(int stateSize, double[] deltaStateValues,
                                            int observationCount,
                                            int[] arrayIdentifiersForObsPoints, int[] obsPointIndicesInArray,
                                            double[] resultValues);

    /**
     * @param observationCount #observations
     * @param arrayIdentifiersForObsPoints Identifiers specifing the array (e.g. water level array)
     *                                     to get the predicted value for the observation location
     * @param obsPointIndicesInArray in    Related indices
     * @param lambdaY                      The lambdaY values for the observations
     * @param stateSize State size         The size of the model's computed state
     * @param resultValues                 Resulting array of stateSize values, where result[i] is
     *                                     arrayDefinedByIdentifier[indexInArray[i]]
     * @return Array of values containing the lambdaX vector
     */
    int m_simple_model_mp_apply_obs_adjoint(int observationCount,
                                            int[] arrayIdentifiersForObsPoints, int[] obsPointIndicesInArray,
                                            double[] lambdaY, int stateSize, double[] resultValues);

}

