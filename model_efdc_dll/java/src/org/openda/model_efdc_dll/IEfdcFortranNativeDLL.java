/* OpenDA v2.4.1 
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
package org.openda.model_efdc_dll;
import com.sun.jna.Library;
import com.sun.jna.ptr.DoubleByReference;
import com.sun.jna.ptr.IntByReference;

/**
 * Specification of the native model dll. In this example it is a Fortran DLL, so:
 * - all arguments go by reference
 * - at the end of a method with (a) string argument(s), the string length(s) are given (#char.s)
 */
public interface IEfdcFortranNativeDLL extends Library {

    /**
     * Initialize the DLL
     *
     * @param modelInstanceParentDirPath     Directory path that serves as parent for the model instances
     * @param modelTemplateDirPath           Directory with the template model
     * @param modelInstanceParentDirPathLen  Length of modelInstanceDir string
     * @param modelTemplateDirPathLen        Length of modelTemplateDir string
     * @return 0: success, /=0: error
     */
    int m_openda_wrapper_init_(String modelInstanceParentDirPath,
                                 String modelTemplateDirPath,
                                 int modelInstanceParentDirPathLen,
                                 int modelTemplateDirPathLen);

    /**
     * Get a model instance
     *
     * @param modelInstancePath     (Output) Full directory path containing the newly created model instance
     * @param modelInstancePathLen  Length of modelInstancePath string
     * @return <0: error; >=0: an integer representing the model instance
     */
    int m_openda_wrapper_get_model_instance_(String modelInstancePath, int modelInstancePathLen);

    /**
     * Save the current model instance
     *
     * @param modelInstanceId The identifier of the model instance to be saved (== the current instance)
     * @return 0: success, /=0: error
     */
    int m_openda_wrapper_save_instance_(IntByReference modelInstanceId);

    /**
     * Restore a model instance
     *
     * @param modelInstanceId The identifier of the model instance to be restored
     * @return 0: success, /=0: error
     */
    int m_openda_wrapper_restore_instance_(IntByReference modelInstanceId);

    
    /**
     * Get the model's reference year
     * 
     * @param modelInstanceId (In:)  The the model instance identifier
     * @return reference year, <0: error
     */
    int m_openda_wrapper_get_reference_year_(IntByReference modelInstanceId);
    
    /**
     * Get the model's start time
     * 
     * @param modelInstanceId (In:)  The the model instance identifier
     * @param time (Out:) the model's start time
     * @return 0: success, /=0: error
     */
    int m_openda_wrapper_get_start_time_(IntByReference modelInstanceId, DoubleByReference time);

    /**
     * Get the model's start time
     * 
     * @param modelInstanceId (In:)  The the model instance identifier
     * @param time (Out:) the model's end time
     * @return 0: success, /=0: error
     */
    int m_openda_wrapper_get_end_time_(IntByReference modelInstanceId, DoubleByReference time);

    /**
     * Get the model's time step size
     *
     * @param time (Out:) the model's time step size
     * @return 0: success, /=0: error
     */
    int m_openda_wrapper_get_delta_t_(DoubleByReference time);
    
    /**
     * Get the model's reference period
     *
     * @param time (Out:) the model's reference period
     * @return 0: success, /=0: error
     */
    int m_openda_wrapper_get_reference_period_(DoubleByReference time);


    /**
     * Get the dimensionless depth of the layers bottom = 1
     *
     * @param modelInstanceId (In:)  The model instance identifier
     * @param values          (Out:) The retrieved values
     * @return 0: success, /=0: error
     */
    int m_openda_wrapper_get_layer_depths_(IntByReference modelInstanceId, double[] values);

    /**
     * Get the number of the layers used by model
     *
     * @param modelInstanceId (In:)  The model instance identifier
     * @return nr of layers , <=0: error
     */
    int m_openda_wrapper_get_layer_count_for_model_(IntByReference modelInstanceId );


    /**
     * Get the model's current time
     *
     * @param modelInstanceId (In:)  The the model instance identifier
     * @param time            (Out:) the model's current time
     * @return 0: success, /=0: error
     */
    int m_openda_wrapper_get_current_time_(IntByReference modelInstanceId, DoubleByReference time);

    /**
     * Get the times for the time dependent exchange item (e.g. a boundary condition).
     *
     * @param modelInstanceId (In:)  The model instance identifier
     * @param exchangeItemId  (In:)  Integer identifying the exchange item
     * @param locationIndex  (In:) Location index. Value == n means: the n-th lateral
     * @param valuesCount     (In:)  The number of points in the time span
     * @param times          (Out:) The retrieved times
     * @return <0: error, /=0: error.
     */
    int m_openda_wrapper_get_times_for_ei_(IntByReference modelInstanceId, 
                                           IntByReference exchangeItemId,
                                           IntByReference locationIndex, 
                                           IntByReference valuesCount,     
                                           double[] times);
    
    /**
     * Set the times for the time dependent exchange item (e.g. a boundary condition).
     *
     * @param modelInstanceId (In:)  The model instance identifier
     * @param exchangeItemId  (In:)  Integer identifying the exchange item
     * @param locationIndex  (In:) Location index. Value == n means: the n-th lateral
     * @param valuesCount     (In:)  The number of points in the time span
     * @param times          (In:) The retrieved times
     * @return <0: error, /=0: error.
     */
    int m_openda_wrapper_set_times_for_ei_(IntByReference modelInstanceId, 
                                           IntByReference exchangeItemId,
                                           IntByReference locationIndex, 
                                           IntByReference valuesCount, 
                                           double[] times);

    /**
     * Get the number of values for a spatial exchange item (e.g. a quantity on the full grid).
     *
     * @param modelInstanceId (In:) The model instance identifier
     * @param exchangeItemId (In:)  Integer identifying the exchange item
     * @return <0: error, >=0: the number of values for the exchange item.
     */
    int m_openda_wrapper_get_values_count_(IntByReference modelInstanceId, IntByReference exchangeItemId);

    /**
     * Get the number of values for a spatial exchange item (e.g. a quantity on the full grid).
     *
     * @param modelInstanceId (In:) The model instance identifier
     * @param exchangeItemId (In:)  Integer identifying the exchange item
     * @return <0: error, >=0: the number of values for the exchange item.
     */
    int m_openda_wrapper_get_cell_count_(IntByReference modelInstanceId, IntByReference exchangeItemId);

    /**
     * Get the number of layers for a spatial exchange item (e.g. a quantity on the full grid).
     *
     * @param modelInstanceId (In:) The model instance identifier
     * @param exchangeItemId (In:)  Integer identifying the exchange item
     * @return <0: error, >=0: the number of layers for the exchange item.
     */
    int m_openda_wrapper_get_layer_count_(IntByReference modelInstanceId, IntByReference exchangeItemId);


    /**
     * Get the number of values for a spatial forcing or boundary exchange item.
     *
     * @param modelInstanceId (In:)  The model instance identifier
     * @param exchangeItemId  (In:)  Integer identifying the exchange item (in this case: discharge_on_laterals)
     * @param locationIndex   (In:)  Location index. Value == n means: the n-th lateral
     * @return <0: error, >=0: the number of values for the exchange item (length of the time series)
     */
    int m_openda_wrapper_get_values_count_for_location_(IntByReference modelInstanceId,
                                                        IntByReference exchangeItemId,
                                                        IntByReference locationIndex);


    /**
     * Get the number of values for a spatial forcing or boundary exchange item.
     *
     * @param modelInstanceId (In:)  The model instance identifier
     * @param exchangeItemId  (In:)  Integer identifying the exchange item (in this case: discharge_on_laterals)
     * @param locationIndex   (In:)  Location index. Value == n means: the n-th lateral
     * @return <0: error, >=0: the number of values for the exchange item (length of the time series)
     */
    int m_openda_wrapper_get_times_count_for_location_(IntByReference modelInstanceId,
                                                        IntByReference exchangeItemId,
                                                        IntByReference locationIndex);


    /**
     * Get the number of values for a spatial forcing or boundary exchange item.
     *
     * @param modelInstanceId (In:)  The model instance identifier
     * @param exchangeItemId  (In:)  Integer identifying the exchange item (in this case: discharge_on_laterals)
     * @param locationIndex   (In:)  Location index. Value == n means: the n-th lateral
     * @return <0: error, >=0: the number of values for the exchange item (length of the time series)
     */
    /**int m_openda_wrapper_get_layer_count_for_location_(IntByReference modelInstanceId,
                                                        IntByReference exchangeItemId,
                                                        IntByReference locationIndex);
     **/
    /**
     * Get the number of values for time dependent exchange item (e.g. a boundary condition),
     * for a certain time span. The number of values is not instance dependent.
     * This function is used to be able to set noise the boundary conditions.
     *
     *
     * @param modelInstanceId (In:) The model instance identifier
     * @param exchangeItemId  (In:) Integer identifying the exchange item (in this case: discharge_on_laterals)
     * @param locationIndex   (In:) Location index. Value == n means: the n-th lateral
     * @param beginTime       (In:) Begin of the time span
     * @param endTime         (In:) End of the time span
     * @return <0: error, >=0: the number of values for the boundary condition in the specified time span.
     */
    int m_openda_wrapper_get_values_count_for_time_span_(IntByReference modelInstanceId,
                                                         IntByReference exchangeItemId,
                                                         IntByReference locationIndex,
                                                         DoubleByReference beginTime,
                                                         DoubleByReference endTime);

    /**
     * Get the number of values for time dependent exchange item (e.g. a boundary condition),
     * for a certain time span. The number of values is not instance dependent.
     * This function is used to be able to set noise the boundary conditions.
     *
     *
     * @param modelInstanceId (In:) The model instance identifier
     * @param exchangeItemId  (In:) Integer identifying the exchange item (in this case: discharge_on_laterals)
     * @param locationIndex   (In:) Location index. Value == n means: the n-th lateral
     * @param beginTime       (In:) Begin of the time span
     * @param endTime         (In:) End of the time span
     * @return <0: error, >=0: the number of values for the boundary condition in the specified time span.
     */
    int m_openda_wrapper_get_times_count_for_time_span_(IntByReference modelInstanceId,
                                                         IntByReference exchangeItemId,
                                                         IntByReference locationIndex,
                                                         DoubleByReference beginTime,
                                                         DoubleByReference endTime);

    /**
     * Get the number of values for time dependent exchange item (e.g. a boundary condition),
     * for a certain time span. The number of values is not instance dependent.
     * This function is used to be able to set noise the boundary conditions.
     *
     *
     * @param modelInstanceId (In:) The model instance identifier
     * @param exchangeItemId  (In:) Integer identifying the exchange item (in this case: discharge_on_laterals)
     * @param locationIndex   (In:) Location index. Value == n means: the n-th lateral
     * @param beginTime       (In:) Begin of the time span
     * @param endTime         (In:) End of the time span
     * @return <0: error, >=0: the number of values for the boundary condition in the specified time span.
     */
    /**int m_openda_wrapper_get_layer_count_for_time_span_(IntByReference modelInstanceId,
                                                         IntByReference exchangeItemId,
                                                         IntByReference locationIndex,
                                                         DoubleByReference beginTime,
                                                         DoubleByReference endTime);
    **/

    /**
     * Get the values for time dependent exchange item (e.g. a boundary condition),
     * for a certain time span.
     *
     * @param modelInstanceId (In:)  The model instance identifier
     * @param exchangeItemId  (In:)  Integer identifying the exchange item
     * @param locationIndex   (In:)  Location index. Value == n means: the n-th lateral
     * @param layerIndex      (In:)  Layer index.
     * @param beginTime       (In:)  Begin of the time span
     * @param endTime         (In:)  End of the time span
     * @param valueCountRef   (In:)  Expected number of values
     * @param values          (Out:) The retrieved values
     * @return 0: success, /=0: error
     */
    int m_openda_wrapper_get_values_for_time_span_(IntByReference modelInstanceId,
                                                   IntByReference exchangeItemId, 
                                                   IntByReference locationIndex,
                                                   IntByReference layerIndex,
                                                   DoubleByReference beginTime, 
                                                   DoubleByReference endTime,
                                                   IntByReference valueCountRef,
                                                   double[] values);

    /**
     * Set the values for time dependent exchange item (e.g. a boundary condition),
     * for a certain time span.
     *
     * @param modelInstanceId (In:)  The model instance identifier
     * @param exchangeItemId  (In:)  Integer identifying the exchange item (in this case: discharge_on_laterals)
     * @param locationIndex   (In:)  Location index. Value == n means: the n-th lateral
     * @param layerIndex      (In:)  Layer index. Value == n means: the n-th lateral
     * @param beginTime       (In:)  Begin of the time span
     * @param endTime         (In:)  End of the time span
     * @param valueCountRef   (In:)  Expected number of values
     * @param values          (In:)  The values to be set
     * @return 0: success, /=0: error
     */
    int m_openda_wrapper_set_values_for_time_span_(IntByReference modelInstanceId,
                                                    IntByReference exchangeItemId, IntByReference locationIndex,
                                                    IntByReference layerIndex,
                                                    DoubleByReference beginTime, DoubleByReference endTime,
                                                    IntByReference valueCountRef, double[] values);

     
    /**
     * Get the number of time series for a forcing or boundary exchange item.
     *
     * @param modelInstanceId (In:)  The model instance identifier
     * @param exchangeItemId  (In:)  Integer identifying the exchange item
     * @return <0: error, >=0: the number of time series for the exchange item.
     */
    int m_openda_wrapper_get_time_series_count_(IntByReference modelInstanceId, IntByReference exchangeItemId);
    
    /**
     * Get the number of time series for a forcing or boundary exchange item.
     *
     * @param modelInstanceId (In:)  The model instance identifier
     * @return <0: error, >=0: The number of x-species.
     */
    int m_openda_wrapper_get_xspecies_count_(IntByReference modelInstanceId);


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
    int m_openda_wrapper_get_values_(IntByReference modelInstanceId, IntByReference exchangeItemId,
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
    int m_openda_wrapper_set_values_(IntByReference modelInstanceId, IntByReference exchangeItemId,
                                      IntByReference startIndex, IntByReference endIndex,
                                      double[] values);

    /**
     * Check the if the exchange item is supported by the current EFDC configuration.
     *
     * @param modelInstanceId (In:)  The model instance identifier
     * @param exchangeItemId  (In:)  Integer identifying the exchange item
     * @return <0: error, = 0: no, 1 = yes 
     */
    int m_openda_wrapper_supports_exchange_item_(  IntByReference modelInstanceId,
    											   IntByReference exchangeItemId);


    /** 
     * Perform time steps with the model
     * 
     * @param fromTimeStamp Time stamp to compute from (usually equal to model's current time)
     * @param toTimeStamp   Time stamp to compute to
     * @return 0: success, <0: error
     */
    int m_openda_wrapper_compute_(  IntByReference modelInstanceId, 
                                    DoubleByReference fromTimeStamp, 
                                    DoubleByReference toTimeStamp);

    /** 
     * Store restart files for the current state
     * 
     * @return 0: success, <0: error
     */
    int m_openda_wrapper_store_current_instance_restart_files_();
    
    /** 
     * Put the model instance in memory with a state that is read from a restart file
     * 
     * @param modelInstanceId   identifier of the instance to be selected
     * @return 0: success, <0: error
     */
    int m_openda_wrapper_select_instance_from_restart_files_(
                IntByReference modelInstanceId);
    
    /**
     * Finalize a model instance
     *
     * @param modelInstanceId The identifier of the model instance to be finalized
     * @return 0: success, <0: error
     */
    int m_openda_wrapper_finish_(IntByReference modelInstanceId);

    /**
     * @return The size of the model state
     */
    int m_openda_wrapper_get_state_size_();
    
    /**
     * Finalize a model static data
     *
     * @return 0: success, <0: error
     */
    int m_openda_wrapper_destroy_();
    
}

