/* OpenDA v2.4 
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
package org.openda.model_delft3d.dll;
import com.sun.jna.Library;
import com.sun.jna.ptr.DoubleByReference;
import com.sun.jna.ptr.IntByReference;

/**
 * Interface specification for D3D Flow
 */
public interface D3dFlowLinuxGnuDll extends Library {

	// General Init/Finish functions (note: these may contain a specific OpenDA part; to be separated)
	int se_initialize_openda_(String componentId, String schematizationId,
					  int componentIdLen, int schematizationIdLen);
    //for the old d3df-dll.dll :
	int se_initialize_(String componentId, String schematizationId,
					  int componentIdLen, int schematizationIdLen);
	int se_finalize_openda_(String componentId, String schematizationId,
					int componentIdLen, int schematizationIdLen);


	// Time info / Get / Set and Compute functions

	int se_gettimehorizon_(String componentId, String schematizationId,
						  DoubleByReference startTime, DoubleByReference endTime,
						  int componentIdLen, int schematizationIdLen);

	void se_getcurrenttime_(String componentId, String schematizationId,
						   DoubleByReference currentTime,
						   int componentIdLen, int schematizationIdLen);

	int se_get_exchange_item_id_ci_(String boundaryId, IntByReference boundaryType,
								   int boundaryIdLen);

	int se_set_noise_for_time_span_(IntByReference boundaryId,
								   DoubleByReference startTime, DoubleByReference endTime,
								   IntByReference operation,
								   IntByReference nvals, double[] values);

	int se_get_values_for_time_span_(IntByReference monitorpointId,
									DoubleByReference startTime, DoubleByReference endTime,
									IntByReference nvals, double[] values);

	int se_performtimestep_(String componentId, String schematizationId,
						   IntByReference currentTime,
						   int componentIdLen, int schematizationIdLen);

    int se_get_observed_localization_(String location, DoubleByReference dist,
                                     IntByReference nvals, double[] values, int locationLen);

    // (OpenDA) The following functions are used for creating and selecting ensemble members.

	int se_create_instance_();

	void se_set_max_instances_in_memory_(IntByReference maxInstances);

	void se_select_instance_(IntByReference instanceId);

	int se_store_current_instance_(IntByReference storageLevel);


	// (OpenDA) The following functions are used for ensemble restart purposes.

	int se_store_current_instance_restartfile_(String restartFileName, int restartFileNameLen);

	void se_select_instance_from_restartfile_(IntByReference instanceId, String restartFileName, int restartFileNameLen);


	// (OpenDA) These methods are temporary, and will be removed when D3d returns a real cta_state treevector
	int se_get_instance_size_();
	int se_set_instance_core_state_(double[] values, IntByReference nvals);
	int se_get_instance_core_state_(double[] values, IntByReference nvals);

}
