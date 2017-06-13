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
public interface D3dFlowWinIfortDll extends Library {

	// General Init/Finish functions (note: these may contain a specific OpenDA part; to be separated)
	int SE_INITIALIZE_OPENDA(String componentId, String schematizationId,
					  int componentIdLen, int schematizationIdLen);
    //for the old d3df-dll.dll :
	int SE_INITIALIZE(String componentId, String schematizationId,
					  int componentIdLen, int schematizationIdLen);
	int SE_FINALIZE_OPENDA(String componentId, String schematizationId,
					int componentIdLen, int schematizationIdLen);


	// Time info / Get / Set and Compute functions

	int SE_GETTIMEHORIZON(String componentId, String schematizationId,
						  DoubleByReference startTime, DoubleByReference endTime,
						  int componentIdLen, int schematizationIdLen);

	void SE_GETCURRENTTIME(String componentId, String schematizationId,
						   DoubleByReference currentTime,
						   int componentIdLen, int schematizationIdLen);

	int SE_GET_EXCHANGE_ITEM_ID_CI(String boundaryId, IntByReference boundaryType,
								   int boundaryIdLen);

	int SE_SET_NOISE_FOR_TIME_SPAN(IntByReference boundaryId,
								   DoubleByReference startTime, DoubleByReference endTime,
								   IntByReference operation,
								   IntByReference nvals, double[] values);

	int SE_GET_VALUES_FOR_TIME_SPAN(IntByReference monitorpointId,
									DoubleByReference startTime, DoubleByReference endTime,
									IntByReference nvals, double[] values);

	int SE_PERFORMTIMESTEP(String componentId, String schematizationId,
						   IntByReference currentTime,
						   int componentIdLen, int schematizationIdLen);

    int SE_GET_OBSERVED_LOCALIZATION(String location, DoubleByReference dist,
                                     IntByReference nvals, double[] values, int locationLen);

    // (OpenDA) The following functions are used for creating and selecting ensemble members.

	int SE_CREATE_INSTANCE();

	void SE_SET_MAX_INSTANCES_IN_MEMORY(IntByReference maxInstances);

	void SE_SELECT_INSTANCE(IntByReference instanceId);

	int SE_STORE_CURRENT_INSTANCE(IntByReference storageLevel);


	// (OpenDA) The following functions are used for ensemble restart purposes.

	int SE_STORE_CURRENT_INSTANCE_RESTARTFILE(String restartFileName, int restartFileNameLen);

	void SE_SELECT_INSTANCE_FROM_RESTARTFILE(IntByReference instanceId, String restartFileName, int restartFileNameLen);


	// (OpenDA) These methods are temporary, and will be removed when D3d returns a real cta_state treevector
	int SE_GET_INSTANCE_SIZE();
	int SE_SET_INSTANCE_CORE_STATE(double[] values, IntByReference nvals);
	int SE_GET_INSTANCE_CORE_STATE(double[] values, IntByReference nvals);

}
