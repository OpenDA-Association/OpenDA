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
package org.openda.model_efdc_dll;
import java.util.HashMap;

/**
 * If the names in a native library are not the same as in java, then you can map these names.
 * This happens eg because fortran compilers may use different nameMangling schemes.
 */
public class GfortranFunctionMapper{

	HashMap<String,String> methodMap= new HashMap<String,String>(); 
	
	public GfortranFunctionMapper(){
	    methodMap.put("m_openda_wrapper_init_","__m_openda_wrapper_MOD_init");
	    methodMap.put("m_openda_wrapper_destroy_","__m_openda_wrapper_MOD_destroy");
	    methodMap.put("m_openda_wrapper_finish_","__m_openda_wrapper_MOD_finish");
	    methodMap.put("m_openda_wrapper_get_model_instance_","__m_openda_wrapper_MOD_get_model_instance");

	    methodMap.put("m_openda_wrapper_get_reference_year_","__m_openda_wrapper_MOD_get_reference_year");
	    methodMap.put("m_openda_wrapper_get_delta_t_","__m_openda_wrapper_MOD_get_delta_t");
	    methodMap.put("m_openda_wrapper_get_reference_period_","__m_openda_wrapper_MOD_get_reference_period");
	    methodMap.put("m_openda_wrapper_get_start_time_","__m_openda_wrapper_MOD_get_start_time");
	    methodMap.put("m_openda_wrapper_get_end_time_","__m_openda_wrapper_MOD_get_end_time");
	    methodMap.put("m_openda_wrapper_get_current_time_","__m_openda_wrapper_MOD_get_current_time");
		methodMap.put("m_openda_wrapper_get_layer_depths_","__m_openda_wrapper_MOD_get_layer_depths");
		methodMap.put("m_openda_wrapper_get_layer_count_for_model_","__m_openda_wrapper_MOD_get_layer_count_for_model");

		methodMap.put("m_openda_wrapper_save_instance_","__m_openda_wrapper_MOD_save_instance");
	    methodMap.put("m_openda_wrapper_restore_instance_","__m_openda_wrapper_MOD_restore_instance");
	    
	    methodMap.put("m_openda_wrapper_compute_","__m_openda_wrapper_MOD_compute");
	    
	    methodMap.put("m_openda_wrapper_store_current_instance_restart_files_","__m_openda_wrapper_MOD_store_current_instance_restart_files");
	    methodMap.put("m_openda_wrapper_select_instance_from_restart_files_","__m_openda_wrapper_MOD_select_instance_from_restart_files");

	    methodMap.put("m_openda_wrapper_supports_exchange_item_","__m_openda_wrapper_MOD_supports_exchange_item");
	    methodMap.put("m_openda_wrapper_get_times_for_ei_","__m_openda_wrapper_MOD_get_times_for_ei");
	    methodMap.put("m_openda_wrapper_set_times_for_ei_","__m_openda_wrapper_MOD_set_times_for_ei");
	    methodMap.put("m_openda_wrapper_get_time_series_count_","__m_openda_wrapper_MOD_get_time_series_count");
		methodMap.put("m_openda_wrapper_get_values_count_","__m_openda_wrapper_MOD_get_values_count");
	    methodMap.put("m_openda_wrapper_get_values_","__m_openda_wrapper_MOD_get_values");
	    methodMap.put("m_openda_wrapper_set_values_","__m_openda_wrapper_MOD_set_values");
	    methodMap.put("m_openda_wrapper_get_values_count_for_location_","__m_openda_wrapper_MOD_get_values_count_for_location");
	    methodMap.put("m_openda_wrapper_get_values_count_for_time_span_","__m_openda_wrapper_MOD_get_values_count_for_time_span");
	    methodMap.put("m_openda_wrapper_get_values_for_time_span_","__m_openda_wrapper_MOD_get_values_for_time_span");
	    methodMap.put("m_openda_wrapper_set_values_for_time_span_","__m_openda_wrapper_MOD_set_values_for_time_span");
		methodMap.put("m_openda_wrapper_get_layer_count_","__m_openda_wrapper_MOD_get_layer_count");
		methodMap.put("m_openda_wrapper_get_cell_count_","__m_openda_wrapper_MOD_get_cell_count");
		methodMap.put("m_openda_wrapper_get_times_count_for_location_","__m_openda_wrapper_MOD_get_times_count_for_location");
		methodMap.put("m_openda_wrapper_get_times_count_for_time_span_","__m_openda_wrapper_MOD_get_times_count_for_time_span");
		methodMap.put("m_openda_wrapper_get_layer_count_for_location_","__m_openda_wrapper_MOD_get_layer_count_for_location");
		methodMap.put("m_openda_wrapper_get_layer_count_for_time_span_","__m_openda_wrapper_MOD_get_layer_count_for_time_span");
	}
	
	public HashMap<String,String> getMap(){
		return this.methodMap;
	}

}
