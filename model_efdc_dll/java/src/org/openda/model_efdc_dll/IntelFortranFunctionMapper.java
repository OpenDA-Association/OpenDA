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
 * This happens eg because fortran compilers may use different name mangling schemes.
 */
public class IntelFortranFunctionMapper{

	HashMap<String,String> methodMap= new HashMap<String,String>(); 
	
	public IntelFortranFunctionMapper(){
		methodMap.put("m_openda_wrapper_init_","m_openda_wrapper_mp_init_");
		methodMap.put("m_openda_wrapper_destroy_","m_openda_wrapper_mp_destroy_");
		methodMap.put("m_openda_wrapper_finish_","m_openda_wrapper_mp_finish_");
		methodMap.put("m_openda_wrapper_get_model_instance_","m_openda_wrapper_mp_get_model_instance_");
		
	    methodMap.put("m_openda_wrapper_get_reference_year_","m_openda_wrapper_mp_get_reference_year_");
		methodMap.put("m_openda_wrapper_get_delta_t_","m_openda_wrapper_mp_get_delta_t_");
		methodMap.put("m_openda_wrapper_get_reference_period_","m_openda_wrapper_mp_get_reference_period_");
		methodMap.put("m_openda_wrapper_get_start_time_","m_openda_wrapper_mp_get_start_time_");
		methodMap.put("m_openda_wrapper_get_end_time_","m_openda_wrapper_mp_get_end_time_");
		methodMap.put("m_openda_wrapper_get_current_time_","m_openda_wrapper_mp_get_current_time_");
		methodMap.put("m_openda_wrapper_get_layer_depths_","m_openda_wrapper_mp_get_layer_depths_");
		methodMap.put("m_openda_wrapper_get_layer_count_model_","m_openda_wrapper_mp_get_layer_count_for_model_");


		methodMap.put("m_openda_wrapper_save_instance_","m_openda_wrapper_mp_save_instance_");
		methodMap.put("m_openda_wrapper_restore_instance_","m_openda_wrapper_mp_restore_instance_");
		
		methodMap.put("m_openda_wrapper_compute_","m_openda_wrapper_mp_compute_");
		
		methodMap.put("m_openda_wrapper_store_current_instance_restart_files_","m_openda_wrapper_mp_store_current_instance_restart_files_");
	    methodMap.put("m_openda_wrapper_select_instance_from_restart_files_","m_openda_wrapper_mp_select_instance_from_restart_files_");
		
	    methodMap.put("m_openda_wrapper_supports_exchange_item_","m_openda_wrapper_mp_supports_exchange_item_");
		methodMap.put("m_openda_wrapper_get_times_for_ei_","m_openda_wrapper_mp_get_times_for_ei_");
		methodMap.put("m_openda_wrapper_set_times_for_ei_","m_openda_wrapper_mp_set_times_for_ei_");
	    methodMap.put("m_openda_wrapper_get_time_series_count_","m_openda_wrapper_mp_get_time_series_count_");
	    methodMap.put("m_openda_wrapper_get_values_count_","m_openda_wrapper_mp_get_values_count_");
	    methodMap.put("m_openda_wrapper_get_values_","m_openda_wrapper_mp_get_values_");
	    methodMap.put("m_openda_wrapper_set_values_","m_openda_wrapper_mp_set_values_");
		methodMap.put("m_openda_wrapper_get_values_count_for_location_","m_openda_wrapper_mp_get_values_count_for_location_");
		methodMap.put("m_openda_wrapper_get_values_count_for_time_span_","m_openda_wrapper_mp_get_values_count_for_time_span_");
		methodMap.put("m_openda_wrapper_get_values_for_time_span_","m_openda_wrapper_mp_get_values_for_time_span_");
		methodMap.put("m_openda_wrapper_set_values_for_time_span_","m_openda_wrapper_mp_set_values_for_time_span_");
		methodMap.put("m_openda_wrapper_get_layer_count_","m_openda_wrapper_mp_get_layer_count_");
		methodMap.put("m_openda_wrapper_get_cell_count_","m_openda_wrapper_mp_get_cell_count_");
		methodMap.put("m_openda_wrapper_get_times_count_for_location_","m_openda_wrapper_mp_get_times_count_for_location_");
		methodMap.put("m_openda_wrapper_get_times_count_for_time_span_","m_openda_wrapper_mp_get_times_count_for_time_span_");
		methodMap.put("m_openda_wrapper_get_layer_count_for_location_","m_openda_wrapper_mp_get_layer_count_for_location_");
		methodMap.put("m_openda_wrapper_get_layer_count_for_time_span_","m_openda_wrapper_mp_get_layer_count_for_time_span_");
	}
	
	public HashMap<String,String> getMap(){
		return this.methodMap;
	}

}
