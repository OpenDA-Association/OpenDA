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
import java.util.HashMap;

/**
 * If the names in a native library are not the same as in java, then you can map these names.
 * This happens eg because fortran compilers may use different nameMangling schemes.
 * @author verlaanm
 *
 */
public class GfortranFunctionMapper{

	HashMap<String,String> methodMap= new HashMap<String,String>(); 
	
	public GfortranFunctionMapper(){
		methodMap.put("m_simple_model_mp_init_","__m_simple_model_MOD_init");
		methodMap.put("m_simple_model_mp_get_model_instance_","__m_simple_model_MOD_get_model_instance");
		methodMap.put("m_simple_model_mp_save_instance_","__m_simple_model_MOD_save_instance");
		methodMap.put("m_simple_model_mp_restore_instance_","__m_simple_model_MOD_restore_instance");
		methodMap.put("m_simple_model_mp_get_start_time_","__m_simple_model_MOD_get_start_time");
		methodMap.put("m_simple_model_mp_get_end_time_","__m_simple_model_MOD_get_end_time");
		methodMap.put("m_simple_model_mp_get_delta_t_","__m_simple_model_MOD_get_delta_t");
		methodMap.put("m_simple_model_mp_get_current_time_","__m_simple_model_MOD_get_current_time");
		methodMap.put("m_simple_model_mp_get_values_count_for_time_span_","__m_simple_model_MOD_get_values_count_for_time_span");
		methodMap.put("m_simple_model_mp_get_values_for_time_span_","__m_simple_model_MOD_get_values_for_time_span");
		methodMap.put("m_simple_model_mp_set_values_for_time_span_","__m_simple_model_MOD_set_values_for_time_span");
		methodMap.put("m_simple_model_mp_get_values_count_","__m_simple_model_MOD_get_values_count");
		methodMap.put("m_simple_model_mp_get_values_","__m_simple_model_MOD_get_values");
		methodMap.put("m_simple_model_mp_set_values_","__m_simple_model_MOD_set_values");
		methodMap.put("m_simple_model_mp_compute_","__m_simple_model_MOD_compute");
		methodMap.put("m_simple_model_mp_finish_","__m_simple_model_MOD_finish");
		methodMap.put("m_simple_model_mp_get_state_size_",""); //Problematic!!!
		methodMap.put("m_simple_model_mp_apply_obs_tangent","");
		methodMap.put("m_simple_model_mp_apply_obs_adjoint","");
		
	}
	
	public HashMap<String,String> getMap(){
		return this.methodMap;
	}

}

//00007eb0 a _DYNAMIC
//00007ff4 a _GLOBAL_OFFSET_TABLE_
//         w _Jv_RegisterClasses
//00007ea0 d __CTOR_END__
//00007e9c d __CTOR_LIST__
//00007ea8 d __DTOR_END__
//00007ea4 d __DTOR_LIST__
//00006698 r __FRAME_END__
//00007eac d __JCR_END__
//00007eac d __JCR_LIST__
//00008118 A __bss_start
//         w __cxa_finalize@@GLIBC_2.1.3
//000060d0 t __do_global_ctors_aux
//000019c0 t __do_global_dtors_aux
//000080a0 d __dso_handle
//         w __gmon_start__
//00001a77 t __i686.get_pc_thunk.bx
//000060cc t __i686.get_pc_thunk.cx
//000026c9 T __m_model_instance_MOD_mi_create
//00002615 T __m_model_instance_MOD_mi_destroy
//00001a7c T __m_model_instance_MOD_mi_read_from_file
//00002058 T __m_model_instance_MOD_mi_write_to_file
//00008128 B __m_model_instance_MOD_model_instance_count
//0000812c B __m_model_instance_MOD_model_instance_currently_in_memory
//00002f9b T __m_simple_model_MOD_check_bc_indices
//00003100 T __m_simple_model_MOD_check_grid_indices
//000028c8 T __m_simple_model_MOD_check_model_instance_dirs_size
//0000330b T __m_simple_model_MOD_compute
//00005d87 T __m_simple_model_MOD_destroy
//00008140 B __m_simple_model_MOD_dm_bc_delta_t_in_seconds
//00008144 B __m_simple_model_MOD_dm_bc_timestep_count
//00008148 B __m_simple_model_MOD_dm_current_time
//00008150 B __m_simple_model_MOD_dm_delta_t_in_seconds
//00008158 B __m_simple_model_MOD_dm_depths
//00008180 B __m_simple_model_MOD_dm_discharges
//000081a8 B __m_simple_model_MOD_dm_end_time_as_mjd
//000081b0 B __m_simple_model_MOD_dm_frictions
//000081c8 B __m_simple_model_MOD_dm_gravity
//000081d0 B __m_simple_model_MOD_dm_gridpoint_count
//000081d4 B __m_simple_model_MOD_dm_lateral_count
//000081d8 B __m_simple_model_MOD_dm_max_dm_model_instance_count
//000081dc B __m_simple_model_MOD_dm_model_instance_count
//000081e0 B __m_simple_model_MOD_dm_model_instance_in_memory
//00008200 B __m_simple_model_MOD_dm_model_parent_dir
//00008300 B __m_simple_model_MOD_dm_outfile_handle
//00008320 B __m_simple_model_MOD_dm_schematization_file
//00008420 B __m_simple_model_MOD_dm_start_time_as_mjd
//00008440 B __m_simple_model_MOD_dm_template_model_dir
//00008540 B __m_simple_model_MOD_dm_waterlevels
//000080c0 D __m_simple_model_MOD_exchange_item_text
//00003227 T __m_simple_model_MOD_finish
//00004b1d T __m_simple_model_MOD_get_current_time
//00004c1d T __m_simple_model_MOD_get_delta_t
//00004d2d T __m_simple_model_MOD_get_end_time
//00005399 T __m_simple_model_MOD_get_model_instance
//00002e0e T __m_simple_model_MOD_get_model_instance_from_memory
//00004e2d T __m_simple_model_MOD_get_start_time
//00003943 T __m_simple_model_MOD_get_values
//00003dd4 T __m_simple_model_MOD_get_values_count
//00004842 T __m_simple_model_MOD_get_values_count_for_time_span
//00004439 T __m_simple_model_MOD_get_values_for_time_span
//00005edb T __m_simple_model_MOD_init
//00008558 B __m_simple_model_MOD_model_instance_dirs
//00004f2d T __m_simple_model_MOD_restore_instance
//0000515e T __m_simple_model_MOD_save_instance
//00002c24 T __m_simple_model_MOD_set_model_instance_to_memory
//00003498 T __m_simple_model_MOD_set_values
//0000401f T __m_simple_model_MOD_set_values_for_time_span
//000031d7 T __m_simple_model_MOD_valid_model_instance
//00008118 A _edata
//00008588 A _end
//00006108 T _fini
//         U _gfortran_concat_string@@GFORTRAN_1.0
//         U _gfortran_flush_i4@@GFORTRAN_1.0
//         U _gfortran_os_error@@GFORTRAN_1.0
//         U _gfortran_runtime_error@@GFORTRAN_1.0
//         U _gfortran_runtime_error_at@@GFORTRAN_1.0
//         U _gfortran_st_close@@GFORTRAN_1.0
//         U _gfortran_st_open@@GFORTRAN_1.0
//         U _gfortran_st_read@@GFORTRAN_1.0
//         U _gfortran_st_read_done@@GFORTRAN_1.0
//         U _gfortran_st_write@@GFORTRAN_1.0
//         U _gfortran_st_write_done@@GFORTRAN_1.0
//         U _gfortran_string_trim@@GFORTRAN_1.0
//         U _gfortran_transfer_array@@GFORTRAN_1.0
//         U _gfortran_transfer_character@@GFORTRAN_1.0
//         U _gfortran_transfer_integer@@GFORTRAN_1.0
//         U _gfortran_transfer_real@@GFORTRAN_1.0
//0000175c T _init
//00008120 b completed.5522
//00008124 b dtor_idx.5524
//00008110 d epsilon.1782
//00001a40 t frame_dummy
//         U free@@GLIBC_2.0
//000080a8 d instance_file_handle.1544
//000080a4 d instance_file_handle.1563
//         U malloc@@GLIBC_2.0
//         U memmove@@GLIBC_2.0
//         U memset@@GLIBC_2.0
//00008570 b org_model_instance_dirs.1683
//         U trunc@@GLIBC_2.1

