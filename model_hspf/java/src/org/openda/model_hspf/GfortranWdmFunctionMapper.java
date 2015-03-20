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

package org.openda.model_hspf;

import java.util.HashMap;

/**
 * If the names in a native library are not the same as in java, then you can map these names.
 * This happens e.g. because fortran compilers may use different nameMangling schemes.
 *
 * @author Arno Kockx
 */
public class GfortranWdmFunctionMapper {
    HashMap<String, String> methodMap = new HashMap<String, String>();

    public GfortranWdmFunctionMapper() {
        methodMap.put("wdbopn_", "wdbopn_");
        methodMap.put("wdflcl_", "wdflcl_");
        methodMap.put("wdtget_", "wdtget_");
        methodMap.put("wdtput_", "wdtput_");
        methodMap.put("wtddel_", "wtddel_");
        methodMap.put("wdatim_", "wdatim_");
        methodMap.put("wdckdt_", "wdckdt_");
        methodMap.put("wddsnx_", "wddsnx_");
        methodMap.put("wdbsgc_", "wdbsgc_");
        methodMap.put("timdif_", "timdif_");
        methodMap.put("timadd_", "timadd_");
        methodMap.put("wdm_open_", "wdm_open_");
        methodMap.put("wdm_close_", "wdm_close_");
    }

    public HashMap<String, String> getMap() {
        return this.methodMap;
    }
}
