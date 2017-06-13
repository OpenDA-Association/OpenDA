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
public class D3dFlowExchangeItemConfig {

    // Note: the values of the final int's must be equal to the related values in the f90 module m_openda_quantities

    // output from D3D:
    public static final int EI_waterlevel = 1;

    // input to D3D:
	public static final int EI_unknown_type = -1;
    public static final int EI_src_discharge = 6;
    public static final int EI_wind_u= 7;           // scalar noise to modify an entire wind field
    public static final int EI_wind_v= 8;           // scalar noise to modify an entire wind field
    public static final int EI_bound_HQ = 9;
    public static final int EI_bound_temp = 10;
    public static final int EI_bound_salt = 11;
	public static final int EI_bound_astroH = 12;
	public static final int EI_wind_gu = 13;        // noise field on a curvilinear grid.
	public static final int EI_wind_gv = 16;        // noise field on a curvilinear grid.


	// operations:
	public static final int OPER_set = 1;
	public static final int OPER_add = 2;
	public static final int OPER_multiply = 3;

    private String id;
    private int type;
    private String metatype;

	public D3dFlowExchangeItemConfig(String id, String type) {
        this.id = id;
		this.type = EI_unknown_type;
		if (type != null) {
			if (type.equalsIgnoreCase("bound_hq")) {
				this.type = EI_bound_HQ;
				this.metatype = "Forcings";
			} else if (type.equalsIgnoreCase("bound_astroh")) {
				this.type = EI_bound_astroH;
				this.metatype = "Forcings";
			} else if (type.equalsIgnoreCase("src_discharge")) {
					this.type = EI_src_discharge;
					this.metatype = "Forcings";
			} else if (type.equalsIgnoreCase("bound_temp")) {
					this.type = EI_bound_temp;
					this.metatype = "Forcings";
			} else if (type.equalsIgnoreCase("bound_salt")) {
					this.type = EI_bound_salt;
					this.metatype = "Forcings";
			} else if (type.equalsIgnoreCase("wind_u")) {
					this.type = EI_wind_u;
					this.metatype = "Forcings";
			} else if (type.equalsIgnoreCase("wind_v")) {
					this.type = EI_wind_v;
					this.metatype = "Forcings";
			} else if (type.equalsIgnoreCase("wind_gu")) {
					this.type = EI_wind_gu;
					this.metatype = "Forcings";
			} else if (type.equalsIgnoreCase("wind_gv")) {
					this.type = EI_wind_gv;
					this.metatype = "Forcings";
			} else if (type.equalsIgnoreCase("waterlevel")) {
					this.type = EI_waterlevel;
					this.metatype = "Monitor";
			} else {
				throw new RuntimeException(
						"D3dFlowModelConfig: unknown exchange item type: " + type);
			}
		}
	}

    public String getId() {
        return id;
    }

    public int getType() {
		if (type == EI_unknown_type) {
			throw new RuntimeException(
					"D3dFlowModelConfig: exchange item type not specified");
		}
        return type;
    }

    public String getMetatype() {
        return metatype;
    }
}
