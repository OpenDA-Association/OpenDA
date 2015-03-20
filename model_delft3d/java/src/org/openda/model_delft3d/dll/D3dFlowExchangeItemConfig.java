package org.openda.model_delft3d.dll;

/**
* Created by IntelliJ IDEA.
* User: Stef Hummel
* Date: Aug 6, 2010
* Time: 2:24:01 PM
* To change this template use File | Settings | File Templates.
*/
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
