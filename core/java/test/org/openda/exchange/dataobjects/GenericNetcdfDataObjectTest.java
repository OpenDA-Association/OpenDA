package org.openda.exchange.dataobjects;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class GenericNetcdfDataObjectTest extends TestCase {
	private OpenDaTestSupport testData;
	private File testRunDataDir;

	public void setUp() throws Exception {
		this.testData = new OpenDaTestSupport(GenericNetcdfDataObjectTest.class, "core");
		this.testRunDataDir = this.testData.getTestRunDataDir();
	}

	public void testReadWriteArrayDataObject(){
		GenericNetcdfDataObject genNcDObject = new GenericNetcdfDataObject();
		genNcDObject.initialize(this.testRunDataDir, new String[]{"mapfile.nc", "delft3d_map_nc.xml"});
		String ids[]=genNcDObject.getExchangeItemIDs();
		assertEquals(3, ids.length);

		// check exchangeItemId and values
		IExchangeItem vel_v = genNcDObject.getDataObjectExchangeItem("VELV.SurfaceLayerSection");
		double[] vel_v_values = vel_v.getValuesAsDoubles();
		assertEquals(50,vel_v_values.length);
		assertEquals(0.002449778141453862, vel_v_values[44]);
		assertEquals(0.0039200084283947945, vel_v_values[49]);

		IExchangeItem vel_u = genNcDObject.getDataObjectExchangeItem("VELU.SurfaceLayer");
		double[] vel_u_values = vel_u.getValuesAsDoubles();
		assertEquals(594,vel_u_values.length);
		assertEquals(-0.008233560249209404,vel_u_values[244]);
		assertEquals(-0.024448467418551445,vel_u_values[334]);

		IExchangeItem waterlevel = genNcDObject.getDataObjectExchangeItem("Waterlevel.Grid");
		double[] waterlevel_values = waterlevel.getValuesAsDoubles();
		assertEquals(594,waterlevel_values.length);
		assertEquals(-0.0011382763041183352,waterlevel_values[62]);
		assertEquals(-0.0010056854225695133, waterlevel_values[81]);

		// modify data
		vel_v.axpyOnValues(5000.0,vel_v_values);
		for (int i=0; i<vel_v_values.length; i++){
			vel_v_values[i] = 5000.0;
		}
		vel_v.setValuesAsDoubles(vel_v_values);
		vel_v_values = vel_v.getValuesAsDoubles();
		assertEquals(5000.0,vel_v_values[39],0.000000001);

		vel_u.axpyOnValues(1.0,vel_u_values);

		vel_u_values = vel_u.getValuesAsDoubles();
		assertEquals(2.0*-0.008233560249209404,vel_u_values[244], 0.000000001);

		waterlevel.axpyOnValues(2.0,waterlevel_values);
		waterlevel_values = waterlevel.getValuesAsDoubles();
		assertEquals(3.0*-0.0011382763041183352,waterlevel_values[62],0.000000001);
		genNcDObject.finish();

		// test reading file after modification: are the modification successfully written to netCDF file?
		GenericNetcdfDataObject genNcDObject2 = new GenericNetcdfDataObject();
		genNcDObject2.initialize(this.testRunDataDir, new String[]{"mapfile.nc", "delft3d_map_nc.xml"});
		IExchangeItem vel_v2 = genNcDObject2.getDataObjectExchangeItem("VELV.SurfaceLayerSection");
		double[] vel_v_values2 = vel_v2.getValuesAsDoubles();
		assertEquals(5000.0,vel_v_values2[39],0.000000001);

		IExchangeItem vel_u2 = genNcDObject2.getDataObjectExchangeItem("VELU.SurfaceLayer");
		double[] vel_u_values2 = vel_u2.getValuesAsDoubles();
		assertEquals(3.0*-0.0011382763041183352,waterlevel_values[62],0.000000001);

		IExchangeItem waterlevel2 = genNcDObject2.getDataObjectExchangeItem("Waterlevel.Grid");
		double[] waterlevel_values2 = waterlevel2.getValuesAsDoubles();
		assertEquals(3.0*-0.0011382763041183352,waterlevel_values2[62],0.000000001);
		assertEquals(3.0*-0.0010056854225695133, waterlevel_values2[81],0.000000001);

		genNcDObject2.finish();
	}

	public void testReadTimeSeriesFromScalarDataObject(){
		// no test of modifying values here (like axpy or set). For the moment, we don't support writing of timeseries EI to netCDF.
		GenericNetcdfDataObject genNcDObject = new GenericNetcdfDataObject();
		genNcDObject.initialize(this.testRunDataDir, new String[]{"trih-cadagno_netcdf.nc", "delft3d_his_nc.xml"});
		String ids[]=genNcDObject.getExchangeItemIDs();
		assertEquals(54, ids.length);

		// check exchangeItemId and values
		IExchangeItem station3_velv_layer14 = genNcDObject.getDataObjectExchangeItem("foo.station3.layer_14.v_velocity");
		double[] vel_v_values = station3_velv_layer14.getValuesAsDoubles();
		assertEquals(17,vel_v_values.length);
		assertEquals(0.007575964089483023, vel_v_values[1]);
		assertEquals(0.005792300216853619, vel_v_values[6]);

		IExchangeItem stat4_wl_eI = genNcDObject.getDataObjectExchangeItem("freetext1.station4.freetext2.waterlevel.freetext3");
		double[] stat4_wl = stat4_wl_eI.getValuesAsDoubles();
		assertEquals(17,stat4_wl.length);
		assertEquals(-3.1849416700424626E-5, stat4_wl[1]);
		assertEquals(-2.5959964841604233E-4, stat4_wl[6]);

	}

	public void testReadTimeSeriesFromGridDataObject(){
		// no test of modifying values here (like axpy or set). For the moment, we don't support writing of timeseries EI to netCDF.
		GenericNetcdfDataObject genNcDObject = new GenericNetcdfDataObject();
		genNcDObject.initialize(this.testRunDataDir, new String[]{"mapfile.nc", "delft3d_timeseries_from_map_nc.xml"});
		String ids[]=genNcDObject.getExchangeItemIDs();
		assertEquals(2, ids.length);

		// check exchangeItemId and values
		IExchangeItem vel_v = genNcDObject.getDataObjectExchangeItem("V1_velocity.Station_n10_m12");
		double[] vel_v_values = vel_v.getValuesAsDoubles();
		assertEquals(4,vel_v_values.length);
		assertEquals(0.007391710765659809, vel_v_values[1]);
		assertEquals(0.002930134069174528, vel_v_values[3]);

		IExchangeItem wl_ei = genNcDObject.getDataObjectExchangeItem("Waterlevel.Station_n10_m12");
		double[] wl = wl_ei.getValuesAsDoubles();
		assertEquals(4,wl.length);
		assertEquals(-2.745141973719001E-4, wl[1]);
		assertEquals(-1.7212324019055814E-4, wl[3]);

	}

}
