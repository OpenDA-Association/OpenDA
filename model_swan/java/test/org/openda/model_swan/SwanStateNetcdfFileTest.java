package org.openda.model_swan;

import junit.framework.TestCase;
import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Created by IntelliJ IDEA.
 * User: Julius Sumihar
 * Date: 9-7-12
 * Time: 14:12
 * To change this template use File | Settings | File Templates.
 */
public class SwanStateNetcdfFileTest extends TestCase {
	OpenDaTestSupport testData = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(SwanStateNetcdfFileTest.class, "model_swan");
	}

	    public void testSwanNetcdfStateFile_1() throws IOException {
			File stateFilesTestDir = new File(testData.getTestRunDataDir(), "StateFiles");
			NetcdfDataObject swanStateNetcdf = new NetcdfDataObject();
			swanStateNetcdf.initialize(stateFilesTestDir,new String[]{"s00nc_a.nc"});
			IExchangeItem waveSpectrum = swanStateNetcdf.getDataObjectExchangeItem("sea_surface_wave_directional_variance_spectral_density");
			double[] values = waveSpectrum.getValuesAsDoubles();
			assertEquals("values[9]",0.0,0.0,1e-8);
			assertEquals("values[10]",0.0008735809,values[10],1e-8);
			assertEquals("values[11]",0.00279943,values[11],1e-8);
			values[9] = 0.00456789;
			values[10] = 2000.01;
			waveSpectrum.setValuesAsDoubles(values);
			swanStateNetcdf.finish();
			NetcdfDataObject swanStateNetcdf2 = new NetcdfDataObject();
			swanStateNetcdf2.initialize(stateFilesTestDir,new String[]{"s00nc_a.nc"});
			waveSpectrum = swanStateNetcdf2.getDataObjectExchangeItem("sea_surface_wave_directional_variance_spectral_density");
			double[] values2 = waveSpectrum.getValuesAsDoubles();
			assertEquals("values[9]",values[9],values2[9],1e-8);
			assertEquals("values[10]",values[10],values2[10],1e-4);
			assertEquals("values[11]",0.00279943,values[11],1e-8);
			swanStateNetcdf2.finish();
		}

}
