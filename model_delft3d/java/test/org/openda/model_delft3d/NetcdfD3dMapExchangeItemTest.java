package org.openda.model_delft3d;

import junit.framework.TestCase;
import org.openda.exchange.dataobjects.NetcdfD3dHisExchangeItemTest;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import ucar.ma2.Array;

import java.io.IOException;

/**
 * Created by Theo on 20.04.2016.
 */
public class NetcdfD3dMapExchangeItemTest extends TestCase {

	OpenDaTestSupport testData = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(NetcdfD3dMapExchangeItemTest.class,"public","model_delft3d");
	}

	public void testGetValuesAsDoubles() throws Exception {

		NetcdfD3dMapDataObject netcdfFile = new NetcdfD3dMapDataObject();
		netcdfFile.initialize(testData.getTestRunDataDir(), new String[] {"trim-cadagno_netcdf.nc"});
		String[] exchangeItemIDs =  netcdfFile.getExchangeItemIDs();
		assertEquals("#exchange items", 4, exchangeItemIDs.length);

		IExchangeItem exchangeItem = netcdfFile.getDataObjectExchangeItem(exchangeItemIDs[3]);
		Object dataMapSeries = exchangeItem.getValues();

	}

}