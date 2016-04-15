package org.openda.exchange.dataobjects;

import junit.framework.TestCase;
import org.openda.exchange.ExchangeItem;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.IOException;

/**
 * Created by Theo on 13.04.2016.
 */
public class NetcdfD3dHisExchangeItemTest extends TestCase {

	OpenDaTestSupport testData = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(NetcdfD3dHisExchangeItemTest.class,"public","core");
	}

	public void testGetValuesAsDoubles() throws Exception {

		NetcdfD3dHisDataObject netcdfFile = new NetcdfD3dHisDataObject();
		netcdfFile.initialize(testData.getTestRunDataDir(), new String[] {"trih-cadagno_netcdf.nc"});
		String[] exchangeItemIDs =  netcdfFile.getExchangeItemIDs();

		assertEquals("#exchange items", 100, exchangeItemIDs.length);

		IExchangeItem exchangeItem = netcdfFile.getDataObjectExchangeItem(exchangeItemIDs[27]);

		double[] dataTimeSeries = exchangeItem.getValuesAsDoubles();

		assertEquals("#time steps", 737, dataTimeSeries.length);

	}

}