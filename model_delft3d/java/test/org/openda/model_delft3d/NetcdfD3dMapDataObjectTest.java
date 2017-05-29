package org.openda.model_delft3d;

import junit.framework.TestCase;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.util.Arrays;

//import org.openda.exchange.dataobjects.NetcdfD3dGridTimeSeriesExchangeItem;
//import org.openda.exchange.dataobjects.NetcdfD3dHisExchangeItemTest;

/**
 * Created by Theo on 06.07.2016.
 */
public class NetcdfD3dMapDataObjectTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	public void setUp() throws Exception {
		this.testData = new OpenDaTestSupport(NetcdfD3dMapDataObjectTest.class, "model_delft3d");
		this.testRunDataDir = this.testData.getTestRunDataDir();
	}

	public void testReadGrid() {

		int exchangeItemID = 3;

		IDataObject netcdfFile = new NetcdfD3dMapDataObject();
		netcdfFile.initialize(this.testRunDataDir, new String[]{"trim-cadagno_netcdf.nc"});
		String[] exchangeItemIDs = netcdfFile.getExchangeItemIDs();
		System.out.println(Arrays.toString(exchangeItemIDs));
		assertEquals("#exchange items", 4, exchangeItemIDs.length);
		IExchangeItem exchangeItem = netcdfFile.getDataObjectExchangeItem(exchangeItemIDs[exchangeItemID]);
		assertFalse(exchangeItem == null);


		double[] exchangeItemValues = exchangeItem.getValuesAsDoubles();
		System.out.println("ExchangeItem selected: " + exchangeItemIDs[exchangeItemID]);
//		System.out.println(Arrays.toString(exchangeItemValues));
		System.out.println(exchangeItemValues.length);
//		dataObject.getExchangeItemValues("U1");
//		int timeIndex = 0;
//		double[] itemValues = exchangeItemValues.getValuesAsDoublesForSingleTimeIndex(timeIndex);
//		assertEquals(594, itemValues.length);

		//Testing of the intelligence expanding the grid to the top level
		//1. Test if KFU and KFV flags are interpreted correctly + empty domain in upper layers is filled
		// To do so, need to uncomment the debug lines in the class
		//System.out.println(Arrays.toString(exchangeItemValues));

		//2. Test if we can go back to the right waterlevel
		IExchangeItem exchangeItemNewTemp = netcdfFile.getDataObjectExchangeItem("R1");
		double[] tempValuesCorr = exchangeItemNewTemp.getValuesAsDoubles();

		netcdfFile.finish();

		assertEquals(exchangeItemValues.length,tempValuesCorr.length);
		assertEquals(Arrays.toString(exchangeItemValues),Arrays.toString(tempValuesCorr));

	}

}