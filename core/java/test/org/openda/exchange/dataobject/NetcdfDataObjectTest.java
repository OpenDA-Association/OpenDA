package org.openda.exchange.dataobject;

import junit.framework.Assert;
import junit.framework.TestCase;
import org.openda.exchange.NetcdfScalarTimeSeriesExchangeItem;
import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

/**
 * Created by bos_en on 7-9-2015.
 */
public class NetcdfDataObjectTest extends TestCase {
	private File testRunDataDir;
	private OpenDaTestSupport testData;

	public void setUp() throws Exception {
		this.testData = new OpenDaTestSupport(NetcdfDataObjectTest.class, "core");
		this.testRunDataDir = this.testData.getTestRunDataDir();
	}

	public void testReadTimeSeriesEnsemble() {
		NetcdfDataObject dataObject = new NetcdfDataObject();
		dataObject.initialize(this.testRunDataDir, new String[]{"netcdf_timeseries_ensemble.nc", "true"});
		int[] ensembleIndices = dataObject.getEnsembleMemberIndices();
		assertEquals(3, ensembleIndices.length);
		String[] ensembleIds = dataObject.getEnsembleExchangeItemIds();
		assertEquals(4, ensembleIds.length);
		IExchangeItem item = dataObject.getDataObjectExchangeItem("27.waterlevel", 1);
		assertFalse(item == null);
		double[] itemValues = item.getValuesAsDoubles();
		assertEquals(3, itemValues.length);
	}

	public void testReadGridEnsemble() {
		NetcdfDataObject dataObject = new NetcdfDataObject();
		dataObject.initialize(this.testRunDataDir, new String[]{"netcdf_grid_with_ensemble.nc", "true"});
		//int[] ensembleIndices = dataObject.getEnsembleMemberIndices();
		//assertEquals(3, ensembleIndices.length);
		//String[] ensembleIds = dataObject.getEnsembleExchangeItemIds();
		//assertEquals(4, ensembleIds.length);
		//IExchangeItem item = dataObject.getDataObjectExchangeItem("27.waterlevel", 1);
		//assertFalse(item == null);
		//double[] itemValues = item.getValuesAsDoubles();
		//assertEquals(3, itemValues.length);
	}
}
