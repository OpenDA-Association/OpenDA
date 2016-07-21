package org.openda.model_openfoam;

import junit.framework.TestCase;
import org.openda.exchange.QuantityInfo;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.utils.Array;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class UnstructuredMeshGeometryInfoTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(UnstructuredMeshGeometryInfoTest.class,"model_openfoam");
		testRunDataDir = testData.getTestRunDataDir();
	}


	public void testGetCoordinates()  {

		IQuantityInfo[] coordinates = new QuantityInfo[3];
		coordinates[0] = new QuantityInfo("x","m");
		coordinates[1] = new QuantityInfo("y","m");
		coordinates[2] = new QuantityInfo("z","m");

		//IArray points = Array( );
		IGeometryInfo geometryInfo = new UnstructuredMeshGeometryInfo(coordinates);
	}

	public void testSetCoordinates()  {

	}

	public void testGetPoints()  {

	}

	public void testSetPoints() {

	}
}
