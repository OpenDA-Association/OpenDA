package org.openda.externalfile;

import junit.framework.TestCase;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class ExternalFileModelFactoryTest extends TestCase {

	private File testRunDataDir;

	protected void setUp() {
		OpenDaTestSupport testData = new OpenDaTestSupport(ExternalFileModelFactoryTest.class, "model_external_file");
		testRunDataDir = testData.getTestRunDataDir();
	}


	public void testReadConfig() {
		ExternalFileModelFactory externalModelStochModelFactory = new ExternalFileModelFactory();
		externalModelStochModelFactory.initialize(testRunDataDir, new String[]{"ExternalFileModelFactory.xml"});
		ExternalFileModelInstance modelInstance = externalModelStochModelFactory.getInstance(new String[0], IStochModelFactory.OutputLevel.Suppress);

		IVector parameters = modelInstance.getParameters();
		assertEquals(3, parameters.getSize());
		assertEquals(10.0, parameters.getValue(0));
		assertEquals(20.0, parameters.getValue(1));
		assertEquals(30.0, parameters.getValue(2));
	}

}
