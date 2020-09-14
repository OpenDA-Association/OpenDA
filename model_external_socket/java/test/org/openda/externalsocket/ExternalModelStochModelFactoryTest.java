package org.openda.externalsocket;

import junit.framework.TestCase;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class ExternalModelStochModelFactoryTest extends TestCase {

	private File testRunDataDir;

	protected void setUp() {
		OpenDaTestSupport testData = new OpenDaTestSupport(ExternalModelStochModelFactoryTest.class, "model_external_socket");
		testRunDataDir = testData.getTestRunDataDir();
	}


	public void testReadConfig() {
		ExternalSocketModelFactory externalModelStochModelFactory = new ExternalSocketModelFactory();
		externalModelStochModelFactory.initialize(testRunDataDir, new String[]{"ExternalSocketModelFactory.xml"});
		ExternalSocketModelInstance stochModelInstance = externalModelStochModelFactory.getInstance(new String[0], IStochModelFactory.OutputLevel.Suppress);
		IVector parameters = stochModelInstance.getParameters();
		int size = parameters.getSize();
		assertEquals(3, size);
		for (int i = 0; i < size; i++) {
			assertEquals((double) i + 1, parameters.getValue(i));
		}
	}

}
