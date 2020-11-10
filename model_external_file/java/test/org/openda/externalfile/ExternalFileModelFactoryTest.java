package org.openda.externalfile;

import junit.framework.TestCase;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class ExternalFileModelFactoryTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() {
		testData = new OpenDaTestSupport(ExternalFileModelFactoryTest.class, "model_fews_workflow");
		testRunDataDir = testData.getTestRunDataDir();
	}


	public void testReadConfig() {
		ExternalFileModelFactory externalModelStochModelFactory = new ExternalFileModelFactory();
		externalModelStochModelFactory.initialize(testRunDataDir, new String[]{"FewsWorkflowModelFactory.xml"});
		ExternalFileModelInstance modelInstance = externalModelStochModelFactory.getInstance(new String[0], IStochModelFactory.OutputLevel.Suppress);

		IVector parameters = modelInstance.getParameters();
		assertEquals(0, parameters.getSize());
	}

}
