package org.openda.fewsworkflow;

import junit.framework.TestCase;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class FewsWorkflowModelFactoryTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() {
		testData = new OpenDaTestSupport(FewsWorkflowModelFactoryTest.class, "model_fews_workflow");
		testRunDataDir = testData.getTestRunDataDir();
	}


	public void testReadConfig() {
		FewsWorkflowModelFactory externalModelStochModelFactory = new FewsWorkflowModelFactory();
		externalModelStochModelFactory.initialize(testRunDataDir, new String[]{"FewsWorkflowModelFactory.xml"});
		FewsWorkflowModelInstance modelInstance = externalModelStochModelFactory.getInstance(new String[0], IStochModelFactory.OutputLevel.Suppress);

		IVector parameters = modelInstance.getParameters();
		assertEquals(0, parameters.getSize());
	}

}
