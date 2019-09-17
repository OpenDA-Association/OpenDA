package org.openda.blackbox.wrapper;

import junit.framework.TestCase;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;
import org.openda.utils.Vector;
import org.openda.utils.io.FileSupport;

import java.io.File;

public class BBStochModelNoDeltaTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() {
		testData = new OpenDaTestSupport(BBStochModelParametersTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testDeltaAndNoDelta() {

		IStochModelFactory stochModelFactoryDelta = new BBStochModelFactory();
		File workingDir = new File(testRunDataDir, "parameterNotAsDelta");
		stochModelFactoryDelta.initialize(workingDir, new String[]{"StochModelConfig_1.xml"});

		IStochModelInstance stochModelInstanceDelta = stochModelFactoryDelta.getInstance(IStochModelFactory.OutputLevel.ModelDefault);

		IVector parameters = stochModelInstanceDelta.getParameters();
		double[] initialValuesParameterDelta = parameters.getValues();

		assertEquals(0.0, initialValuesParameterDelta[0]);
		assertEquals(0.0, initialValuesParameterDelta[1]);

		IVector axpyVector = new Vector(new double[]{5,5});
		stochModelInstanceDelta.axpyOnParameters(1, axpyVector);
		
		double[] axpyValuesParameterDelta = parameters.getValues();

		assertEquals(5.0, axpyValuesParameterDelta[0]);
		assertEquals(5.0, axpyValuesParameterDelta[1]);

		stochModelInstanceDelta.compute(new Time(0.0));
		stochModelInstanceDelta.finish();
		String[] contentDelta = FileSupport.readContentOfFile(new File(workingDir, "work0/DummyParametersIdAndValues.txt"));

		BBStochModelFactory stochModelFactoryNoDelta = new BBStochModelFactory();
		stochModelFactoryNoDelta.initialize(workingDir, new String[]{"StochModelConfig_2.xml"});

		IStochModelInstance stochModelInstanceNoDelta = stochModelFactoryNoDelta.getInstance(IStochModelFactory.OutputLevel.ModelDefault);

		IVector parametersNoDelta = stochModelInstanceNoDelta.getParameters();

		double[] initialValuesParameterNoDelta = parametersNoDelta.getValues();

		assertEquals(101.0, initialValuesParameterNoDelta[0]);
		assertEquals(502.0, initialValuesParameterNoDelta[1]);

		stochModelInstanceNoDelta.axpyOnParameters(1, axpyVector);
		IVector parametersNoDeltaAxpy = stochModelInstanceNoDelta.getParameters();
		double[] axpyValuesParameterNoDelta = parametersNoDeltaAxpy.getValues();

		assertEquals(106.0, axpyValuesParameterNoDelta[0]);
		assertEquals(507.0, axpyValuesParameterNoDelta[1]);

		stochModelInstanceNoDelta.compute(new Time(0.0));

		stochModelInstanceNoDelta.finish();
		String[] contentNoDelta = FileSupport.readContentOfFile(new File(workingDir, "work0/DummyParametersIdAndValues.txt"));

		assertEquals(contentDelta.length, contentNoDelta.length);
		for (int i = 0; i < contentDelta.length; i++) {
			assertEquals(contentDelta[i], contentNoDelta[i]);
		}
	}


	// extend for transformation is ln
}
