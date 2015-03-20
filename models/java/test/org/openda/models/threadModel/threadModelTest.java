package org.openda.models.threadModel;

import junit.framework.TestCase;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

/**
 * Created by IntelliJ IDEA.
 * User: nils
 * Date: 12/16/11
 * Time: 10:15 AM
 * To change this template use File | Settings | File Templates.
 */
public class threadModelTest extends TestCase {

	static int numInstances = 100;
	OpenDaTestSupport testData;

	protected void setUp() {
		testData = new OpenDaTestSupport(threadModelTest.class,"models");
	}





	public void testLorenzModel_1() {
		System.out.println("=========================================================");
		System.out.println("Model constructor");
		System.out.println("=========================================================");

		File testDataDir = testData.getTestRunDataDir();

		/* prepare input file */
		try {
			File outFile = new File(testDataDir,"ThreadStochModel.xml");
			FileWriter outWriter = new FileWriter(outFile);
			PrintWriter out = new PrintWriter(outWriter);
			// Write text to file
			out.println("<threadConfig>\n");
			out.println("   <maxThreads>8</maxThreads>\n");
			out.println("   <stochModelFactory className=\"org.openda.models.lorenz.LorenzStochModelFactory\">\n");
			out.println("      <workingDirectory>"+testDataDir+"</workingDirectory>\n");
			out.println("      <configFile>LorenzStochModel.xml</configFile>\n");
			out.println("   </stochModelFactory>\n");
			out.println("</threadConfig>\n");
			out.close();
		} catch (IOException e){
			e.printStackTrace();
		}

		// Set up the model factory
		IStochModelFactory fact1 = new ThreadStochModelFactory();
		fact1.initialize(testDataDir, new String[]{"ThreadStochModel.xml"});

		//create model instances
		IStochModelInstance[] model = new IStochModelInstance[numInstances];
		for (int iModel=0; iModel<numInstances; iModel++){
			model[iModel] = fact1.getInstance(IStochModelFactory.OutputLevel.Suppress);
		}

		ITime endTime= model[0].getTimeHorizon();
		for (int iModel=0; iModel<numInstances; iModel++){
			model[iModel].compute(endTime);
		}
		IVector[] state = new IVector[numInstances];
		for (int iModel=0; iModel<numInstances; iModel++){
			state[iModel] = model[iModel].getState();
		}
	}











}
