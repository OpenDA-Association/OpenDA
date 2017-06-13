/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/
package org.openda.utils;
//import org.openda.interfaces.*;
import junit.framework.TestCase;

import java.io.File;
import java.io.IOException;

public class ConfigTreeTest extends TestCase{

	private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(ConfigTreeTest.class,"core");
        testRunDataDir = testData.getTestRunDataDir();
    }

	
	public void testConfig_1() {
		System.out.println("==============================================================================");
		System.out.println("Config basics : constructor, toString, dimensions, ...");
		System.out.println("==============================================================================");
		ConfigTree conf = new ConfigTree("<config><maxIter tag=\"5\">8</maxIter><eps>0.001</eps></config>");
		String xmlString = conf.toString();
		System.out.println("config="+xmlString);
		
		String maxIterString = conf.getContentString("/maxIter");
		System.out.println("maxIter (String) ="+maxIterString);
		assertEquals("MaxIterString",maxIterString,"8");
		int maxIter = conf.getAsInt("/maxIter",100);
		System.out.println("maxIter (int) ="+maxIter);
		assertEquals("maxIter",maxIter,8);
		int minIter = conf.getAsInt("/minIter",1);
		System.out.println("minIter (int, NOT SET!) ="+minIter);
		assertEquals("minIter",minIter,1);
		
		String tagString = conf.getContentString("/maxIter@tag");
		System.out.println("maxIter@tag (String) ="+tagString);
		assertEquals("maxIter@tag",tagString,"5");
		int tag = conf.getAsInt("/maxIter@tag",1);
		System.out.println("minIter@tag (int) ="+tag);
		assertEquals("tag",tag,5);

		double eps = conf.getAsDouble("/eps",0.0);
		System.out.println("eps (double) ="+eps);
		assertEquals("eps",eps,0.001);	
	}

	public void testConfigFile_1() {
		System.out.println("==============================================================================");
		System.out.println("Config from file. ");
		System.out.println("==============================================================================");
		ConfigTree conf = new ConfigTree("<config><maxIter tag=\"5\">8</maxIter><eps>0.001</eps></config>");
		String xmlString = conf.toString();
		System.out.println("config="+xmlString);
		conf.toFile(testRunDataDir, "testConfig.xml");

		ConfigTree conf2 = new ConfigTree(testRunDataDir, "testConfig.xml");
		
		String maxIterString = conf2.getContentString("/maxIter");
		System.out.println("maxIter (String) ="+maxIterString);
		assertEquals("MaxIterString",maxIterString,"8");
	}
	

	
	public void testCalibrationConfig() {
		System.out.println("==============================================================================");
		System.out.println("Calibration config");
		System.out.println("==============================================================================");
		ConfigTree dudConf = new ConfigTree("<DudConfig><costFunction weakParameterConstraint=\"off\" class=\"org.openda.algorithms.SimulationKwadraticCostFunction\" /><outerLoop maxIterations=\"10\" absTolerance=\"0.01\" relTolerance=\"0.01\" /><lineSearch maxIterations=\"5\" maxRelStepSize=\"10.0\" ><backtracking shorteningFactor=\"0.5\" startIterationNegativeLook=\"3\" /></lineSearch><matlabOutputFile>dud.m</matlabOutputFile></DudConfig>");
		ConfigTree simplexConf = new ConfigTree("<SimplexConfig><costFunction weakParameterConstraint=\"off\" class=\"org.openda.algorithms.SimulationKwadraticCostFunction\" /><outerLoop maxIterations=\"10\" absTolerance=\"0.01\" relTolerance=\"0.01\" /><matlabOutputFile>simplex.m</matlabOutputFile></SimplexConfig>");
		ConfigTree powellConf = new ConfigTree("<PowellConfig><costFunction weakParameterConstraint=\"off\" class=\"org.openda.algorithms.SimulationKwadraticCostFunction\" /><outerLoop maxIterations=\"10\" absTolerance=\"0.01\" relTolerance=\"0.01\" /><lineSearch type=\"brent\" maxIterations=\"20\" relTolerance=\"0.01\" maxRelStepSize=\"100.0\" ><brent startBracketValue=\"1.0\" /></lineSearch><matlabOutputFile>powell.m</matlabOutputFile></PowellConfig>");

		// String xmlString = dudConf.toString();
		//test dudConf
        /*		
        <DudConfig>
           <costFunction weakParameterConstraint="off" class="org.openda.algorithms.SimulationKwadraticCostFunction" />
 		   <outerLoop maxIterations=10 absTolerance=0.01 relTolerance=0.01 />
		   <lineSearch maxIterations=5 maxRelStepSize=10.0 >
		      <backtracking shorteningFactor=0.5 startIterationNegativeLook=3 />
		   </lineSearch>
		   <matlabOutputFile>dud.m</matlabOutputFile>
		</DudConfig>
        */
		int maxIter = dudConf.getAsInt("outerLoop@maxIterations",1);
		System.out.println("maxIterations (int) ="+maxIter);
		assertEquals("maxIter",maxIter,10);
		boolean weakConstraint = dudConf.getAsBoolean("costFunction@weakParameterConstraint",true);
		System.out.println("weakConstraint (boolean) ="+weakConstraint);
		assertEquals("weakConstraint",weakConstraint,false);		

		/*
		<SimplexConfig>
           <costFunction weakParameterConstraint="off" class="org.openda.algorithms.SimulationKwadraticCostFunction" />
 		   <outerLoop maxIterations="10" absTolerance="0.01" relTolerance="0.01" />
		   <matlabOutputFile>simplex.m</matlabOutputFile>
		</SimplexConfig>
		*/
		double relTol = simplexConf.getAsDouble("outerLoop@relTolerance",999.99);
		System.out.println("outerLoop@relTolerance (double) ="+relTol);
		assertEquals("relTol",relTol,0.01);

		/*
		<PowellConfig>
           <costFunction weakParameterConstraint="off" class="org.openda.algorithms.SimulationKwadraticCostFunction" />
 		   <outerLoop maxIterations="10" absTolerance="0.01" relTolerance="0.01" />
		   <lineSearch maxIterations="20" relTolerance="0.01" maxRelStepSize="100.0" >
		      <brent startBracketValue="1.0" />
		   </lineSearch>
		   <matlabOutputFile>powell.m</matlabOutputFile>
		</PowellConfig>
		*/
		double startBracketValue = powellConf.getAsDouble("lineSearch/brent@startBracketValue",999.99);
		System.out.println("startBracketValue (double) ="+startBracketValue);
		assertEquals("startBracketValue",startBracketValue,1.0);
	}
	
	
	public void testTreeAnalysis() throws IOException {
		System.out.println("==============================================================================");
		System.out.println("Aplication config");
		System.out.println("==============================================================================");
		ConfigTree appConf = new ConfigTree(testRunDataDir,"applicationConfig.xml" );
		String xmlString = appConf.toString();
		System.out.println("config="+xmlString);
		String[][] parts = appConf.getParts();
		System.out.println(ConfigTree.parts2String(parts));
		
		System.out.println("parts[3][0] ="+parts[3][0]);
		System.out.println("Should be parts[3][0] = dud_results.m");
		assertEquals("parts[3][0]",parts[3][0],"dud_results.m");	
	}

	public void testTreeAnalysis2() throws IOException {
		System.out.println("==============================================================================");
		System.out.println("Application config: analysis of the tree");
		System.out.println("==============================================================================");
		ConfigTree appConf = new ConfigTree(testRunDataDir,"noosObservations.xml" );
		String xmlString = appConf.toString();
		System.out.println("config="+xmlString);
		ConfigTree parts[] = appConf.getSubTrees("timeSeries");
		int n = parts.length;
		for(int i=0;i<n;i++){
			System.out.println("subTree["+i+"]="+parts[i].toString());
		}
		assertEquals("number f subtrees",2, n);
		System.out.println("parts[0] ="+parts[0].toString());
		System.out.println("Should be parts[0] =<?xml version=\"1.0\"?>...");
		int l = "<?xml version=\"1.0\"?>".length();
		String temp = parts[0].toString().substring(0,l);
		assertEquals("parts[0]","<?xml version=\"1.0\"?>",temp);	
	}

	public void testTreeAnalysis3() throws IOException {
		System.out.println("==============================================================================");
		System.out.println("Aplication config with default namespace and xsd");
		System.out.println("==============================================================================");
		ConfigTree appConf = new ConfigTree(testRunDataDir,"applicationConfig.oda" ); 
		String xmlString = appConf.toString();
		System.out.println("config="+xmlString);
		String[][] parts = appConf.getParts();
		System.out.println(ConfigTree.parts2String(parts));
		
		System.out.println("parts[3][0] ="+parts[3][0]);
		System.out.println("Should be parts[3][0] = dud_results.m");
		assertEquals("parts[3][0]",parts[3][0],"dud_results.m");	
	}
	
	public void testTreeAnalysis4() throws IOException {
		System.out.println("==============================================================================");
		System.out.println("Aplication config with schema error");
		System.out.println("==============================================================================");
		ConfigTree appConf = new ConfigTree(testRunDataDir,"applicationConfig_with_error.oda",true ); 
		String xmlString = appConf.toString();
		System.out.println("config="+xmlString);
		String[][] parts = appConf.getParts();
		System.out.println(ConfigTree.parts2String(parts));
		
		System.out.println("parts[3][0] ="+parts[3][0]);
		System.out.println("Should be parts[3][0] = dud_results.m");
		assertEquals("parts[3][0]",parts[3][0],"dud_results.m");	
		
		String message = Results.getLastMessage();
		System.out.println("xml parsing error="+message);
		// NOTE: this does not work without an internet connection.
		//TODO: Enable when ODA-496 is fixed properly.
//		if(message.indexOf("Failed to read schema document")<0){
//			assertTrue(message.indexOf("Invalid content was found starting with element 'stochModelBakery")>0);
//		}
	}

	
	public void testFileWithSchema() {
		System.out.println("==============================================================================");
		System.out.println("Parse file with schema : First ignore schema, but read correctly");
		System.out.println("==============================================================================");
        ConfigTree appConf = new ConfigTree(testRunDataDir,"oscillatorEnkfOpenDaConfig.xml",true );
		// first serialize to string
		String xmlString = appConf.toString();
		System.out.println("appConf.toString()="+xmlString.substring(0,20));
		System.out.println("appConf.toString() ="+xmlString.substring(0,20));
		System.out.println("Should be appConf.toString() = <?xml version=\"1.0\"?");
		assertEquals("appConf.toString()",xmlString.substring(0,20),"<?xml version=\"1.0\"?");	
		
		// get some elements
		String stochobsClassString = appConf.getContentString("/stochObserver@className");
		System.out.println("/stochObserver@className (String) ="+stochobsClassString);
		assertEquals("naxIter@tag",stochobsClassString,"org.openda.utils.CsvStochObserver");

		// find includes (openda style)
		String[][] parts = appConf.getParts();
		System.out.println(ConfigTree.parts2String(parts));
		System.out.println("parts[3][0] ="+parts[3][0]);
		System.out.println("Should be parts[3][0] = enkf_results.m");
		assertEquals("parts[3][0]",parts[3][0],"enkf_results.m");	
	}

	public void testSwanTreeAnalysis() {
		System.out.println("==============================================================================");
		System.out.println("more complex swan stochModel config file");
		System.out.println("==============================================================================");
        ConfigTree appConf = new ConfigTree(testRunDataDir,"swanStochModelConfig.xml" );
		String xmlString = appConf.toString();
		System.out.println("config="+xmlString);
		String[][] parts = appConf.getParts();
		System.out.println(ConfigTree.parts2String(parts));
		
		System.out.println("parts[3][0] ="+parts[2][0]);
		System.out.println("Should be parts[2][0] = parameterUncertainties.xml");
		assertEquals("parts[3][0]",parts[2][0],"parameterUncertainties.xml");
		
		ConfigTree subConfig = appConf.getFirstChild();
		String subAsString = subConfig.toString();
		System.out.println("appConf.getFirstChild()="+subAsString);
		assertTrue(subAsString.contains("swan"));
	}

	public void testAlgorithmFileWithSchema() {
		System.out.println("==============================================================================");
		System.out.println("Parse algorithm file with schema : First ignore schema, but read correctly");
		System.out.println("==============================================================================");
        ConfigTree appConf = new ConfigTree(testRunDataDir,"EnkfAlgorithm.xml" );
		// first serialize to string
		String xmlString = appConf.toString();
		System.out.println("appConf.toString()="+xmlString.substring(0,20));
		System.out.println("appConf.toString() ="+xmlString.substring(0,20));
		System.out.println("Should be appConf.toString() = <?xml version=\"1.0\"?");
		assertEquals("appConf.toString()",xmlString.substring(0,20),"<?xml version=\"1.0\"?");	
		
		// get some elements
		//		<ensembleSize>30</ensembleSize>
		String ensembleSizeString= appConf.getContentString("/ensembleSize");
		System.out.println("/ensembleSize (String) ="+ensembleSizeString);
		assertEquals("/ensembleSize","30",ensembleSizeString);

		// <analysisTimes type="fromObservationTimes" ></analysisTimes> 
		String analysisType= appConf.getContentString("/analysisTimes@type");
		System.out.println("/analysisTimes@type (String) ="+analysisType);
		assertEquals("/analysisTimes@type","fromObservationTimes",analysisType);
		
		//<mainModel stochParameter="false" stochForcing="false" stochInit="false" />
		String mainStoch= appConf.getContentString("/mainModel@stochParameter");
		System.out.println("/mainModel@stochParameter (String) ="+mainStoch);
		assertEquals("/mainModel@stochParameter","false",mainStoch);
		String mainForcing= appConf.getContentString("/mainModel@stochForcing");
		System.out.println("/mainModel@stochForcing (String) ="+mainForcing);
		assertEquals("/mainModel@stochForcing","false",mainForcing);
		String mainInit= appConf.getContentString("/mainModel@stochInit");
		System.out.println("/mainModel@stochInit (String) ="+mainInit);
		assertEquals("/mainModel@stochInit","false",mainInit);
		//<ensembleModel stochParameter="false" stochForcing="true" stochInit="true" />
		String ensStoch= appConf.getContentString("/ensembleModel@stochParameter");
		System.out.println("/ensembleModel@stochParameter (String) ="+ensStoch);
		assertEquals("/ensembleModel@stochParameter","false",ensStoch);
		String ensForcing= appConf.getContentString("/ensembleModel@stochForcing");
		System.out.println("/ensembleModel@stochForcing (String) ="+ensForcing);
		assertEquals("/ensembleModel@stochForcing","true",ensForcing);
		String ensInit= appConf.getContentString("/ensembleModel@stochInit");
		System.out.println("/ensembleModel@stochInit (String) ="+ensInit);
		assertEquals("/ensembleModel@stochInit","true",ensInit);

	}

	
}
