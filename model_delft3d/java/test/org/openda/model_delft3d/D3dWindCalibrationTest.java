package org.openda.model_delft3d;

import junit.framework.TestCase;
import org.openda.application.ApplicationRunner;
import org.openda.application.OpenDaApplication;
import org.openda.blackbox.config.BBUtils;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for D3D flow black box wrapper wind field subselection classes
 */
public class D3dWindCalibrationTest extends TestCase {


	private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(D3dWindCalibrationTest.class,"public","model_delft3d");

        testRunDataDir = new File(testData.getTestRunDataDir(), "test_4");

    }

	public static void testDummy() {
		// No action. Test only exists to avoid warnings on empty test class when
		//            the test below is de-activated by renaming it to tst...()
	}

	public void tstDudD3d() throws IOException {
    	String configFileName=null;
    	if (!BBUtils.RUNNING_ON_WINDOWS) {
    		if(System.getProperty("sun.arch.data.model").equals("64")){
    			configFileName="kalib04-15_linux64_gnu.oda";
    		}else{
    			//no testing on linux 32-bit
    			return;
    		}
    	}else{
    		configFileName="kalib04-15.oda";
    	}

    	String inputFile  = ( new File(testRunDataDir,configFileName)).getAbsolutePath();
    	System.out.println("path ="+inputFile);
    	String args[] = new String[1];
    	args[0] = inputFile;
		ApplicationRunner.setRunningInTest(true);
		OpenDaApplication.main(args);

		//TODO: check the output files?

		 // check that the Dud-results are as expected
        File windFile_reload = new File(testRunDataDir,"results_dud.csv" );

        File ref_windFile = new File(testRunDataDir, "ref_results_dud.csv");
      //  assertTrue(testData.FilesAreIdentical(windFile_reload, ref_windFile));


		System.out.println("done?");
    }


}
