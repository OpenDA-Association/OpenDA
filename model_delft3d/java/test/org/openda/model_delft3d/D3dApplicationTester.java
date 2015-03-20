package org.openda.model_delft3d;

import org.openda.application.ApplicationRunner;
import org.openda.application.OpenDaApplication;

/**
 * General applicaton test class.
 * Added to be able to run a test from IntelliJ IDEA, without having to
 * add d3d_openda as a depenency to public/application.
 */
public class D3dApplicationTester {

	public static void main(String[] arguments) {
		ApplicationRunner.setRunningInTest(true);
		OpenDaApplication.main(arguments);
	}

}
