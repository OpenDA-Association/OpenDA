/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
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

package org.openda.uncertaintygui;

import com.incors.plaf.alloy.AlloyLookAndFeel;
import org.exolab.castor.util.LocalConfiguration;

import javax.swing.*;
import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * User: Juzer Dhondia
 * Date: 21-nov-2007
 * Time: 13:20:55
 */
public class UILoader {

	//constant strings for all possible command line argument options.
	private static final String UNCERTAINTY_ANALYSIS = "uncertaintyanalysis";
	private static final String CALIBRATION = "calibration";
	private static final String DATA_ASSIMILATION = "dataassimilation";
	private static final String UNCERTAINTY_ANALYSIS_SHORT = "ua";
	private static final String CALIBRATION_SHORT = "cal";
	private static final String DATA_ASSIMILATION_SHORT = "da";
	private static MainFrameController.APPLICATION_TYPE applicationType;

	/**
	 * Main method which starts the program depending on which arguments are given.
	 *
	 * @param args name of the directory and the file where the model config data are stored
	 */
	public static void main(String[] args) {

		//set look and feel.
		//setLookAndFeel();

		// Set platform depend look and feel
//		try {
//			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
//		} catch (Exception e) {
//			e.printStackTrace();
//		}

		//process command line arguments.
		if (args != null && args.length > 0) {

			//process first argument (application type).
			processApplicationTypeArgument(args[0]);

			if (args.length == 1) {
				//start program without loading any files.
				new MainFrameController(applicationType);
			}
		} else {//if no arguments.
			applicationType = MainFrameController.APPLICATION_TYPE.UNCERTAINTY_ANALYSIS;
			new MainFrameController(applicationType);
		}
		//start program without loading any files with standard UA application type.
		LocalConfiguration.getInstance().getProperties().setProperty("org.exolab.castor.indent", "true");
	}

	/**
	 * Set the look and feel for the whole application.
	 */
	private static void setLookAndFeel() {

		//Custom theme colors
		//Color contrast = new Color(188, 188, 206);
		Color contrast = new Color(190, 203, 222);
		Color standard = new Color(241, 240, 227);
		Color desktop = new Color(164, 164, 158);
		Color selection = new Color(96, 128, 172);
		Color rollover = new Color(255, 207, 49);
		Color highlight = new Color(249, 224, 137);

		//Create custom theme
		com.incors.plaf.alloy.AlloyTheme theme = com.incors.plaf.alloy.themes.custom.CustomThemeFactory.createTheme(contrast, standard, desktop, selection, rollover, highlight);
		AlloyLookAndFeel.setProperty("alloy.licenseCode", "4#James_Brown#1g2r7nj#5z9r8g");
		javax.swing.LookAndFeel alloy = new com.incors.plaf.alloy.AlloyLookAndFeel(theme);
		AlloyLookAndFeel.setProperty("alloy.isLookAndFeelFrameDecoration", "true");

		try {
			UIManager.setLookAndFeel(alloy);
		} catch (UnsupportedLookAndFeelException e) {
			//No action required
		}

		JFrame.setDefaultLookAndFeelDecorated(true);
		JDialog.setDefaultLookAndFeelDecorated(true);
	}


	/**
	 * Displays a dialog with information on how to start the program correctly.
	 */
	private static void showUsageDialog() {
		JOptionPane.showMessageDialog(null,
				"Only the following combinations of command line arguments are possible:\n" +
						"No arguments.\n" +
						"One argument: -appType\n" +
						"Two arguments: -appType <configFile>\n" +
						"Three arguments: -appType <uncertaintyMappingFile> <resultMappingFile>\n\n" +
						"Where -appType is one of the following:\n" +
						"-" + UNCERTAINTY_ANALYSIS_SHORT + ", -" + UNCERTAINTY_ANALYSIS + ",\n" +
						"-" + CALIBRATION_SHORT + ", -" + CALIBRATION + ",\n" +
						"-" + DATA_ASSIMILATION_SHORT + " or -" + DATA_ASSIMILATION + "\n\n" +
						"Where <configFile> is the full path of a model configuration xml file, e.g.:\n" +
						"C:\\smhi\\ihms\\dat\\uaExecutive\\uaConfigHBVMAAS.xml\n\n" +
						"Where <uncertaintyMappingFile> is the full path of an xml file with uncertain items.\n\n" +
						"Where <resultMappingFile> is the full path of an xml file with results.\n" +
						"\n",
				"DATools - Incorrect Command Line Arguments", JOptionPane.INFORMATION_MESSAGE);
	}

	/**
	 * Process application type argument.
	 *
	 * @param argument application type
	 */
	private static void processApplicationTypeArgument(String argument) {
		if (argument.equalsIgnoreCase("-" + UNCERTAINTY_ANALYSIS)
				|| argument.equalsIgnoreCase("-" + UNCERTAINTY_ANALYSIS_SHORT)) {
			applicationType = MainFrameController.APPLICATION_TYPE.UNCERTAINTY_ANALYSIS;
		} else if (argument.equalsIgnoreCase("-" + CALIBRATION)
				|| argument.equalsIgnoreCase("-" + CALIBRATION_SHORT)) {
			applicationType = MainFrameController.APPLICATION_TYPE.CALIBRATION;
		} else if (argument.equalsIgnoreCase("-" + DATA_ASSIMILATION)
				|| argument.equalsIgnoreCase("-" + DATA_ASSIMILATION_SHORT)) {
			applicationType = MainFrameController.APPLICATION_TYPE.DATA_ASSIMILATION;
		} else {//incorrect argument.
			showUsageDialog();
			System.exit(-1);
		}
	}
}
