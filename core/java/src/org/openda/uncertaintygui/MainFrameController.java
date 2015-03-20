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

import org.openda.blackbox.config.BBUtils;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.File;

/**
 * Uppermost Controller class for the GUI, which also
 * contains main method.
 */
public class MainFrameController {

	//frame for this program.
	private MainFrame parentFrame;
	//controller for the main panel inside the parent frame.
	private MainPanelController mainPanelController;

    //application type and path to initialize mainPanelController.
    private APPLICATION_TYPE applicationType;

    //enumeration for different application types.
    public enum APPLICATION_TYPE {
        UNCERTAINTY_ANALYSIS, CALIBRATION, DATA_ASSIMILATION, UNCERTAINTY_GUI_ONLY
    }



    /**
     * Construct a frame controller without loading any files.
     * @param applicationType Application Type DA/UA/Cal
     */
	public MainFrameController(APPLICATION_TYPE applicationType) {
        if (applicationType != MainFrameController.APPLICATION_TYPE.UNCERTAINTY_GUI_ONLY) {
		    //set default application path.
		    File currentWorkingDirectory = BBUtils.getCurrentDir();
		    String applicationPath = currentWorkingDirectory.getAbsolutePath();
            this.applicationType = applicationType;

            //initialize GUI.
		    this.parentFrame = new MainFrame();
		    this.mainPanelController = new MainPanelController(
				    this, this.parentFrame.getMainPanel(), applicationPath);

		    //initialize event handlers.
		    this.initEventHandlers();
        } else {
            // Masquerade as the uncertainty analysis GUI - but we do not want the windows that go with it
            this.applicationType = MainFrameController.APPLICATION_TYPE.UNCERTAINTY_ANALYSIS;
        }
    }

	/**
	 * Initialize event handlers
	 */
	private void initEventHandlers() {

		this.parentFrame.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				exitApplication();
			}
		});

		this.parentFrame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

		//TODO: Add help functionality. AK
//		ActionListener helpActionListener = new ActionListener() {
//			public void actionPerformed(ActionEvent actionEvent) {
//				//helpActionPerformed(actionEvent);
//			}
//		};
//
//		this.parentFrame.getRootPane().registerKeyboardAction(helpActionListener,
//				KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0),
//				JComponent.WHEN_IN_FOCUSED_WINDOW);
//
//		this.parentFrame.addKeyListener(new java.awt.event.KeyAdapter() {
//			public void keyPressed(KeyEvent e) {
//				thisKeyPressed(e);
//			}
//		});

		this.parentFrame.getNewProjectMenuItem().addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
    			mainPanelController.newConfigurationActionPerformed();
			}
		});

		this.parentFrame.getNewProjectButton().addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
    			mainPanelController.newConfigurationActionPerformed();
			}
		});

		this.parentFrame.getCloseProjectMenuItem().addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				mainPanelController.closeConfigurationActionPerformed();
			}
		});

		this.parentFrame.getSaveUncertaintySpecificationAsMenuItem().addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				mainPanelController.saveUncertaintySpecificationAsActionPerformed();
			}
		});

		this.parentFrame.getExitMenuItem().addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				exitApplication();
			}
		});

        this.parentFrame.getShowHelpMenuItem().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                mainPanelController.showHelpActionPerformed();
            }
        });

        this.parentFrame.getShowAboutBoxMenuItem().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                mainPanelController.showAboutBoxActionPerformed();
            }
        });
	}

	/**
	 * Exit the Application.
	 */
	private void exitApplication() {
		this.parentFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		this.parentFrame.setEnabled(false);
		System.exit(0);
	}

	public MainFrame getParentFrame() {
		return this.parentFrame;
	}

    public APPLICATION_TYPE getApplicationType() {
        return this.applicationType;
    }

	public void setApplicationType(APPLICATION_TYPE applicationType) {
		this.applicationType = applicationType;
	}
}
