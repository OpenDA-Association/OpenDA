/* OpenDA v2.4.1 
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

package org.openda.model_swan.swivt;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.io.File;

/**
 * User interface for Swivt Case Extraction
 */
public class CaseExtractorGUI extends JFrame implements ActionListener, FocusListener {

	private static final String VERSION_STRING = "SWIVT 1.4 Case Extractor (OpenDA 2.1)";

    private JButton extractButton = new JButton("Extract");
    private JButton exitButton = new JButton("Exit");

    private File swivtCaseDir = null;
    private File calParentDir = null;
    private File windowsSwanExePath = null;

    private JButton browseSwivtCaseDirButton = new JButton("Browse");
    private JButton browseCalibrationParentDirButton = new JButton("Browse");
    private JButton browseWindowsExeButton = new JButton("Browse");

    private JTextField swivtCaseDirTextField = new JTextField("");
    private JTextField calParentDirTextField = new JTextField("");
    private JTextField windowsSwanExeTextField = new JTextField("");

    private JTextField linuxSwanCalBinDirTextField = new JTextField("");
    private JTextField linuxSequentialSwanExeTextField = new JTextField("");
    private JTextField linuxParallelSwanExeTextField = new JTextField("");
    private JTextField messageTextField = new JTextField("");

    private CaseExtractorUserSettings userSettings = new CaseExtractorUserSettings();

    public CaseExtractorGUI() throws HeadlessException {
    	setTitle(VERSION_STRING);

        // main layout
        setSize(userSettings.getGuiWidth() , userSettings.getGuiHeight());
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLayout(new BorderLayout());

        // default or user settings
        String defaultLinuxSwanCalBinDirText = "";
        String userName = System.getenv("USER");
        if (userName != null) {
            String linuxUserDir = "/u/" + userName;
            defaultLinuxSwanCalBinDirText = linuxUserDir + "/swan_cal_bin";
        }

        JLabel swivtCaseLabel = new JLabel("Swivt case directory");
        JLabel parentDirLabel = new JLabel("Parent of calibration case directory");
        JLabel exeForWindowsLabel = new JLabel("Swan executable for windows");
        swivtCaseDirTextField.setEditable(false);
        calParentDirTextField.setEditable(false);
        windowsSwanExeTextField.setEditable(true);
        windowsSwanExeTextField.addActionListener(this);
        windowsSwanExeTextField.addFocusListener(this);

        JLabel messageTextLabel = new JLabel("Messages:");
        messageTextField.setEditable(false);

        JLabel linuxSwanCalBinDirTextLabel = new JLabel("Swan Calibration bin dir. on Linux");
        JLabel linuxSequentialSwanExeTextLabel = new JLabel("Swan executable for Linux");
        JLabel linuxParallelSwanExeTextLabel = new JLabel("Swan parallel executable for Linux");
        String defaultLinuxSequentialSwanExeText = "";
        String defaultLinuxParallelSwanExeText = "";

        String userValue = userSettings.getSwivtCaseDirPath();
        if ( userValue != null && userValue.length() != 0) {
            swivtCaseDir = new File(userValue);
            if (!swivtCaseDir.exists()) {
                swivtCaseDir = null;
            } else {
                swivtCaseDirTextField.setText(userValue);
            }
        }

        userValue = userSettings.getCalibrationParentDir();
        if ( userValue != null && userValue.length() != 0) {
            calParentDir = new File(userValue);
            if (!calParentDir.exists()) {
                calParentDir = null;
            } else {
                calParentDirTextField.setText(userValue);
            }
        }

        userValue = userSettings.getWindowsSwanExePath();
        if ( userValue != null && userValue.length() != 0) {
            windowsSwanExePath = new File(userValue);
            if (!windowsSwanExePath.exists()) {
                windowsSwanExePath = null;
            } else {
                windowsSwanExeTextField.setText(userValue);
            }
        }

        userValue = userSettings.getLinuxSwanCalBinDir();
        linuxSwanCalBinDirTextField.setText(
                ( userValue != null && userValue.length() != 0) ? userValue :
                defaultLinuxSwanCalBinDirText);
        userValue = userSettings.getLinuxSequentialSwanExe();
        linuxSequentialSwanExeTextField.setText(
                ( userValue != null && userValue.length() != 0) ? userValue :
                defaultLinuxSequentialSwanExeText);
        userValue = userSettings.getLinuxParallelSwanExe();
        linuxParallelSwanExeTextField.setText(
                ( userValue != null && userValue.length() != 0) ? userValue :
                defaultLinuxParallelSwanExeText);

        // Create panel and layout for text fields with browse button

        JComponent panel = new JPanel();
        GroupLayout layout = new GroupLayout(panel);
        panel.setLayout(layout);
        // Turn on automatically adding gaps between components
        layout.setAutoCreateGaps(true);
        // Turn on automatically creating gaps between components that touch
        // the edge of the container and the container.
        layout.setAutoCreateContainerGaps(true);

        GroupLayout.SequentialGroup hGroup = layout.createSequentialGroup();

        hGroup.addGroup(layout.createParallelGroup().
                addComponent(swivtCaseLabel).
                addComponent(parentDirLabel).
                addComponent(exeForWindowsLabel));
        hGroup.addGroup(layout.createParallelGroup().
                addComponent(swivtCaseDirTextField).
                addComponent(calParentDirTextField).
                addComponent(windowsSwanExeTextField));
        hGroup.addGroup(layout.createParallelGroup().
                addComponent(browseSwivtCaseDirButton).
                addComponent(browseCalibrationParentDirButton).
                addComponent(browseWindowsExeButton));
        layout.setHorizontalGroup(hGroup);

        GroupLayout.SequentialGroup vGroup = layout.createSequentialGroup();
        vGroup.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE).
                addComponent(swivtCaseLabel).
                addComponent(swivtCaseDirTextField).
                addComponent(browseSwivtCaseDirButton));
        vGroup.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE).
                addComponent(parentDirLabel).
                addComponent(calParentDirTextField).
                addComponent(browseCalibrationParentDirButton));
        vGroup.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE).
                addComponent(exeForWindowsLabel).
                addComponent(windowsSwanExeTextField).
                addComponent(browseWindowsExeButton));
        layout.setVerticalGroup(vGroup);

        this.add(panel, BorderLayout.NORTH);

        // Create panel and layout for text fields without browse button

        panel = new JPanel();
        layout = new GroupLayout(panel);
        panel.setLayout(layout);
        layout.setAutoCreateGaps(true);
        layout.setAutoCreateContainerGaps(true);

        hGroup = layout.createSequentialGroup();

        hGroup.addGroup(layout.createParallelGroup().
                addComponent(linuxSwanCalBinDirTextLabel).
                addComponent(linuxSequentialSwanExeTextLabel).
                addComponent(linuxParallelSwanExeTextLabel).
                addComponent(messageTextLabel));
        hGroup.addGroup(layout.createParallelGroup().
                addComponent(linuxSwanCalBinDirTextField).
                addComponent(linuxSequentialSwanExeTextField).
                addComponent(linuxParallelSwanExeTextField).
                addComponent(messageTextField));
        layout.setHorizontalGroup(hGroup);

        vGroup = layout.createSequentialGroup();
        vGroup.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE).
                addComponent(linuxSwanCalBinDirTextLabel).
                addComponent(linuxSwanCalBinDirTextField));
        vGroup.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE).
                addComponent(linuxSequentialSwanExeTextLabel).
                addComponent(linuxSequentialSwanExeTextField));
        vGroup.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE).
                addComponent(linuxParallelSwanExeTextLabel).
                addComponent(linuxParallelSwanExeTextField));
        vGroup.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE).
                addComponent(messageTextLabel).
                addComponent(messageTextField));
        layout.setVerticalGroup(vGroup);

        this.add(panel, BorderLayout.CENTER);

        // Extract buttons
        panel = new JPanel();
        BorderLayout extractAndExitlayout = new BorderLayout();
        panel.setLayout(extractAndExitlayout);
        panel.add(extractButton, BorderLayout.WEST);
        panel.add(exitButton, BorderLayout.EAST);

        this.add(panel, BorderLayout.SOUTH);

        // set event listner
        extractButton.addActionListener(this);
        browseSwivtCaseDirButton.addActionListener(this);
        browseCalibrationParentDirButton.addActionListener(this);
        browseWindowsExeButton.addActionListener(this);
        exitButton.addActionListener(this);

        // show gui
        setVisible(true);
    }

    public void actionPerformed(ActionEvent e) {
        messageTextField.setText("");
        Object source = e.getSource();
        if (source == extractButton) {
            updateUserSettings();
            performExtraction();
        }
        else if (source == browseSwivtCaseDirButton) {
            swivtCaseDir = performBrowsAction("Swivt case directory", swivtCaseDir, null, true);
            if (swivtCaseDir != null) {
                String absolutePath = swivtCaseDir.getAbsolutePath();
                userSettings.setSwivtCaseDirPath(absolutePath);
                swivtCaseDirTextField.setText(absolutePath);
            }
        }
        else if (source == browseCalibrationParentDirButton) {
            calParentDir = performBrowsAction("Parent directory for calibration case", calParentDir, null, true);
            if (calParentDir != null) {
                String absolutePath = calParentDir.getAbsolutePath();
                userSettings.setCalibrationParentDir(absolutePath);
                calParentDirTextField.setText(absolutePath);
            }
        }
        else if (source == browseWindowsExeButton) {
            windowsSwanExePath = performBrowsAction("Parent directory for calibration case", windowsSwanExePath, "exe", false);
            if (windowsSwanExePath != null) {
                String absolutePath = windowsSwanExePath.getAbsolutePath();
                userSettings.setWindowsSwanExePath(absolutePath);
                windowsSwanExeTextField.setText(absolutePath);
            }
        }
        else if (source == windowsSwanExeTextField) {
        	//if enter was pressed inside windowsSwanExeTextField.
        	updateSwanExecutableFromTextField();
        }
        else if (source == exitButton) {
            updateUserSettings();
            System.exit(0);
        }
        else {
            System.out.println("CLICK" + source.toString());
        }
    }

	public void focusGained(FocusEvent e) {
		//do nothing.
	}

	public void focusLost(FocusEvent e) {
        Object source = e.getSource();
        if (source == windowsSwanExeTextField) {
        	//if windowsSwanExeTextField lost focus.
        	updateSwanExecutableFromTextField();
        }
	}

    /**
	 * If windowsSwanExeTextField contains a valid file path, then use that.
	 * If windowsSwanExeTextField is empty, then reset windowsSwanExePath variable and user setting.
     */
    private void updateSwanExecutableFromTextField() {
    	//(re)set windowsSwanExePath variable.
    	windowsSwanExePath = null;
    	String text = windowsSwanExeTextField.getText();
    	if (text != null && !text.isEmpty()) {
    		windowsSwanExePath = new File(text);
            if (!windowsSwanExePath.exists()) {
                windowsSwanExePath = null;
            }
    	}

    	//(re)set windowsSwanExePath user setting.
    	String absolutePath = "";
    	if (windowsSwanExePath != null) {
    		absolutePath = windowsSwanExePath.getAbsolutePath();
    	}
        userSettings.setWindowsSwanExePath(absolutePath);
	}

    private void performExtraction() {
        updateUserSettings();
        File targetCalCaseDir = new File(calParentDir, swivtCaseDir.getName());
        if (targetCalCaseDir.exists()) {
            String message = "Target calibration case\n" + targetCalCaseDir.getAbsolutePath() +
                    "\n already exists.\n\nDo you want to overwrite it?" ;
            int selection = JOptionPane.showConfirmDialog(this, message,
                    "Confirm overwrite", JOptionPane.OK_CANCEL_OPTION);
            if ((selection == JOptionPane.CANCEL_OPTION)||(selection == JOptionPane.CLOSED_OPTION)) {
                return;
            }
        }
        messageTextField.setText("Extracting case " + swivtCaseDir.getName());
        this.update(getGraphics());
        Cursor currentCursor = this.getCursor();
        Cursor waitCursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
        this.setCursor(waitCursor);
        try {
            CaseExtractor caseExtractor = new CaseExtractor(swivtCaseDir);
            caseExtractor.extractCase(calParentDir, userSettings.getWindowsSwanExePath(), userSettings.getLinuxSwanCalBinDir(),
                    userSettings.getLinuxSequentialSwanExe(), userSettings.getLinuxParallelSwanExe());
            messageTextField.setText("Case " + swivtCaseDir.getName() + " extracted");
            this.setCursor(currentCursor);
        } catch (Exception e) {
            messageTextField.setText("Case " + swivtCaseDir.getName() + " extracted");
            this.setCursor(currentCursor);
            JOptionPane.showMessageDialog(this, e.getMessage());
        }
    }

    private File performBrowsAction(String browseFor, File currentFileSelection, String extension, boolean directoriesOnly) {
        FileBrowser fileBrowser = new FileBrowser("Select " + browseFor, currentFileSelection, extension, directoriesOnly);
        return fileBrowser.getSelectedFile();
    }

    private void updateUserSettings() {
        userSettings.setGuiWidth(this.getWidth());
        userSettings.setGuiHeight(this.getHeight());
        userSettings.setLinuxSwanCalBinDir(linuxSwanCalBinDirTextField.getText());
        userSettings.setLinuxSequentialSwanExe(linuxSequentialSwanExeTextField.getText());
        userSettings.setLinuxParallelSwanExe(linuxParallelSwanExeTextField.getText());
    }

    public static void main(String[] arguments) {
        new CaseExtractorGUI();
    }
}
