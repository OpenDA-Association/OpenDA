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
package org.openda.application.gui;

import org.openda.application.ApplicationRunner;
import org.openda.interfaces.IAlgorithm;
import org.openda.interfaces.IModelState;
import org.openda.utils.ResultSelectionConfig;
import org.openda.utils.Results;
import org.openda.utils.plotting.Figure;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

@SuppressWarnings("serial")
public class ControlGui extends JPanel implements ActionListener {
   private static final String DISABLE_LOGGING_TEXT = "Disable Logging";
   private static final String ENABLE_LOGGING_TEXT = "Enable Logging";

   // logging
   private static GUIResultWriter guiResultWriter    = null;
   PlotResultWriter               costPlotter;
   private JButton                disableLoggingButton = null;
   // current Run
   private ApplicationRunner      runThread          = null;
   // Buttons
   JButton                        openRestartButton  = null;
   JButton                        saveRestartButton  = null;
   JButton                        presentButton      = null;
   private static ControlGui      myScreen           = null;
   private OpenDaUserSettings     openDaUserSettings = null;
   private OutputGui              outputGui          = null;
   private SelectCases            selectCases        = null;

   public ControlGui(OpenDaUserSettings openDaUserSettings, OutputGui output, JPanel plot) {
      this.openDaUserSettings = openDaUserSettings;
      this.outputGui = output;
      setLayout(new BorderLayout());
      // Toolbar: with load and save restart
      JToolBar controlBar = new JToolBar();
      ImageIcon openIcon = new ImageIcon(this.getClass().getResource("Open24.gif"));
      this.openRestartButton = new JButton("Open restart file", openIcon);
      ImageIcon saveIcon = new ImageIcon(this.getClass().getResource("Save24.gif"));
      this.saveRestartButton = new JButton("Save restart file", saveIcon);
      ImageIcon presentIcon = new ImageIcon(this.getClass().getResource("Open24.gif"));
      this.presentButton = new JButton("Plot selected runs", presentIcon);
      controlBar.add(this.openRestartButton);
      controlBar.add(this.saveRestartButton);
      controlBar.add(this.presentButton);
      this.openRestartButton.addActionListener(this);
      this.saveRestartButton.addActionListener(this);
      this.presentButton.addActionListener(this);
      add(controlBar, BorderLayout.NORTH);

      // top : progress
      JPanel topPanel = new JPanel();
      topPanel.setLayout(new BorderLayout());
      JLabel progressLabel = new JLabel("Progress");
      topPanel.add(progressLabel, BorderLayout.NORTH);

      JTextArea progress = new JTextArea("", 5, 60);
      JScrollPane progressView =
               new JScrollPane(progress, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
                        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      topPanel.add(progressView, BorderLayout.CENTER);

      // bottom : output
      JPanel bottomPanel = new JPanel();
      bottomPanel.setLayout(new BorderLayout());
      JPanel logTitlePanel = new JPanel();
      logTitlePanel.setLayout(new BoxLayout(logTitlePanel, BoxLayout.LINE_AXIS));
      JLabel logLabel = new JLabel("Log and output");
      logTitlePanel.add(logLabel);
      logTitlePanel.add(Box.createHorizontalStrut(10));
      this.disableLoggingButton = new JButton(DISABLE_LOGGING_TEXT);
      this.disableLoggingButton.addActionListener(this.disableLoggingButtonListener);
      logTitlePanel.add(this.disableLoggingButton);
      bottomPanel.add(logTitlePanel, BorderLayout.NORTH);

      JTextArea log = new JTextArea("", 15, 60);
      JScrollPane logView =
               new JScrollPane(log, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
                        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      bottomPanel.add(logView, BorderLayout.CENTER);

      // status bar
      JLabel statusBar = new JLabel("OpenDA ready");
      bottomPanel.add(statusBar, BorderLayout.SOUTH);

      // Set layout
      // Add the scroll panes to a split pane.
      JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
      splitPane.setTopComponent(topPanel);
      splitPane.setBottomComponent(bottomPanel);

      Dimension minimumSize = new Dimension(200, 200);
      topPanel.setMinimumSize(minimumSize);
      bottomPanel.setMinimumSize(minimumSize);
      splitPane.setDividerLocation(200);
      splitPane.setPreferredSize(new Dimension(300, 300));
      this.add(splitPane, BorderLayout.CENTER);

      guiResultWriter = new GUIResultWriter(progress, log, statusBar);
      guiResultWriter.setWriteResults(true);

      this.costPlotter = new PlotResultWriter(plot, "costtotal", "Cost", "Cost function evaluation", "", "Cost", "");

      myScreen = this;
   }

   public ApplicationRunner.Status getStatus() {
      if (this.runThread != null) { return this.runThread.getStatus(); }
      // no thead active (yet or any more)
      guiResultWriter.putStatus(null, "Application finished");
      return ApplicationRunner.Status.FINISHED;
   }

   public void startRun(File input) {
      /*
       * check input file for validity
       * only start if it is a valid file
       */
      boolean validInput = true;
      File workingDir = new File(".");
      String fileName = "";
      if (input == null) {
         validInput = false;
      }
      else {
         workingDir = input.getParentFile();
         fileName = input.getName();
         if (!fileName.endsWith(".xml") && !fileName.endsWith(".oda")) {
            validInput = false;
         }
         if (!input.exists()) {
            validInput = false;
         }
      }
      if (validInput) {
         resetOutput();
         guiResultWriter.putStatus(null, "Application running...");
         this.runThread = new ApplicationRunner(workingDir, fileName, true);
         SelectCases.setPostprocessorFactory(this.runThread.getStochModelFactory());
      }
      else {
         guiResultWriter.putStatus(null, "Input for application invalid");
      }
   }

   public void resetOutput() {
      this.costPlotter.reset();
      guiResultWriter.reset();
      this.outputGui.reset();
      Results.reset();
      Results.resetRegisterFile();
      Results.addResultWriter(ControlGui.guiResultWriter);
      Results.addResultWriter(this.outputGui);
      Results.addResultWriter(this.costPlotter, new ResultSelectionConfig("cost"));
      if (this.selectCases != null) {
         this.selectCases.resetOutput();
      }
   }

   public void stopRun() {
      if (this.runThread != null) {
         guiResultWriter.putStatus(null, "Application is being stopped... (finishing current loop)");
         this.runThread.stop();
      }
   }

   public void pauseOrResumeRun() {
      if (this.runThread != null) {
         if (this.runThread.getStatus() == ApplicationRunner.Status.PAUSED) {
            guiResultWriter.putStatus(null, "Application running...");
            this.runThread.resume();
         }
         else {
            guiResultWriter.putStatus(null, "Application paused");
            this.runThread.pause();
         }
      }
   }

   private ActionListener disableLoggingButtonListener = new ActionListener() {
       
       public void actionPerformed(ActionEvent e) {
           //toggle logging of results in guiResultWriter.
           guiResultWriter.setWriteResults(!guiResultWriter.isWriteResults());

           //update disableLoggingButton text.
           if (guiResultWriter.isWriteResults()) {
               disableLoggingButton.setText(DISABLE_LOGGING_TEXT);
           } else {
               disableLoggingButton.setText(ENABLE_LOGGING_TEXT);
           }
       }
   };

   
   public void actionPerformed(ActionEvent event) {
      Object source = event.getSource();
      if (source == this.openRestartButton) {
         IAlgorithm algorithm = this.runThread.getAlgorithm();
         File workingDir = this.openDaUserSettings.getLastUsedDir();
         File savedStateFile = new File(workingDir, "savedState.xml");
         savedStateFile = FileDialog.openInput(savedStateFile);
         if (savedStateFile.exists()) {
            IModelState state = algorithm.loadPersistentState(savedStateFile);
            algorithm.restoreInternalState(state);
            algorithm.releaseInternalState(state); // release storage
            Results.putProgression("Restoring state from " + savedStateFile.toString());
         }
         else {
            Results.putProgression("No saved state file was found");
         }
      }
      else if (source == this.saveRestartButton) {
         IAlgorithm algorithm = this.runThread.getAlgorithm();
         IModelState state = algorithm.saveInternalState();
         File workingDir = this.openDaUserSettings.getLastUsedDir();
         File savedStateFile = new File(workingDir, "savedState.xml");
         savedStateFile = FileDialog.saveInput(savedStateFile);
         state.savePersistentState(savedStateFile);
         algorithm.releaseInternalState(state);
         Results.putProgression("Saved state to " + savedStateFile.toString());
         guiResultWriter.putStatus(null, "Saved data to files");
      }
      else if (source == this.presentButton) {
         // TODO: raise existing window - or turn it into a pane
         this.selectCases = new SelectCases(this.openDaUserSettings.getLastUsedDir());
         SelectCases.setPostprocessorFactory(this.runThread.getStochModelFactory());
      }
   }

   public static void statusChangedHandler(ApplicationRunner.Status status) {
      if (myScreen == null) return;
      myScreen.updateButtonStatus(status);

      String statusText = "Application ";
      switch (status) {
      case ERROR:
         statusText += "error.";
         break;
      case FINISHED:
         statusText += "finished.";
         break;
      case INITIALIZED:
         statusText += "initialized.";
         break;
      case INITIALIZING:
         statusText += "initializing...";
         break;
      case PAUSED:
         statusText += "paused.";
         break;
      case RUNNING:
         statusText += "running...";
         break;
      case STOPPED:
         statusText += "stopped.";
         break;
      }
      guiResultWriter.putStatus(null, statusText);
   }

   private void updateButtonStatus(ApplicationRunner.Status status) {
      if (this.runThread != null) {

         this.saveRestartButton.setEnabled(
                    status == ApplicationRunner.Status.FINISHED ||
                             status == ApplicationRunner.Status.STOPPED ||
                             status == ApplicationRunner.Status.PAUSED);

         this.openRestartButton.setEnabled(
                    status == ApplicationRunner.Status.PAUSED);

      }
      else {
         this.saveRestartButton.setEnabled(false);
         this.openRestartButton.setEnabled(false);
      }
   }

   public Figure getFigure() {
      return this.costPlotter.getFigure();
   }
}
