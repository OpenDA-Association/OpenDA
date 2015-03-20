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

package org.openda.application.gui;

import org.openda.application.ApplicationRunner;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelPostProcessor;
import org.openda.utils.StoredResult;
import org.openda.blackbox.config.BBUtils;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class SelectCases extends JFrame implements ActionListener {

    private JMenuBar menubar;
    private JMenu file;
    private JMenuItem fileClose;
    private DefaultListModel availableCases;
    private DefaultListModel selectedCases;
    private DefaultListModel presentCases;
    private JList availableCasesList;
    private JList selectedCasesList;
    private JList presentCasesList;

    private JButton addButton;
    private JButton removeButton;
    private JButton generateButton;

    private File workingDir = null;

    private static IStochModelPostProcessor postProcessor = null;
    private static IStochModelFactory factory = null;
    private static File input = null;

    public SelectCases(File workingDir) {

        super("Select cases - OpenDaApplication");
        setSize(550, 430);
        setLayout(new BorderLayout());

        this.workingDir = workingDir;

        // add menu
        menubar = new JMenuBar();
        file = new JMenu("File");
        fileClose = new JMenuItem("Close", KeyEvent.VK_C);

        file.add(fileClose);
        file.setMnemonic(KeyEvent.VK_F);
        menubar.add(file);
        this.setJMenuBar(menubar);
        fileClose.addActionListener(this);

        // Top half: two lists and some buttons
        JPanel topPanel = new JPanel();
        topPanel.setLayout(new BorderLayout());

        availableCases = new DefaultListModel();
        availableCasesList = new JList(availableCases);
        availableCasesList.setVisibleRowCount(-1);
        availableCasesList.setLayoutOrientation(JList.VERTICAL);
        JScrollPane availablePane = new JScrollPane(availableCasesList);
        availablePane.setPreferredSize(new Dimension(200, 150));
        topPanel.add(availablePane, BorderLayout.WEST);

        selectedCases = new DefaultListModel();
        selectedCasesList = new JList(selectedCases);
        selectedCasesList.setVisibleRowCount(-1);
        selectedCasesList.setLayoutOrientation(JList.VERTICAL);
        JScrollPane selectedPane = new JScrollPane(selectedCasesList);
        selectedPane.setPreferredSize(new Dimension(200, 150));
        topPanel.add(selectedPane, BorderLayout.EAST);

        JPanel centrePanel = new JPanel();
        BorderLayout centreBorder = new BorderLayout();
        centreBorder.setHgap(30);
        centreBorder.setVgap(30);
        centrePanel.setLayout(centreBorder);

        addButton = new JButton("Add >>");
        removeButton = new JButton("Remove <<");
        generateButton = new JButton("Generate");

        addButton.setSize(50, 20);
        removeButton.setSize(50, 20);
        generateButton.setSize(50, 20);

        JPanel centrePanelTop = new JPanel();
        centrePanelTop.setLayout(new BorderLayout());
        JLabel emptyLabel = new JLabel("");

        centrePanelTop.add(addButton, BorderLayout.NORTH);
        centrePanelTop.add(removeButton, BorderLayout.SOUTH);

        centrePanel.add(centrePanelTop, BorderLayout.NORTH);
        centrePanel.add(emptyLabel, BorderLayout.CENTER);
        centrePanel.add(generateButton, BorderLayout.SOUTH);
        topPanel.add(centrePanel, BorderLayout.CENTER);

        // bottom half: available plots
        JPanel bottomPanel = new JPanel();
        bottomPanel.setLayout(new BorderLayout());
        JLabel logLabel = new JLabel("Current presentations:");
        presentCases = new DefaultListModel();
        presentCasesList = new JList(presentCases);
        presentCasesList.setVisibleRowCount(-1);
        presentCasesList.setLayoutOrientation(JList.VERTICAL);
        JScrollPane presentPane = new JScrollPane(presentCasesList);
        presentPane.setPreferredSize(new Dimension(200, 150));

        bottomPanel.add(logLabel, BorderLayout.NORTH);
        bottomPanel.add(presentPane, BorderLayout.SOUTH);

        //add(topPanel, BorderLayout.NORTH);
        //add(bottomPanel, BorderLayout.SOUTH);

        addButton.addActionListener(this);
        removeButton.addActionListener(this);
        generateButton.addActionListener(this);

        //Set layout
        //Add the scroll panes to a split pane.
        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        splitPane.setTopComponent(topPanel);
        splitPane.setBottomComponent(bottomPanel);

        Dimension minimumSize = new Dimension(200, 200);
        topPanel.setMinimumSize(minimumSize);
        bottomPanel.setMinimumSize(minimumSize);
        splitPane.setDividerLocation(200);
        splitPane.setPreferredSize(new Dimension(300, 300));
        this.add(splitPane, BorderLayout.CENTER);

        if (input != null) {
            fillLists();
        }

        setVisible(true);
    }


    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == this.addButton) {
            int[] selectedIndices = availableCasesList.getSelectedIndices();
            for (int i = 0; i < selectedIndices.length; i ++) {
                StoredResult newElement = (StoredResult) availableCases.getElementAt(selectedIndices[i]);

                boolean addIt = true;
                for (int j = 0; j < selectedCases.getSize(); j ++) {
                    StoredResult element = (StoredResult) selectedCases.getElementAt(j);
                    if (element.toString().equals(newElement.toString())) {
                        addIt = false;
                    }
                }
                if (addIt) {
                    selectedCases.addElement(newElement);
                }
            }
        } else if (source == this.removeButton) {
            int[] selectedIndices = selectedCasesList.getSelectedIndices();
            for (int i = selectedIndices.length-1; i >= 0; i --) {
                selectedCases.removeElement(selectedCases.getElementAt(selectedIndices[i]));
            }
        } else if (source == this.generateButton) {
            //JOptionPane.showMessageDialog(null, "Generating pictures ...", "Select cases", JOptionPane.INFORMATION_MESSAGE);
            runPresentationProgram();
        } else if (source== this.fileClose) {
            dispose();
        }
    }

    private void fillLists() {

        // Available cases ...
        availableCases.removeAllElements();
        StoredResult[] availableResults = StoredResult.getStoredResults(
                new File(BBUtils.getFileNameWithoutExtension(input.getAbsolutePath()) + ".orp"), false);
        for (StoredResult result: availableResults) {
            availableCases.addElement(result);
        }

        // Selected cases ...
        selectedCases.removeAllElements();

        // Available presentations ...
        StoredResult[] availablePresentations = StoredResult.getStoredResults(
                new File(BBUtils.getFileNameWithoutExtension(input.getAbsolutePath()) + ".oap"), true);
        for (StoredResult presentation : availablePresentations) {
            boolean addIt = true;
            for (int j = 0; j < presentCases.getSize(); j++) {
                StoredResult element = (StoredResult) presentCases.getElementAt(j);
                if (element.toString().equals(presentation.toString())) {
                    addIt = false;
                }
            }
            if (addIt) {
                presentCases.addElement(presentation);
            }
        }
    }

    private void runPresentationProgram() {
        if (factory == null) {
            if (input == null) {
                JOptionPane.showMessageDialog(null, "No model input has been selected yet", "Select cases", JOptionPane.INFORMATION_MESSAGE);
                return;
            }
            factory = ApplicationRunner.createStochModelFactoryComponent(input);
        }

    // TODO    
    //    if (!(factory instanceof IStochModelPostProcessor)) {
    //        JOptionPane.showMessageDialog(null, "Chosen model has no postprocessing counterpart", "Select cases", JOptionPane.INFORMATION_MESSAGE);
    //        return;
    //    }

        try {
            FileWriter writer = new FileWriter(new File(BBUtils.getFileNameWithoutExtension(input.getAbsolutePath()) + ".oap"), true);
            for (int i = 0; i < selectedCases.getSize(); i ++) {
                StoredResult selectedResult = (StoredResult) selectedCases.getElementAt(i);
                postProcessor = factory.getPostprocessorInstance(new File(input.getParent(), selectedResult.getDirectory()));
                postProcessor.produceAdditionalOutput();

                writer.write(selectedResult.getDirectory() + "\n");
                writer.write(selectedResult.toString() + "\n");
            }

            writer.close();

            fillLists();
        } catch (IOException e) {
            JOptionPane.showMessageDialog(null, "Error while producing presentations:\n"
                + e.getMessage(), "Select cases", JOptionPane.INFORMATION_MESSAGE);
        }

        //
    }

    public static void setPostprocessorFactory(IStochModelFactory factoryInstance) {
        factory = factoryInstance;
    }

    public static void newInputFile(File inputFile) {
        input = inputFile;
    }

    public void resetOutput() {
        File registeredCasesFile = new File(BBUtils.getFileNameWithoutExtension(input.getAbsolutePath()) + ".orp");
        registeredCasesFile.delete();
        File registeredPresentationsFile = new File(BBUtils.getFileNameWithoutExtension(input.getAbsolutePath()) + ".oap");
        registeredPresentationsFile.delete();
        fillLists();
    }
}
