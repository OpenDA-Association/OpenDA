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
import org.openda.uncertainties.Uncertainties;
import org.openda.uncertaintygui.MainFrameController;
import org.openda.uncertaintygui.MainPanel;
import org.openda.uncertaintygui.MainPanelController;
import org.openda.utils.io.UncertaintyWriter;

import javax.swing.*;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;
import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.io.StringWriter;

/**
 * Sets up gui component for editing the input. The panel is split in two, with the
 * left hand part showing a tree with the main input files and its includes. The right
 * hand part shows a component for editing a specific file (main or include).
 * @author verlaanm
 *
 */
public class InputGui extends JPanel
                      implements TreeSelectionListener {
	// lhs tree
	private JTree tree=null;
	private InputTree root=null;
	private InputTree currentNode=null;
	// rhs component editor
    private JEditorPane inputComponentPane;
    private JPanel inputPropertiesPane;
    private JScrollPane inputView;
    private JScrollPane propertiesView;
    private JSplitPane splitPane;
    private String[] editableTypes = {".oda", ".xml",".m",".csv",".tab",".swn",".loc"};

    //Optionally play with line styles.  Possible values are
    //"Angled" (the default), "Horizontal", and "None".
    private static boolean playWithLineStyle = false;
    private static String lineStyle = "Horizontal";

    private JPanel uncertaintyPanel = null;
    private MainPanelController mainPanelController = null;

    /**
     * Constructor for InputGui. Sets up dual pane editor for input.
     */
    public InputGui() {
        super(new GridLayout(1,0));

        //Create the nodes.
        this.root = new InputTree("",".","","",""); //TODO start empty
        this.currentNode = root;

        //Create a tree that allows one selection at a time.
        this.tree = new JTree(root);
        this.tree.getSelectionModel().setSelectionMode
                (TreeSelectionModel.SINGLE_TREE_SELECTION);

        //Listen for when the selection changes.
        this.tree.addTreeSelectionListener(this);

        if (playWithLineStyle) {
            this.tree.putClientProperty("JTree.lineStyle", lineStyle);
        }

        //Create the scroll pane and add the tree to it.
        JScrollPane treeView = new JScrollPane(this.tree);

        //Create the HTML viewing pane.
        inputComponentPane = new JEditorPane();
        inputComponentPane.setEditable(true);
        inputComponentPane.setFont(new Font("MonoSpaced", Font.PLAIN, 12));
        inputView = new JScrollPane(inputComponentPane);

        propertiesView = new JScrollPane(uncertaintyPanel);

        //Add the scroll panes to a split pane.
        splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        splitPane.setTopComponent(treeView);
        splitPane.setBottomComponent(inputView);

        Dimension minimumSize = new Dimension(200, 200);
        inputView.setMinimumSize(new Dimension(400, 300));
        treeView.setMinimumSize(minimumSize);
        splitPane.setDividerLocation(200);
        splitPane.setPreferredSize(new Dimension(300, 300));

        MainFrameController mainFrameController = new MainFrameController(MainFrameController.APPLICATION_TYPE.UNCERTAINTY_GUI_ONLY);
        MainPanel datoolsPanel = new MainPanel();
        mainPanelController = new MainPanelController(mainFrameController, datoolsPanel, "");

        // TODO: uncertaintyPanel = datoolsPanel.getVariationPerParameterPanel();
        uncertaintyPanel = datoolsPanel.getPdfDefinitionPanel();

        //Add the split pane to this panel.
        displayComponent(this.currentNode);
        add(splitPane);
    }

    /** Required by TreeSelectionListener interface. */
    public void valueChanged(TreeSelectionEvent e) {
    	InputTree node = (InputTree) this.tree.getLastSelectedPathComponent();

    	if (node == null) return;

    	// save buffer if needed
        saveBuffer();
    	// update
    	this.currentNode = node;
    	displayComponent(node);
    }

    /**
     * If there is no window event the buffer of the current nod may be out of sync with
     * the text in the editor.
     */
    private void checkForChanges(){
    	InputTree node = (InputTree) this.tree.getLastSelectedPathComponent();
        if (node == null){
        	node = this.root;
        }
        if (node.getTextEditable()) {
            node.setCurrentBuffer(this.inputComponentPane.getText());
        }
    }

    private void saveBuffer() {
        if (this.currentNode.getTextEditable()) {
            this.currentNode.setCurrentBuffer(this.inputComponentPane.getText());
        } else {
            try {
                StringWriter writer = new StringWriter();
                UncertaintyWriter uncertaintiesWriter = new UncertaintyWriter(writer);
                uncertaintiesWriter.write(this.mainPanelController.getModel().getUncertaintiesObject(), true);
                this.currentNode.setCurrentBuffer(writer.getBuffer().toString());
           } catch (IOException ioe) {
                // Ignore it - it won't happen?
           }
        }
    }

    /**
     * Open a new root file
     * @param workingDir Root file's working directory
     * @param fileName Root file's name (file is on working directory)
     */
    public void setRootFile(String workingDir, String fileName){
    	// set current file
        this.root = new InputTree(null, workingDir, fileName, "", "");
        this.currentNode = this.root;
        DefaultTreeModel tempTreeModel = new DefaultTreeModel(this.root);
        this.tree.setModel(tempTreeModel);
        // always show full tree
        int rowCount = this.tree.getRowCount();
        for(int row=rowCount-1;row>=0;row--){
        	this.tree.expandRow(row);
        }

        // refresh TODO ?
        displayComponent(this.currentNode);
    }

    /**
     * Update editor for input component.
     * @param node selected node
     */
    private void displayComponent(InputTree node) {
    	String text;
    	String workingDir = node.getWorkingDir();
    	String fileName = node.getFileName();
    	String parentDir = node.getParentDir();
    	String fullDir = (new File(parentDir,workingDir)).getPath();
    	// is this file editable?
    	boolean editable = false;
        boolean textEditable = true;
        for (String editableType : this.editableTypes) {
            if (fileName.endsWith(editableType)) {
                editable = true;
                break;
            }
        }

        File file = new File(fullDir, fileName);
        if(editable){
    		if(file.exists()){
    			text  = node.getCurrentBuffer();
                //if (text.indexOf("<uncertainties") >= 0) {
                //    textEditable = false;
                //}
                textEditable = node.getTextEditable();
    		}else{
    			text = InputTree.fileDoesNotExistMessage;
    			node.setCurrentBuffer(text);
    			node.setHash(text.hashCode()); //File is assumed to be modified if this text is modified.
    		}
    	}else{
            text = InputTree.fileNotEditableMessage;
    	}

    	//
        if (textEditable) {
    	    this.inputComponentPane.setText(text);
            this.inputComponentPane.setCaretPosition(0);
            this.splitPane.setBottomComponent(inputView);
        } else {
            String buffer = node.getCurrentBuffer();
            if (buffer.equals("")) {
                loadUncertaintySpecification(file);
            } else {
                loadUncertaintySpecification(node.getCurrentBuffer());
            }
            propertiesView = new JScrollPane(mainPanelController.getMainPanel().getPdfDefinitionPanel());
            this.splitPane.setBottomComponent(propertiesView);
        }
        node.setTextEditable(textEditable);
    }


    public void saveInput(){
    	// save buffer if needed
    	//this.currentNode.setCurrentBuffer(this.inputComponentPane.getText());
        saveBuffer();
    	this.root.saveTree(true);
    }

    public boolean needsSave(){
    	this.checkForChanges();
    	return this.root.treeHasChanged();
    }

    private void loadUncertaintySpecification(File fileName) {
        //VariationPerParameterTableController variationPerParameterTableController = new VariationPerParameterTableController(mainPanelController, uiModel);

        // TODO: uncertaintyPanel = datoolsPanel.getVariationPerParameterPanel();
        mainPanelController.loadUncertaintySpecificationFile(fileName.getAbsolutePath(), null);
        mainPanelController.setUncertaintyMethod(Uncertainties.PDF);
    }

    private void loadUncertaintySpecification(String buffer) {
        //VariationPerParameterTableController variationPerParameterTableController = new VariationPerParameterTableController(mainPanelController, uiModel);

        // TODO: uncertaintyPanel = datoolsPanel.getVariationPerParameterPanel();
        mainPanelController.loadUncertaintySpecificationFile(null, buffer);
        mainPanelController.setUncertaintyMethod(Uncertainties.PDF);
    }
}

