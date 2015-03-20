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

import org.openda.uncertainties.UncertainItem;
import org.openda.uncertainties.Uncertainties;
import org.openda.uncertainties.autocorrelationfunctions.AutoCorrelationFunction;
import org.openda.uncertainties.pdfs.NormalDistribution;
import org.openda.uncertainties.pdfs.PDF;
import org.openda.uncertainties.variationfunctions.RangeVariation;
import org.openda.uncertainties.variationfunctions.Variation;
import org.openda.uncertaintygui.MainFrameController.APPLICATION_TYPE;
import org.openda.uncertaintygui.tablemodels.PDFContext;
import org.openda.uncertaintygui.tablemodels.ResultSelectionModel;
import org.openda.uncertaintygui.tablemodels.UIModel;
import org.openda.utils.io.UncertaintyWriter;

import javax.swing.*;
import javax.swing.table.TableModel;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;


/**
 * The controller class for the panel GUI panel containing all tabs.
 */
public class MainPanelController {

    //enumeration for different uncertainty analysis methods.
	public enum UNCERTAINTY_ANALYSIS_METHOD {
		MONTE_CARLO, VARIATION_PER_PARAMETER
	}

    //constants for names of items in comboboxes.
    private static final String[] applicationTypeList = {"Uncertainty Analysis", "Calibration", "Data Assimilation"};
    private static final String[] uncertaintyAnalysisMethodList = {"Monte Carlo (PDF Definition)", "Variation Per Parameter"};
    private static final String[] calibrationMethodList = {"Dud", "Powell", "Simplex"};
    private static final String[] dataAssimilationMethodList = {"Ensemble Kalman Filter", "Particle Filter"};

    //The FileChooser used for loading and saving files. Using an instance field to
    //remember file selections within one session.
    private JFileChooser fileChooser;

	//to store the application path.
	//application path is the default directory where the filechooser
	//starts when browsing for files.
    private String applicationPath;

    //reference to mainFrameController.
	private MainFrameController parentController;

	//reference to mainPanel.
	private MainPanel mainPanel;

    //reference to about box.
    private DaGuiAboutBox daGuiAboutBox;

    //table controllers.
	private PDFDefinitionTableController pdfDefinitionTableController;
	private VariationPerParameterTableController variationPerParameterTableController;
	private CorrelationTableController correlationTableController;
	private ResultsTableController resultsTableController;

	//to indicate which graph type to plot for PDFDefinition.
    private PDFContext.GRAPH_TYPE currentPDFGraphType;

	//object that stores all GUI data and all model configuration data in memory.
	private UIModel uiModel;


    /**
     * Creates a new MainPanelController.
     *
     * @param parentController the mainFrame controller
     * @param mainPanel main GUI panem
     * @param applicationPath the application path
     */
    public MainPanelController(MainFrameController parentController,
    		MainPanel mainPanel, String applicationPath) {

    	//initialize GUI.
        this.currentPDFGraphType = PDFContext.GRAPH_TYPE.PDF_MARGINAL;
    	this.parentController = parentController;
    	this.mainPanel = mainPanel;
        this.initParameterAndGraphPanels();
    	this.mainPanel.setVisible(true);
        this.mainPanel.getMainTabbedPane().setVisible(true);

		//initialize file chooser.
        this.applicationPath = applicationPath;
        this.fileChooser = new JFileChooser();
        this.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        this.fileChooser.setSelectedFile(new File(this.applicationPath));

        //initialize applicationTypeComboBox.
    	this.mainPanel.getApplicationTypeComboBox().removeAllItems();
        for (String string : applicationTypeList) {
            mainPanel.getApplicationTypeComboBox().addItem(string);
        }

        //initialize uncertaintyAnalysisMethodComboBox.
    	this.mainPanel.getUncertaintyAnalysisMethodComboBox().removeAllItems();
        for (String string : uncertaintyAnalysisMethodList) {
            mainPanel.getUncertaintyAnalysisMethodComboBox().addItem(string);
        }

        //initialize calibrationMethodComboBox.
    	this.mainPanel.getCalibrationMethodComboBox().removeAllItems();
        for (String string : calibrationMethodList) {
            mainPanel.getCalibrationMethodComboBox().addItem(string);
        }

        //initialize dataAssimilationMethodComboBox.
    	this.mainPanel.getDataAssimilationMethodComboBox().removeAllItems();
        for (String string : dataAssimilationMethodList) {
            mainPanel.getDataAssimilationMethodComboBox().addItem(string);
        }

    	//create tables with empty model.
		this.uiModel = new UIModel(); // empty

    	//initialize table controllers.
		this.pdfDefinitionTableController = new PDFDefinitionTableController(this, uiModel);
        this.variationPerParameterTableController = new VariationPerParameterTableController(this, uiModel);
        this.correlationTableController = new CorrelationTableController(this, uiModel);
        this.resultsTableController = new ResultsTableController(this, uiModel);

        //configure GUI for default application type.
        if (this.uiModel.getUaToolsGuiConfiguration().getUncertaintiesObject() != null) {
    	    this.uiModel.getUaToolsGuiConfiguration().getUncertaintiesObject()
    	        .setUncertaintyType(Uncertainties.PDF);
        }
    	this.configureControlsForApplicationAndMethodType();

        initEventHandlers();
    }

    public MainPanelController(MainPanel mainPanel) {
        this.mainPanel = mainPanel;
        this.uiModel = new UIModel();
        //this.applicationType = MainFrameController.APPLICATION_TYPE.UNCERTAINTY_ANALYSIS;
    }

	/**
	 * Sets custom configurations and border titles for all
	 * parameterAndGraphPanels.
	 */
	private void initParameterAndGraphPanels() {
		// configure all parameterAndGraphPanels.
		this.mainPanel.getPdfParameterAndGraphPanel().setBorderTitles(
				"PDF Graph", "PDF Parameters Table");
		this.mainPanel.getAutoCorrelationParameterAndGraphPanel()
				.setBorderTitles("Auto-Correlation Model Graph",
						"Auto-Correlation Parameters Table");
		this.mainPanel.getAutoCorrelationParameterAndGraphPanel()
				.getViewingModePanel().setVisible(false);
		this.mainPanel.getvariationPerParameterSidePanel()
				.setBorderTitles("Not Available", "Variation Parameters Table");
		this.mainPanel.getvariationPerParameterSidePanel()
				.getGraphPlotPanel().setVisible(false);
		this.mainPanel.getvariationPerParameterSidePanel()
				.getViewingModePanel().setVisible(false);
	}

    /**
     * initializes event handlers
     */
    private void initEventHandlers() {

        this.mainPanel.getApplicationTypeComboBox().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                selectApplicationTypeActionPerformed();
            }
        });

        this.mainPanel.getUncertaintyAnalysisMethodComboBox().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                selectUncertaintyAnalysisMethodActionPerformed();
            }
        });

        this.mainPanel.getCalibrationMethodComboBox().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                selectCalibrationMethodActionPerformed();
            }
        });

        this.mainPanel.getUncertaintyAnalysisMethodComboBox().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                selectDataAssimilationMethodActionPerformed();
            }
        });

        this.mainPanel.getLoadUncertaintySpecificationFileButton().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
				loadUncertaintySpecificationFileActionPerformed();
            }
        });

        this.mainPanel.getAddRowPdfDefinitionButton().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                addPdfRow();
            }
        });


        this.mainPanel.getDeleteRowPdfDefinitionButton().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                deletePdfRow(pdfDefinitionTableController.getSelectedRow());
            }
        });

        this.mainPanel.getAddRowVariationPerParameterButton().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                addVariationFunctionRow();
            }
        });

        this.mainPanel.getDeleteRowVariationPerParameterButton().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                deleteVariationFunctionRow(variationPerParameterTableController.getSelectedRow());
            }
        });

        this.mainPanel.getAddRowResultsButton().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                addResultsRow(mainPanel.getResultsTable().getModel());
            }
        });

        this.mainPanel.getDeleteRowResultsButton().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                deleteResultsRow(mainPanel.getResultsTable(), mainPanel.getResultsTable().getModel());
            }
        });

        this.mainPanel.getPdfParameterAndGraphPanel().getProbabilityRadioButton().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
            	setCurrentPDFGraphType(PDFContext.GRAPH_TYPE.PDF_MARGINAL);
                pdfDefinitionTableController.updatePDFDefinitionParameterAndGraphPanel();
            }
        });

        this.mainPanel.getPdfParameterAndGraphPanel().getCumulativeRadioButton().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
            	setCurrentPDFGraphType(PDFContext.GRAPH_TYPE.PDF_CUMULATIVE);
                pdfDefinitionTableController.updatePDFDefinitionParameterAndGraphPanel();
            }
        });
    }

	/**
	 * Method fired when changing the ApplicationType to update the models
	 * and GUI.
	 */
    private void selectApplicationTypeActionPerformed() {
        if (mainPanel.getApplicationTypeComboBox().getSelectedIndex() >= 0) {
	       	switch(mainPanel.getApplicationTypeComboBox().getSelectedIndex()) {
	       		case 0://Uncertainty Analysis
	       			this.parentController.setApplicationType(APPLICATION_TYPE.UNCERTAINTY_ANALYSIS);
	       			break;
	       		case 1://Calibration
	       			this.parentController.setApplicationType(APPLICATION_TYPE.CALIBRATION);
	       			break;
	       		case 2://Data Assimilation
	       			this.parentController.setApplicationType(APPLICATION_TYPE.DATA_ASSIMILATION);
	       			break;
	       	    default:
	       	    	//do nothing.
	       	    	break;
	       	}

	       	this.configureControlsForApplicationAndMethodType();

	       	updateGUI();
        }
	}

	/**
	 * Method fired when changing the UncertaintyAnalysisMethod to update the models
	 * and GUI.
	 */
    private void selectUncertaintyAnalysisMethodActionPerformed() {

        if (mainPanel.getUncertaintyAnalysisMethodComboBox().getSelectedIndex() >= 0) {

        	//update uncertainty type in model data.
        	if(mainPanel.getUncertaintyAnalysisMethodComboBox().getSelectedIndex() == 0) {
        		this.uiModel.getUaToolsGuiConfiguration().getUncertaintiesObject()
        				.setUncertaintyType(Uncertainties.PDF);
        	}
        	else if(mainPanel.getUncertaintyAnalysisMethodComboBox().getSelectedIndex() == 1) {
        		this.uiModel.getUaToolsGuiConfiguration().getUncertaintiesObject()
        				.setUncertaintyType(Uncertainties.VARIATION_PER_PARAMETER);
        	}

            this.configureControlsForApplicationAndMethodType();

	       	updateGUI();
        }
    }

    public void setUncertaintyMethod(int uncertaintyMethod) {
        this.uiModel.getUaToolsGuiConfiguration().getUncertaintiesObject()
                .setUncertaintyType(uncertaintyMethod);
        this.configureControlsForApplicationAndMethodType();
        updateGUI();
    }

	/**
	 * Method fired when changing the CalibrationMethod to update the models
	 * and GUI.
	 */
	protected void selectCalibrationMethodActionPerformed() {
    	//TODO implement (see selectUncertaintyAnalysisMethodActionPerformed) AK
	}

	/**
	 * Method fired when changing the DataAssimilationMethod to update the models
	 * and GUI.
	 */
    protected void selectDataAssimilationMethodActionPerformed() {
    	//TODO implement (see selectUncertaintyAnalysisMethodActionPerformed) AK
	}

    /**
     * Configure visibility of GUI controls for current application type.
     * This method works by removing and adding panels to mainTabbedPane to display
     * tabs as required.
     */
    private void configureControlsForApplicationAndMethodType() {

    	//remove all optional tabs.
    	this.mainPanel.getMainTabbedPane().remove(this.mainPanel.getPdfDefinitionPanel());
    	this.mainPanel.getMainTabbedPane().remove(this.mainPanel.getAutoCorrelationPanel());
    	this.mainPanel.getMainTabbedPane().remove(this.mainPanel.getVariationPerParameterPanel());
    	this.mainPanel.getMainTabbedPane().remove(this.mainPanel.getResultsPanel());

    	if (this.parentController.getApplicationType() == MainFrameController.APPLICATION_TYPE.UNCERTAINTY_ANALYSIS) {
        	//select Uncertainty Analysis in combobox.
        	this.mainPanel.getApplicationTypeComboBox().setSelectedItem(applicationTypeList[0]);

    		if (this.uiModel.getUaToolsGuiConfiguration().getUncertaintiesObject() == null ||
                this.uiModel.getUaToolsGuiConfiguration().getUncertaintiesObject()
    				.getUncertaintyType() == Uncertainties.PDF) {
	        	//select Monte Carlo in combobox.
	        	this.mainPanel.getUncertaintyAnalysisMethodComboBox().setSelectedItem(uncertaintyAnalysisMethodList[0]);

    			//add PDF Definition tab.
    			//the title of a given tab corresponds to component.getName().
    	    	this.mainPanel.getMainTabbedPane().add(this.mainPanel.getPdfDefinitionPanel());

	        	//set monte carlo properties panel visible.
	        	this.mainPanel.getMonteCarloPropertiesPanel().setVisible(true);
            }
    		else if (this.uiModel.getUaToolsGuiConfiguration().getUncertaintiesObject()
    				.getUncertaintyType() == Uncertainties.VARIATION_PER_PARAMETER) {
	        	//select VPP in combobox.
	        	this.mainPanel.getUncertaintyAnalysisMethodComboBox().setSelectedItem(uncertaintyAnalysisMethodList[1]);

    			//add Variation tab.
    	    	this.mainPanel.getMainTabbedPane().add(this.mainPanel.getVariationPerParameterPanel());

	        	//set monte carlo properties panel invisible.
	        	this.mainPanel.getMonteCarloPropertiesPanel().setVisible(false);
    		}

			//add Results tab.
	    	this.mainPanel.getMainTabbedPane().add(this.mainPanel.getResultsPanel());

        	//set uncertainty analysis method label, combobox visible.
        	this.mainPanel.getUncertaintyAnalysisMethodLabel().setVisible(true);
        	this.mainPanel.getUncertaintyAnalysisMethodComboBox().setVisible(true);
        	//set calibration method label, combobox invisible.
        	this.mainPanel.getCalibrationMethodLabel().setVisible(false);
        	this.mainPanel.getCalibrationMethodComboBox().setVisible(false);
        	//set data assimilation method label, combobox invisible.
        	this.mainPanel.getDataAssimilationMethodLabel().setVisible(false);
        	this.mainPanel.getDataAssimilationMethodComboBox().setVisible(false);
    	}
    	else if (this.parentController.getApplicationType() ==  MainFrameController.APPLICATION_TYPE.CALIBRATION) {
        	//select Calibration in combobox.
        	this.mainPanel.getApplicationTypeComboBox().setSelectedItem(applicationTypeList[1]);

        	//add Variation tab.
	    	this.mainPanel.getMainTabbedPane().add(this.mainPanel.getVariationPerParameterPanel());
			//add Results tab.
	    	this.mainPanel.getMainTabbedPane().add(this.mainPanel.getResultsPanel());

        	//set uncertainty analysis method label, combobox invisible.
        	this.mainPanel.getUncertaintyAnalysisMethodLabel().setVisible(false);
        	this.mainPanel.getUncertaintyAnalysisMethodComboBox().setVisible(false);
        	//set monte carlo properties panel invisible.
        	this.mainPanel.getMonteCarloPropertiesPanel().setVisible(false);
        	//set calibration method label, combobox visible.
        	this.mainPanel.getCalibrationMethodLabel().setVisible(true);
        	this.mainPanel.getCalibrationMethodComboBox().setVisible(true);
        	//set data assimilation method label, combobox invisible.
        	this.mainPanel.getDataAssimilationMethodLabel().setVisible(false);
        	this.mainPanel.getDataAssimilationMethodComboBox().setVisible(false);
    	}
        else if (this.parentController.getApplicationType() == MainFrameController.APPLICATION_TYPE.DATA_ASSIMILATION) {
        	//select Data Assimilation in combobox.
        	this.mainPanel.getApplicationTypeComboBox().setSelectedItem(applicationTypeList[2]);

        	//add PDF Definition tab.
	    	this.mainPanel.getMainTabbedPane().add(this.mainPanel.getPdfDefinitionPanel());
	    	//add Auto Correlation tab.
	    	this.mainPanel.getMainTabbedPane().add(this.mainPanel.getAutoCorrelationPanel());

        	//set uncertainty analysis method label, combobox invisible.
        	this.mainPanel.getUncertaintyAnalysisMethodLabel().setVisible(false);
        	this.mainPanel.getUncertaintyAnalysisMethodComboBox().setVisible(false);
        	//set monte carlo properties panel invisible.
        	this.mainPanel.getMonteCarloPropertiesPanel().setVisible(false);
        	//set calibration method label, combobox invisible.
        	this.mainPanel.getCalibrationMethodLabel().setVisible(false);
        	this.mainPanel.getCalibrationMethodComboBox().setVisible(false);
        	//set data assimilation method label, combobox visible.
        	this.mainPanel.getDataAssimilationMethodLabel().setVisible(true);
        	this.mainPanel.getDataAssimilationMethodComboBox().setVisible(true);
    	}
    }


    /**
     * Display a file chooser and loads selected uncertainty specification file.
     */
	public void loadUncertaintySpecificationFileActionPerformed() {
    	File file = this.uiModel.getUaToolsGuiConfiguration().getUncertaintiesSpecificationFile();
    	if (file != null) {
    		//set filechooser to current uncertainty specification file path.
    		this.fileChooser.setSelectedFile(file);
    	}

		this.fileChooser.setDialogTitle("Open Uncertainty Specification File");
		int returnValue = fileChooser.showOpenDialog(this.mainPanel.getMainTabbedPane());
		if (returnValue == JFileChooser.APPROVE_OPTION) {
			// get full path of selected file.
			String filename = fileChooser.getSelectedFile().toString();
			this.loadUncertaintySpecificationFile(filename, null);
		} else {
			// do nothing.
		}
	}

    /**
     * New Configuration Action Performed. Closes the current model configuration,
     * so that the user can start from scratch.
     */
	public void newConfigurationActionPerformed() {
		//TODO new project menu item does the same as close project menu item. AK

		//TODO ask user if changes should be saved before closing. AK

		//close model.
		this.closeProject();
	}

    /**
     * Close Configuration Action Performed. Closes the current model configuration,
     * so that the user can start from scratch.
     */
    public void closeConfigurationActionPerformed() {
    	//TODO ask user if changes should be saved before closing. AK

		//close model.
		this.closeProject();
    }

    /**
     * Display a file chooser and saves current uncertainty specification
     * to selected file.
     */
	public void saveUncertaintySpecificationAsActionPerformed() {
    	File file = this.uiModel.getUaToolsGuiConfiguration().getUncertaintiesSpecificationFile();
    	if (file != null) {
    		//set filechooser to current uncertainty specification file path.
    		this.fileChooser.setSelectedFile(file);
    	}

    	this.fileChooser.setDialogTitle("Save Uncertainty Specification As");
		int returnValue = fileChooser.showSaveDialog(this.mainPanel.getMainTabbedPane());
		if (returnValue == JFileChooser.APPROVE_OPTION) {
			// get full path of selected file.
			String filename = fileChooser.getSelectedFile().toString();

			//add extension .xml, if no extension present.
			if (filename != null) {
				int index = filename.lastIndexOf('.');
				if (index == -1) {//if filename does not have an extension.
    				filename = filename + ".xml";
				}
			}

			this.saveUncertaintySpecificationAs(filename);
		} else {
			// do nothing.
		}
	}

    public void showHelpActionPerformed() {
        showAboutBoxActionPerformed();  // TODO: create help
    }

    public void showAboutBoxActionPerformed() {
        if (daGuiAboutBox == null) {
            daGuiAboutBox = new DaGuiAboutBox(parentController.getParentFrame());
        }
        daGuiAboutBox.setVisible(true);
    }


	/**
     * Load the specified uncertainty specification file.
     *
     * @param fullFilePath the full path of the file
     */
    public void loadUncertaintySpecificationFile(String fullFilePath, String buffer) {

		//create file object from full path.
		File file = null;
        StringReader reader = null;
        if (fullFilePath != null) {
            file = new File(fullFilePath);
        }
        else {
            if (buffer == null) {
                showMessageDialog("Error Loading Uncertainty Specification File",
                    "Error Loading Uncertainty Specification File: invalid filename specified.");
                return;
            } else {
                reader = new StringReader(buffer);
            }
        }

        //check if file exists.
        if (file != null && !file.exists()) {
            showMessageDialog("Error Loading Uncertainty Specification File",
            		"Error Loading Uncertainty Specification File: file '" + fullFilePath + "' does not exist.");
        	return;
        }

    	//read uncertainty specification file.
    	//replaces current uncertainties object in data model, even if read error.
        try {
            this.uiModel.readUncertaintyFiles(file, reader);

	        //configure GUI for uncertainty type (Monte Carlo or VPP).
	    	this.configureControlsForApplicationAndMethodType();
        } catch (IOException e) {
            showMessageDialog("Error Loading Uncertainty Specification File",
            		"Error Loading Uncertainty Specification File: " + e.getMessage());
        }
        updateGUI();
	}

	/**
     * Close the ui model.
     */
    private void closeProject() {
    	//sets new empty model.
		this.setModel(new UIModel());//empty model.
    }

    /**
     * Save the current uncertainty specification to the specified file.
     *
     * @param fullFilePath the full path of the file
     */
	private void saveUncertaintySpecificationAs(String fullFilePath) {
		//create file object from full path.
		File file = null;
        if (fullFilePath != null) {
            file = new File(fullFilePath);
        }
        else {
            showMessageDialog("Error Saving Uncertainty Specification",
            		"Error Saving Uncertainty Specification: invalid filename specified.");
        	return;
        }

        //check if file exists.
        if (file != null && file.exists()) {
        	//ask user confirmation to overwrite file.
        	String message = "File '" + fullFilePath + "' already exists. Do you want to replace it?";
        	int option = JOptionPane.showConfirmDialog(this.mainPanel.getMainTabbedPane(),
        			message, "Save As", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

        	if (option == JOptionPane.YES_OPTION) {
        		//do nothing.
        	} else if (option == JOptionPane.NO_OPTION || option == JOptionPane.CANCEL_OPTION) {
            	return;
        	}
        }

        //write file.
		UncertaintyWriter uncertaintyWriter = null;
		try {
			uncertaintyWriter = new UncertaintyWriter(file);
            uncertaintyWriter.write(this.uiModel.getUncertaintiesObject());

            showMessageDialog("Uncertainty Specification Saved",
            		"Uncertainty specification saved successfully.");
            //put uncertainty specification file in underlying data model.
            this.uiModel.getUaToolsGuiConfiguration().setUncertaintiesSpecificationFile(file);
		} catch (IOException e) {
            showMessageDialog("Error Saving Uncertainty Specification",
            		"Error Saving Uncertainty Specification: " + e.getMessage());
		} finally {
			//close writer.
			try {
				uncertaintyWriter.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

        //update GUI.
        updateGUI();
	}


    /**
     * Add (append) a row to pdf definition tables.
     */
    private void addPdfRow() {
		//create default normal pdf which is inactive.
		PDF pdf = new NormalDistribution();
		UncertainItem uncertainItem = new UncertainItem("Enter Id here", "Enter description here", false);
		pdf.setUncertainItem(uncertainItem);

		//add to data model.
		this.uiModel.getUncertaintiesObject().addPdf(pdf);
		updateGUI();

        //select the added row (the last row).
		this.pdfDefinitionTableController.setSelectedRow(this.uiModel.getUncertaintiesObject().pdfCount() - 1);
    }

    /**
     * Delete a row from pdf definition table.
     *
     * @param tableRowIndex index of row to be deleted
     */
    private void deletePdfRow(int tableRowIndex) {
    	if (tableRowIndex >= 0 && tableRowIndex < this.uiModel.getUncertaintiesObject().pdfCount()) {
    		//get autocorrelation object with same Id as pdf.
			PDF pdf = this.uiModel.getUncertaintiesObject().getPdf(tableRowIndex);
			//get autocorrelation object with same Id as pdf.
			AutoCorrelationFunction autoCorrelationFunction = this.uiModel.getUncertaintiesObject()
			    .getAutoCorrelationFunction(pdf.getUncertainItem().getId());
			//remove autoCorrelationFunction for pdf, if present.
			if (autoCorrelationFunction != null) {
				this.uiModel.getUncertaintiesObject().removeAutoCorrelationFunction(autoCorrelationFunction);
			}

			//remove pdf.
    		this.uiModel.getUncertaintiesObject().removePdf(tableRowIndex);
    		updateGUI();

            //select first row.
    		this.pdfDefinitionTableController.setSelectedRow(0);
    		this.correlationTableController.setSelectedRow(0);
    	}
    }

    /**
     * Add (append) a row to variation per parameter table.
     */
	private void addVariationFunctionRow() {
		//create default range variationFunction, which is inactive.
		Variation variationFunction = new RangeVariation();
		UncertainItem uncertainItem = new UncertainItem("Enter Id here", "Enter description here", false);
		variationFunction.setUncertainItem(uncertainItem);

		//add to data model.
		this.uiModel.getUncertaintiesObject().addVariationFunction(variationFunction);
		updateGUI();

        //select the added row (the last row).
		this.variationPerParameterTableController.setSelectedRow(this.uiModel.getUncertaintiesObject().variationFunctionCount() - 1);
	}

    /**
     * Delete a row from variation per parameter table.
     *
     * @param tableRowIndex index of row to be deleted
     */
	private void deleteVariationFunctionRow(int tableRowIndex) {
    	if (tableRowIndex >= 0 && tableRowIndex < this.uiModel.getUncertaintiesObject().variationFunctionCount()) {
			this.uiModel.getUncertaintiesObject().removeVariationFunction(tableRowIndex);
			updateGUI();

            //select first row.
    		this.variationPerParameterTableController.setSelectedRow(0);
        }
	}

    /**
     * Add (append) a result row.
     *
     * @param model
     */
    private void addResultsRow(TableModel model) {
    	if (model instanceof ResultSelectionModel) {
            ((ResultSelectionModel) model).addRow();
        }
    }

    /**
     * Delete a result row.
     *
     * @param table
     * @param model
     */
    private void deleteResultsRow(JTable table, TableModel model) {
    	if (model instanceof ResultSelectionModel) {
            ((ResultSelectionModel) model).delRow(table.getSelectedRow());
        }
    }

	/**
	 * Show a Message Dialog.
	 * @param title the title of the dialog
	 * @param msg the message
	 */
    private void showMessageDialog(String title, String msg) {
        JOptionPane.showMessageDialog(this.mainPanel.getMainTabbedPane(), msg, title, JOptionPane.INFORMATION_MESSAGE);
    }

	/**
	 * Updates the textFields with data from uiModel.
	 */
    private void updateTextFields() {

		//set uncertainty specification file textfield.
		String filename = this.uiModel.getUncertaintySpecificationFile() == null ? ""
				: this.uiModel.getUncertaintySpecificationFile()
						.getAbsolutePath();
		this.mainPanel.getUncertaintySpecificationFileTextField().setText(filename);

		//set monte carlo start and end textfields.
		this.mainPanel.getMonteCarloStartFormattedTextField().setText(
				String.valueOf(this.uiModel.getStartMonteCarloRun()));
		this.mainPanel.getMonteCarloEndFormattedTextField().setText(
				String.valueOf(this.uiModel.getEndMonteCarloRun()));
	}

	/**
	 * update the tables with data from uiModel.
	 */
    private void updateTables() {
   		this.pdfDefinitionTableController.updatePDFDefinitionTable(this.uiModel);
        this.variationPerParameterTableController.updateVariationPerParameterTable(this.uiModel);
        this.correlationTableController.updateCorrelationTable(this.uiModel);
        this.resultsTableController.updateResultsTable(this.uiModel);
    }

	/**
	 * update the GUI. Should be called after data in uiModel has changed.
	 */
    private void updateGUI() {
		updateTextFields();
		updateTables();
	}

	/**
	 * Set the model and update GUI.
	 *
	 * @param uiModel the user interface model
	 */
    private void setModel(UIModel uiModel) {
		this.uiModel = uiModel;
		updateGUI();
	}

	/**
	 * Get the model.
     * @return the UI Model
     */
	public UIModel getModel() {
		return this.uiModel;
	}

	public MainPanel getMainPanel() {
		return this.mainPanel;
	}

	public CorrelationTableController getCorrelationTableController() {
		return this.correlationTableController;
	}

	public PDFContext.GRAPH_TYPE getCurrentPDFGraphType() {
		return this.currentPDFGraphType;
	}

	public void setCurrentPDFGraphType(PDFContext.GRAPH_TYPE currentGraphType) {
		this.currentPDFGraphType = currentGraphType;
	}

	public String getApplicationPath() {
		return this.applicationPath;
	}

	public void setApplicationPath(String applicationPath) {
		this.applicationPath = applicationPath;
	}

}
