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

package org.openda.uncertaintygui.tablemodels;

import org.openda.interfaces.IPrevExchangeItem;
import org.openda.uncertainties.UncertainItem;
import org.openda.uncertainties.Uncertainties;
import org.openda.uncertainties.autocorrelationfunctions.AutoCorrelationFunction;
import org.openda.uncertainties.autocorrelationfunctions.GaussianCorrelation;
import org.openda.uncertainties.pdfs.NormalDistribution;
import org.openda.uncertainties.pdfs.PDF;
import org.openda.uncertainties.variationfunctions.RangeVariation;
import org.openda.uncertainties.variationfunctions.Variation;
import org.openda.uncertaintygui.UncertaintyGuiConfiguration;
import org.openda.utils.io.UncertaintyReader;

import java.io.File;
import java.io.IOException;
import java.io.Reader;

/**
 * Main UI Model which loads the necessary XML files and stores the data.
 * This class uses UaToolsGuiConfiguration to store model configuration data
 * and contains methods used for reading uncertainties and result selections
 * data from files.
 */
public class UIModel  {

	//constants used by UaResultSelectionModel to indicate if result selections are active.
	public static final String ACTIVE = "active";
    public static final String NONACTIVE = "non-active";

    //reference to object containing model configuration data.
	private UncertaintyGuiConfiguration uncertaintyGuiConfiguration;


    /**
     * Constructor for "empty" UAtools UI Model.
     */
    public UIModel() {
    	this.uncertaintyGuiConfiguration = new UncertaintyGuiConfiguration();//empty.
    }

    /**
     * Read uncertainty mapping and uncertainties specification file and stores
     * uncertainties in this.uncertainties object.
     *
     * @param uncertaintiesSpecificationFile File containing the uncertainty specification
     * @param reader If not equal to null, there is already an opened file reader
     * @throws IOException
     */
    public void readUncertaintyFiles(File uncertaintiesSpecificationFile, Reader reader) throws IOException {

        Uncertainties uncertainties = new Uncertainties();
        if (uncertaintiesSpecificationFile != null || reader != null) {
            // read uncertainties
            this.uncertaintyGuiConfiguration.setUncertaintiesSpecificationFile(uncertaintiesSpecificationFile);
            UncertaintyReader uncertaintyReader;
            if (reader == null) {
                uncertaintyReader = new UncertaintyReader(uncertaintiesSpecificationFile);
            } else {
                uncertaintyReader = new UncertaintyReader(reader);
            }
            boolean getActiveUncertaintiesOnly = false;
            uncertainties = uncertaintyReader.getUncertainties(getActiveUncertaintiesOnly);
            uncertaintyReader.close();
        }

        //fill up other data depending on uncertainty type.
        if (uncertainties.pdfCount() > 0) {
        	if (uncertainties.variationFunctionCount() <= 0) {
            	createDefaultVariationFunctions(uncertainties);
        	}
        }
        else if (uncertainties.variationFunctionCount() > 0) {
        	if (uncertainties.pdfCount() <= 0) {
                createDefaultPdfs(uncertainties);
        	}
        }

        this.uncertaintyGuiConfiguration.setUncertaintiesObject(uncertainties);
    }



    /**
     * Creates default pdfs for all the uncertainItems that are present
     * in the variation functions in the given uncertainties object.
	 * The pdfs have uncertainItem objects separate from the
	 * variationFunctions.
	 *
     * @param uncertainties object contains read in uncertainties data
	 */
    public void createDefaultPdfs(Uncertainties uncertainties) {
		for (int n = 0; n < uncertainties.variationFunctionCount(); n++) {
			Variation variationFunction = uncertainties.getVariationFunction(n);

        	//create new UncertainItem.
            UncertainItem uncertainItem = variationFunction.getUncertainItem().clone();

	    	//create new default pdf.
	        PDF pdf = new NormalDistribution();
	        pdf.setUncertainItem(uncertainItem);
	        uncertainties.addPdf(pdf);
		}
    }

    /**
     * Creates default variation functions
     * for all the uncertainItems present in the pdfs in the given uncertainties object.
	 * The VariationFunctions have their own separate uncertainItem objects.
	 *
     * @param uncertainties object contains read in uncertainties data
	 */
    public void createDefaultVariationFunctions(Uncertainties uncertainties) {
		for (int n = 0; n < uncertainties.pdfCount(); n++) {
			PDF pdf = uncertainties.getPdf(n);

			//create new UncertainItem.
		    UncertainItem uncertainItem = pdf.getUncertainItem().clone();

		    //create new default VariationFunction.
			Variation variationFunction = new RangeVariation();
			variationFunction.setUncertainItem(uncertainItem);
		    uncertainties.addVariationFunction(variationFunction);
		}
    }

    private void addNewVariationUncertaintyFromMapping(Uncertainties uncertainties, String uncertaintyMappingId, String description, double basicValue) {
        //create new UncertainItem for uncertainty from mapping.
        UncertainItem uncertainItem = new UncertainItem(uncertaintyMappingId, "", false);

        //get description.
        if (description != null) {
            uncertainItem.setDescription(description);
        }

        // get basic value from mapping file.
        uncertainItem.setBasicValue(basicValue);

        //create new VariationFunction for uncertainty from mapping.
        Variation variationFunction = new RangeVariation();
        variationFunction.setUncertainItem(uncertainItem);
        uncertainties.addVariationFunction(variationFunction);
    }

    private void addNewPdfFromExchangeItem(Uncertainties uncertainties, IPrevExchangeItem exchangeItem) {

        //create new UncertainItem for uncertainty from mapping.
        UncertainItem uncertainItem = new UncertainItem(exchangeItem.getId(), "", false);

        //get description.
        if (exchangeItem.getDescription() != null) {
            uncertainItem.setDescription(exchangeItem.getDescription());
        }

        // get basic value from mapping file.
        double[] valuesAsDoubles = exchangeItem.getValuesAsDoubles();
        double value = Double.NaN;
        if (valuesAsDoubles.length == 1) {
            value = valuesAsDoubles[0];
        }
        uncertainItem.setBasicValue(value);

        //create new PDF for uncertainty from mapping.
        PDF pdf = new NormalDistribution();
        pdf.setUncertainItem(uncertainItem);
        uncertainties.addPdf(pdf);

        //create new AutoCorrelationFunction for uncertainty from mapping.
        AutoCorrelationFunction autoCorrelationFunction = new GaussianCorrelation();
        autoCorrelationFunction.setUncertainItem(uncertainItem);
        uncertainties.addAutoCorrelation(autoCorrelationFunction);
    }

    public File getUncertaintySpecificationFile() {
        return this.uncertaintyGuiConfiguration.getUncertaintiesSpecificationFile();
    }

    public int getEndMonteCarloRun() {
        return this.uncertaintyGuiConfiguration.getEndMonteCarloRun();
    }

    public int getStartMonteCarloRun() {
        return this.uncertaintyGuiConfiguration.getStartMonteCarloRun();
    }

	public Uncertainties getUncertaintiesObject() {
		return this.uncertaintyGuiConfiguration.getUncertaintiesObject();
	}

	public void setUncertaintiesObject(Uncertainties uncertaintiesObject) {
		this.uncertaintyGuiConfiguration.setUncertaintiesObject(uncertaintiesObject);
	}

	public UncertaintyGuiConfiguration getUaToolsGuiConfiguration() {
		return this.uncertaintyGuiConfiguration;
	}
}
