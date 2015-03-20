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

package org.openda.uncertainties;

import org.openda.uncertainties.autocorrelationfunctions.AutoCorrelationFunction;
import org.openda.uncertainties.pdfs.NormalDistribution;
import org.openda.uncertainties.pdfs.PDF;
import org.openda.uncertainties.variationfunctions.Variation;

import java.io.Serializable;
import java.util.ArrayList;


public class Uncertainties implements Serializable{

    public static final int PDF = 1;
    public static final int VARIATION_PER_PARAMETER = 2;

    private int uncertaintyType;
    //to store a list of PDF definitions.
	private ArrayList<PDF> pdfs;
    //to store a list of variationFunctions.
	private ArrayList<Variation> variationFunctions;
	//to store a list of auto correlation functions.
	private ArrayList<AutoCorrelationFunction> autoCorrelations;

	public Uncertainties() {
        this.uncertaintyType = PDF;//default to PDF.
        pdfs = new ArrayList<PDF>();
        variationFunctions = new ArrayList<Variation>();
        autoCorrelations = new ArrayList<AutoCorrelationFunction>();
    }

    /**
     * Add PDF to Arraylist<PDF>
     * @param pdf
     */
    public void addPdf(PDF pdf) {
        this.pdfs.add(pdf) ;
    }

    /**
     * Remove PDF from Arraylist<PDF>
     * @param index
     */
	public void removePdf(int index) {
        if (index < pdfCount()) {
        	this.pdfs.remove(index);
        }
	}

    /**
     * return Count of PDF
     * @return
     */
    public int pdfCount() {
        return this.pdfs.size();
    }

    /**
     * Returns number of pdfs that have an uncertainItem with isActive == true.
     * @return activeCount
     */
	public int activePdfCount() {
		int activeCount = 0;

		for (PDF pdf : this.pdfs) {
			if (pdf.getUncertainItem().isActive()) {
				activeCount++;
			}
		}

		return activeCount;
	}

    /**
     * Returns number of normal distributed pdfs that have an
     * uncertainItem with isActive == true.
     * @return activeNormalCount
     */
	public int activeNormalPdfCount() {
		int activeNormalCount = 0;

		for (PDF pdf : this.pdfs) {
			if (pdf.getUncertainItem().isActive() && (pdf instanceof NormalDistribution)) {
				activeNormalCount++;
			}
		}

		return activeNormalCount;
	}

    /**
     * Add variation function to Arraylist<Variation>
     * @param variationFunction
     */
     public void addVariationFunction(Variation variationFunction) {
        this.variationFunctions.add(variationFunction) ;
    }

    /**
     * Remove variation function from Arraylist<Variation>
     * @param index
     */
	public void removeVariationFunction(int index) {
        if (index < variationFunctionCount()) {
        	this.variationFunctions.remove(index);
        }
	}

    /**
     * return Count of Variation
     * @return
     */
    public int variationFunctionCount() {
        return this.variationFunctions.size();
    }

    /**
     * Add auto correlation to Arraylist<AutoCorrelationFunction>
     * @param autoCorrelationFunction
     */
    public void addAutoCorrelation(AutoCorrelationFunction autoCorrelationFunction) {
        this.autoCorrelations.add(autoCorrelationFunction) ;
    }

    /**
     * Remove auto correlation with specified index from Arraylist<AutoCorrelationFunction>
     * @param index of auto correlation in arraylist AutoCorrelations.
     */
	public void removeAutoCorrelationFunction(int index) {
        if (index < autoCorrelationFunctionCount()) {
        	this.autoCorrelations.remove(index);
        }
	}

    /**
     * Remove specified auto correlation object from arraylist AutoCorrelations.
     * @param autoCorrelationFunction the object to remove.
     */
	public void removeAutoCorrelationFunction(AutoCorrelationFunction autoCorrelationFunction) {
        this.autoCorrelations.remove(autoCorrelationFunction);
	}

    /**
     * return Count of autoCorrelations
     * @return
     */
    public int autoCorrelationFunctionCount() {
        return this.autoCorrelations.size();
    }

    /**
     * Get UncertaintyType
     * @return
     */
    public int getUncertaintyType() {
        return uncertaintyType;
    }

    /**
     * Set UncertaintyType
     * @param uncertaintyType
     */
    public void setUncertaintyType(int uncertaintyType) {
        this.uncertaintyType = uncertaintyType;
    }

    /**
     * return the count of Uncertainties as defined under uncertaintyType
     * @return
     */
    public int uncertaintyCount() {
        if (uncertaintyType==PDF) return pdfCount();
        if (uncertaintyType==VARIATION_PER_PARAMETER) return variationFunctionCount();
        return 0;
    }

    /**
     * return the the uncertainty as defined under uncertaintyType
     * @param index
     * @return
     */
    public Uncertainty getUncertainty(int index ) {
        if (uncertaintyType==PDF) return getPdf(index);
        if (uncertaintyType==VARIATION_PER_PARAMETER) return getVariationFunction(index);
        return null; // will never occur
    }

    /**
     * Returns the pdf with given index in this.pdfs arraylist.
     * @param index index in PDF list
     * @return pdf The required PDF
     */
    public PDF getPdf(int index) {
        if (index < pdfCount() ) return this.pdfs.get(index);
        return null;
    }

    /**
     * Returns the pdf with given index in this.pdfs arraylist.
     * @param id uncertainty Identifier
     * @return pdf The required PDF
     */
    public PDF getPdf(String id) {
        for (PDF pdf : this.pdfs) {
            if (pdf.getUncertainItem().getId().equals(id)) {
                return pdf;
            }
        }
        return null;
    }

    /**
     * This method counts each pdf that is active in this.pdfs arraylist and
     * if the count reaches the given index, then it returns the
     * corresponding pdf.
     * Returns null if index is < 0 or >= activePdfCount.
     *
     * @param index the number of the active pdf.
     * @return pdf
     */
    public PDF getActivePdf(int index) {
    	int activeCount = -1;

    	for (PDF pdf : this.pdfs) {
			if (pdf.getUncertainItem().isActive()) {
				activeCount++;
				if (activeCount == index) {
					return pdf;
				}
			}
        }

    	return null;
    }

    /**
     * This method counts each pdf that is active and is a normal
     * distribution in this.pdfs arraylist and
     * if the count reaches the given index, then it returns the
     * corresponding pdf.
     * Returns null if index is < 0 or >= activeNormalPdfCount.
     *
     * @param index the number of the active normal pdf.
     * @return pdf
     */
    public PDF getActiveNormalPdf(int index) {
    	int activeNormalCount = -1;

    	for (PDF pdf : this.pdfs) {
			if (pdf.getUncertainItem().isActive() && (pdf instanceof NormalDistribution)) {
				activeNormalCount++;
				if (activeNormalCount == index) {
					return pdf;
				}
			}
        }

    	return null;
    }

    /**
     * Returns the variationFunction with given index in this.variationFunctions arraylist.
     * @param index
     * @return variationFunction
     */
    public Variation getVariationFunction(int index) {
        if (index < variationFunctionCount() ) return this.variationFunctions.get(index);
        return null;
    }

    /**
     * Returns the autoCorrelationFunction with given index in this.autoCorrelations arraylist.
     * @param index
     * @return autoCorrelationFunction
     */
    public AutoCorrelationFunction getAutoCorrelationFunction(int index) {
        if (index < autoCorrelationFunctionCount() ) return this.autoCorrelations.get(index);
        return null;
    }

	/**
	 * Returns the autoCorrelationFunction with the given Id.
	 * Returns null if autoCorrelationFunction with given Id does not exist.
	 * @param id
	 * @return autoCorrelationFunction
	 */
	public AutoCorrelationFunction getAutoCorrelationFunction(String id) {
		for (AutoCorrelationFunction autoCorrelationFunction : this.autoCorrelations) {
			if (autoCorrelationFunction.getUncertainItem().getId().equals(id)) {
				return autoCorrelationFunction;
			}
		}
		return null;
	}

	public void setPDF(int index, PDF pdf) {
        if (index < pdfCount()) {
        	this.pdfs.set(index, pdf);
        }
	}

	public void setAutoCorrelationFunction(int index, AutoCorrelationFunction autoCorrelationFunction) {
        if (index < autoCorrelationFunctionCount()) {
        	this.autoCorrelations.set(index, autoCorrelationFunction);
        }
	}

	public void setVariationFunction(int index, Variation variationFunction) {
        if (index < variationFunctionCount()) {
        	this.variationFunctions.set(index, variationFunction);
        }
	}
}
