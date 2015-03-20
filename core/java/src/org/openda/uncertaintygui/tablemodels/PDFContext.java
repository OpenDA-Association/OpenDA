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

import java.util.ArrayList;

import org.openda.uncertainties.FunctionParameter;
import org.openda.uncertainties.pdfs.ChiSquareDistribution;
import org.openda.uncertainties.pdfs.GammaDistribution;
import org.openda.uncertainties.pdfs.LognormalDistribution;
import org.openda.uncertainties.pdfs.PDF;
import org.openda.uncertainties.pdfs.WeibullDistribution;
import due.utilities.matrix.DenseDoubleMatrix2D;

/**
 * This class has a pdf object which can be of
 * different types (subclasses) and some methods for
 * using the pdf object. This class uses the
 * strategy pattern.
 */
public class PDFContext {

	//enumeration to indicate the type of graph to plot.
	public enum GRAPH_TYPE {
	    PDF_MARGINAL, PDF_CUMULATIVE, PDF_INVERSE_CUMULATIVE
	}

	//to store a pdf object.
    private PDF pdf;

    /**
     * Constructor to make a new pdfContext object.
     * @param pdf to store in this context object.
     */
    public PDFContext(PDF pdf) {
      this.pdf = pdf;
    }

    /**
     * This method creates a clone of this pdfContext object.
     * @return clone a clone of this context object.
     */
    public PDFContext clone() {

    	PDF clonedPdf = this.getPdfObject().clone();

    	PDFContext pdfContext = new PDFContext(clonedPdf);

    	return pdfContext;
    }

    /**
     * Gets points for a graph of this.pdf distribution.
     * This method calls getDistribution and then either shifts the graph x values
     * or multiplies the graph x values by the basic value stored in
     * this.pdf.getUncertainItem(), if basic value is available.
     *
     * @param numberOfPoints the number of points in the returned DenseDoubleMatrix2D object.
     * @param graphType the type of graph to plot (marginal or cumulative)
     * @return DenseDoubleMatrix2D containing the points of the graph
     */
	public DenseDoubleMatrix2D getGraphPoints(int numberOfPoints, PDFContext.GRAPH_TYPE graphType) {
    	DenseDoubleMatrix2D graphPoints = getDistributionGraph(this.pdf, numberOfPoints, graphType);

        double basicValue = this.pdf.getUncertainItem().getBasicValue();

        if (Double.compare(basicValue, Double.NaN) != 0) {

        	if (this.pdf instanceof ChiSquareDistribution
        			|| this.pdf instanceof GammaDistribution
        			|| this.pdf instanceof LognormalDistribution
        			|| this.pdf instanceof WeibullDistribution) {
    			// multiply all x values by basic value.
    			for (int j = 0; j < numberOfPoints; j++) {
					double currentXValue = graphPoints.getElement(j, 0);
					graphPoints.setElement(j, 0, currentXValue*basicValue);
				}
        	}
        	else {
    			// shift graph by adding basic value to all x values.
    			for (int j = 0; j < numberOfPoints; j++) {
					double currentXValue = graphPoints.getElement(j, 0);
					graphPoints.setElement(j, 0, currentXValue + basicValue);
				}
        	}
		}

  		return graphPoints;
	}

    /**
     * Evaluates the distribution function on numberOfPoints points within
     * a certain range. These points are used to make a graph of the
     * graph type supplied (marginal probability distribution function or
     * cumulative probability distribution function).
     *
     *@param pdf
     *@param numberOfPoints the number of points in the returned DenseDoubleMatrix2D object.
     *@param graphType marginal or cumulative
     *@return DenseDoubleMatrix2D containing the points of the graph
     *@throws IllegalArgumentException if numberOfPoints <= 0
     */
    public DenseDoubleMatrix2D getDistributionGraph(PDF pdf, int numberOfPoints,
			PDFContext.GRAPH_TYPE graphType) throws IllegalArgumentException {

        if (numberOfPoints > 0) {
            double xLowerLimit = Double.MIN_VALUE;
            double xUpperLimit = Double.MAX_VALUE;

            switch (graphType) {
	            case PDF_MARGINAL:
	                xLowerLimit = pdf.getLowerLimit();
	                xUpperLimit = pdf.getUpperLimit();
	                break;
	            case PDF_CUMULATIVE:
	                xLowerLimit = pdf.getCumulativeLowerLimit();
	                xUpperLimit = pdf.getCumulativeUpperLimit();
	                break;
                default:
                    throw new IllegalArgumentException("Invalid graph type supplied.");
            }

            double step = (xUpperLimit - xLowerLimit) / numberOfPoints;

            DenseDoubleMatrix2D samples = new DenseDoubleMatrix2D(numberOfPoints, 2);
            for (int j = 0; j < numberOfPoints; j++) {
                samples.setElement(j, 0, xLowerLimit + (j * step));

                switch (graphType) {
	                case PDF_MARGINAL:
	                    samples.setElement(j, 1, pdf.getPDFPoint(xLowerLimit + (j * step)));
	                    break;
	                case PDF_CUMULATIVE:
	                    samples.setElement(j, 1, pdf.getCDFPoint(xLowerLimit + (j * step)));
	                    break;
	                default:
	                    throw new IllegalArgumentException("Invalid graph type supplied.");
                }
            }
            return samples;
        }
        throw new IllegalArgumentException("Invalid number of points supplied.");
    }

	/**
	 * Returns a string with the type of the pdf object.
	 * @return type of pdf function
	 */
    public String toString() {
		return this.pdf.toString();
	}

	/**
	 * Get this contexts pdf object.
	 * @return pdf
	 */
	public PDF getPdfObject() {
		return this.pdf;
	}

	/**
	 * Returns an arraylist with parameters of the pdf object.
	 * @return arraylist of parameters of pdf object.
	 */
	public ArrayList<FunctionParameter> getParams() {
		return this.pdf.getParams();
	}
}

