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

package org.openda.utils.io;

import nl.wldelft.da.tools.castor.v13.*;
import nl.wldelft.da.tools.castor.v13.types.UncertaintyEnumStringType;
import org.openda.uncertainties.Uncertainties;
import org.openda.uncertainties.autocorrelationfunctions.*;
import org.openda.uncertainties.pdfs.*;
import org.openda.uncertainties.variationfunctions.PercentageVariation;
import org.openda.uncertainties.variationfunctions.RangeVariation;
import org.openda.uncertainties.variationfunctions.Variation;
import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.ValidationException;

import java.io.*;

public class UncertaintyWriter {

    private File file;
    private Writer writer;
    private UncertaintiesComplexType uncertaintiesComplexType;
    //private PropertyVariables variables;

    public UncertaintyWriter(File uncertaintiesFile) throws IOException {
        if (uncertaintiesFile == null)
            throw new IllegalArgumentException("Argument File missing");
        if (uncertaintiesFile.exists()) uncertaintiesFile.delete() ; // remove old file
/*
        if (PropertyVariables.isInitialized()) {
            variables = PropertyVariables.getInstance();
        }
        else {
            variables = null;
        }
*/

        this.file = uncertaintiesFile;
        try {
            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(this.file), "UTF-8"));
            this.uncertaintiesComplexType = new UncertaintiesComplexType();
        } catch (Exception e) {
            throw new IOException("Writing Data " + e.getMessage() + " (file: " + uncertaintiesFile + ")");
        }
    }

    public UncertaintyWriter(Writer uncertaintiesWriter) throws IOException {
        if (uncertaintiesWriter == null)
            throw new IllegalArgumentException("Argument Writer missing");
 /*       if (PropertyVariables.isInitialized()) {
            variables = PropertyVariables.getInstance();
        } else {
            variables = null;
        }
*/
        this.file = null;
        this.writer = uncertaintiesWriter;
        this.uncertaintiesComplexType = new UncertaintiesComplexType();
    }

    /**
     * Convert data from uncertainties object to this.uncertaintiesComplexType
     * object and marshall this object (write data to file).
     *
     * @param uncertainties object with data to write.
     */
    public void write(Uncertainties uncertainties) {
        write(uncertainties, false);
    }

    /**
     * Convert data from uncertainties object to this.uncertaintiesComplexType
     * object and marshall this object (write data to file).
     *
     * @param uncertainties object with data to write.
     * @param writeSelectedTypeOnly Write only the Pfd or the Var. per pars, or write both groups
     */
    public void write(Uncertainties uncertainties, boolean writeSelectedTypeOnly) {

        boolean writePdfs = true;
        boolean writeVarPerPar = true;
    	if (uncertainties.getUncertaintyType() == Uncertainties.PDF) {
           	this.uncertaintiesComplexType.setUncertaintyType(UncertaintyEnumStringType.PROBABILITYDISTRIBUTIONFUNCTION);
            if (writeSelectedTypeOnly) {
                writeVarPerPar = false;
            }
    	} else if (uncertainties.getUncertaintyType() == Uncertainties.VARIATION_PER_PARAMETER) {
         	this.uncertaintiesComplexType.setUncertaintyType(UncertaintyEnumStringType.VARIATIONPERPARAMETER);
            if (writeSelectedTypeOnly) {
                writePdfs = false;
            }
    	} else {
    		//undefined uncertainty type.
    		throw new RuntimeException("Writing Data file " + file.getName() + " error: undefined uncertainty type in program.");
    	}

        if (writePdfs) {
            // for all PDFs in uncertainties create and add
            // a corresponding complexType to uncertaintiesComplexType.
            int count = uncertainties.pdfCount();
            for (int iUncertainty = 0; iUncertainty < count; iUncertainty++) {
                PDF pdf = uncertainties.getPdf(iUncertainty);
                this.uncertaintiesComplexType
                        .addProbabilityDistributionFunction(writePdf(pdf));
            }// end for

            // for all AutoCorrelation create and add
            // a corresponding complexType to uncertaintiesComplexType.
            count = uncertainties.autoCorrelationFunctionCount();
            for (int iUncertainty = 0; iUncertainty < count; iUncertainty++) {
                AutoCorrelationFunction correlationFunction = uncertainties
                        .getAutoCorrelationFunction(iUncertainty);
                this.uncertaintiesComplexType
                        .addAutoCorrelation(writeAutoCorrelation(correlationFunction));
            }// end for
        }

        if (writeVarPerPar) {
            // for all VaiationFunctions in uncertainties create and add
            // a corresponding complexType to uncertaintiesComplexType.
            int count = uncertainties.variationFunctionCount();
            for (int iUncertainty = 0; iUncertainty < count; iUncertainty++) {
                Variation variationFunction = uncertainties
                        .getVariationFunction(iUncertainty);
                this.uncertaintiesComplexType
                        .addVariationPerParameter(writeVariation(variationFunction));
            }// end for
        }

        // write data (by marshalling this.uncertaintiesComplexType).
        try {
            this.write();
        } catch (Exception e) {
            throw new RuntimeException("Writing Data file " + file.getName() + " error:" + e.getMessage());
        }
    }

    private AutoCorrelationComplexType writeAutoCorrelation(AutoCorrelationFunction correlationFunction) {
        AutoCorrelationComplexType complexType = new AutoCorrelationComplexType();
        complexType.setId(correlationFunction.getUncertainItem().getId());
        complexType.setIsActive(correlationFunction.getUncertainItem().isActive());

        if (correlationFunction instanceof CircularCorrelation) {
            CorrelationCircularComplexType correlationComplexType = new CorrelationCircularComplexType();
            float sill = (float) correlationFunction.getSill();
            float range = (float) correlationFunction.getRange();
            correlationComplexType.setRange(range);
            correlationComplexType.setSill(sill);
            complexType.setCircular(correlationComplexType);

        } else if (correlationFunction instanceof ExponentialCorrelation) {
            CorrelationExponentialComplexType correlationComplexType = new CorrelationExponentialComplexType();
            float sill = (float) correlationFunction.getSill();
            float range = (float) correlationFunction.getRange();
            correlationComplexType.setRange(range);
            correlationComplexType.setSill(sill);
            complexType.setExponential(correlationComplexType);

        } else if (correlationFunction instanceof GaussianCorrelation) {
            CorrelationGaussianComplexType correlationComplexType = new CorrelationGaussianComplexType();
            float sill = (float) correlationFunction.getSill();
            float range = (float) correlationFunction.getRange();
            correlationComplexType.setRange(range);
            correlationComplexType.setSill(sill);
            complexType.setGaussian(correlationComplexType);

        } else if (correlationFunction instanceof LinearCorrelation) {

            CorrelationLinearComplexType correlationComplexType = new CorrelationLinearComplexType();
            float sill = (float) correlationFunction.getSill();
            float range = (float) correlationFunction.getRange();
            correlationComplexType.setRange(range);
            correlationComplexType.setSill(sill);
            complexType.setLinear(correlationComplexType);

        } else if (correlationFunction instanceof NuggetCorrelation) {

            CorrelationNuggetComplexType correlationComplexType = new CorrelationNuggetComplexType();
            float sill = (float) correlationFunction.getSill();
            correlationComplexType.setSill(sill);
            complexType.setNugget(correlationComplexType);

        } else if (correlationFunction instanceof PentasphericalCorrelation) {

            CorrelationPentasphericalComplexType correlationComplexType = new CorrelationPentasphericalComplexType();
            float sill = (float) correlationFunction.getSill();
            float range = (float) correlationFunction.getRange();
            correlationComplexType.setRange(range);
            correlationComplexType.setSill(sill);
            complexType.setPentaSpherical(correlationComplexType);

        } else if (correlationFunction instanceof PeriodicCorrelation) {
            CorrelationPeriodicComplexType correlationComplexType = new CorrelationPeriodicComplexType();
            float sill = (float) correlationFunction.getSill();
            float range = (float) correlationFunction.getRange();
            correlationComplexType.setRange(range);
            correlationComplexType.setSill(sill);
            complexType.setPeriodic(correlationComplexType);

        } else if (correlationFunction instanceof SphericalCorrelation) {

            CorrelationSphericalComplexType correlationComplexType = new CorrelationSphericalComplexType();
            float sill = (float) correlationFunction.getSill();
            float range = (float) correlationFunction.getRange();
            correlationComplexType.setRange(range);
            correlationComplexType.setSill(sill);
            complexType.setSpherical(correlationComplexType);
        }
        return complexType;
    }

    private VariationPerParameterComplexType writeVariation(Variation variationFunction) {
        VariationPerParameterComplexType complexType = new VariationPerParameterComplexType();
        complexType.setId(variationFunction.getUncertainItem().getId());
        complexType.setIsActive(variationFunction.getUncertainItem().isActive());
        //write basic value only when a basic value is specified.
        //if basic value not specified, then do not write basic value.
        if (Double.compare(variationFunction.getUncertainItem().getBasicValue(), Double.NaN) != 0) {
            complexType.setBasicValue((float) variationFunction.getUncertainItem().getBasicValue());
        }

        //write variation function.
        if (variationFunction instanceof RangeVariation) {
            RangeVariation range = (RangeVariation)variationFunction;
            VariationRangeComplexType rangeComplexType = new VariationRangeComplexType ();
            rangeComplexType.setMin((float) range.getLowerLimit());
            rangeComplexType.setMax((float) range.getUpperLimit());
            complexType.setRange(rangeComplexType);

        } else if (variationFunction instanceof PercentageVariation) {
            PercentageVariation percentage = (PercentageVariation)variationFunction;

            VariationPercentComplexType percentComplexType = new VariationPercentComplexType();
            percentComplexType.setPercentage((float) percentage.getPercentage());
            complexType.setPercent(percentComplexType);
        }
        return complexType;
    }

    private ProbabiltyDistributionFunctionComplexType writePdf(PDF pdf) {
        ProbabiltyDistributionFunctionComplexType complexType = new ProbabiltyDistributionFunctionComplexType();
        complexType.setId(pdf.getUncertainItem().getId());
        complexType.setIsActive(pdf.getUncertainItem().isActive());
        //write basic value only when a basic value is specified.
        //if basic value not specified, then do not write basic value.
        if (Double.compare(pdf.getUncertainItem().getBasicValue(), Double.NaN) != 0) {
            complexType.setBasicValue((float) pdf.getUncertainItem().getBasicValue());
        }

        //write pdf.
        if(pdf instanceof BetaDistribution) {

            PdfBetaComplexType betaComplexType = new PdfBetaComplexType();
            BetaDistribution distribution = (BetaDistribution)pdf;
            betaComplexType.setAlpha((float)distribution.getAlpha());
            betaComplexType.setBeta((float)distribution.getBeta());
            betaComplexType.setMin((float)distribution.getMin());
            betaComplexType.setMax((float)distribution.getMax());
            complexType.setBeta(betaComplexType);

        } else if (pdf instanceof CauchyDistribution) {

              PdfCauchyComplexType cauchyComplexType = new PdfCauchyComplexType();
            CauchyDistribution distribution = (CauchyDistribution)pdf;
            cauchyComplexType.setLocation((float) distribution.getLocation());
            cauchyComplexType.setScale((float) distribution.getScale());
            complexType.setCauchy(cauchyComplexType);

        } else if (pdf instanceof ChiSquareDistribution) {

            PdfChiSquareComplexType chiSquareComplexType = new PdfChiSquareComplexType();
            ChiSquareDistribution distribution = (ChiSquareDistribution)pdf;
            chiSquareComplexType.setDegreeOfFreedom((float)distribution.getShape());
            complexType.setChisquare(chiSquareComplexType);

        } else if (pdf instanceof ExponentialDistribution) {

            PdfExponentialComplexType exponentialComplexType = new PdfExponentialComplexType();
            ExponentialDistribution distribution = (ExponentialDistribution)pdf;
            exponentialComplexType.setLocation((float)distribution.getLocation());
            exponentialComplexType.setRate((float)distribution.getRate());
            complexType.setExponential(exponentialComplexType);

        } else if (pdf instanceof GammaDistribution ) {

            PdfGammaComplexType gammaComplexType = new PdfGammaComplexType();
            GammaDistribution distribution = (GammaDistribution)pdf;
            gammaComplexType.setShape((float)distribution.getShape());
            gammaComplexType.setScale((float)distribution.getScale());
            complexType.setGamma(gammaComplexType);

        } else if (pdf instanceof GumbelMaximumDistribution ) {

             PdfGumbelMaximumComplexType gumbelMaximumComplexType = new PdfGumbelMaximumComplexType();
             GumbelMaximumDistribution distribution = (GumbelMaximumDistribution)pdf;
            gumbelMaximumComplexType.setLocation((float) distribution.getLocation());
            gumbelMaximumComplexType.setScale((float) distribution.getScale());
            complexType.setGumbelmaximum(gumbelMaximumComplexType);

        } else if (pdf instanceof GumbelMinimumDistribution ) {

            PdfGumbelMinimumComplexType gumbelMinimumComplexType = new PdfGumbelMinimumComplexType();
            GumbelMinimumDistribution distribution = (GumbelMinimumDistribution)pdf;
            gumbelMinimumComplexType.setLocation((float) distribution.getLocation());
            gumbelMinimumComplexType.setScale((float) distribution.getScale());
            complexType.setGumbelminimum(gumbelMinimumComplexType);

        } else if (pdf instanceof LognormalDistribution ) {

            PdfLognormalComplexType lognormalComplexType = new PdfLognormalComplexType();
            LognormalDistribution distribution = (LognormalDistribution)pdf;
            lognormalComplexType.setMean((float) distribution.getMean());
            lognormalComplexType.setStdv((float) distribution.getStd());
            complexType.setLognormal(lognormalComplexType);

        } else if (pdf instanceof NormalDistribution ) {

            PdfNormalComplexType normalComplexType = new PdfNormalComplexType();
            NormalDistribution distribution = (NormalDistribution)pdf;
            normalComplexType.setMean((float) distribution.getMean());
            normalComplexType.setStdv((float) distribution.getStd());
            normalComplexType.setStdvIsFactor(distribution.isStdFactor());
            complexType.setNormal(normalComplexType);

        } else if (pdf instanceof TriangularDistribution) {

            PdfTriangularComplexType triangularComplexType = new PdfTriangularComplexType();
            TriangularDistribution distribution = (TriangularDistribution)pdf;
            triangularComplexType.setMin((float)distribution.getXMin());
            triangularComplexType.setMax((float)distribution.getXMax());
            triangularComplexType.setMode((float)distribution.getMode());
            complexType.setTriangular(triangularComplexType);

        } else if (pdf instanceof UniformDistribution) {

            PdfUniformComplexType uniformComplexType  = new PdfUniformComplexType();
            UniformDistribution distribution = (UniformDistribution)pdf;
            uniformComplexType.setMax((float) distribution.getXMax());
            uniformComplexType.setMin((float) distribution.getXMin());
            complexType.setUniform(uniformComplexType);

        } else if (pdf instanceof WeibullDistribution) {

            PdfWeibullComplexType weibullComplexType = new PdfWeibullComplexType();
            WeibullDistribution distribution = (WeibullDistribution)pdf;
            weibullComplexType.setShape((float)distribution.getShape());
            weibullComplexType.setScale((float)distribution.getScale());
            complexType.setWeibull(weibullComplexType);
        }
        return complexType;
    }

    public void close() throws IOException {
        try {
            this.writer.close();
        } catch (IOException e) {
            throw new IOException("Closing Data file " + file.getName() + " error:" + e.getMessage());
        }
    }

    // private functions
    private void write() throws IOException, ValidationException, MarshalException {

        String rootElement = "uncertainties" ;
        Marshaller marshaller = new Marshaller(writer);
        marshaller.setRootElement(rootElement);

        //if (variables != null && variables.containsKey("UA_SCHEMALOCATION"))
        //    marshaller.setSchemaLocation("http://www.wldelft.nl " + variables.getValue("UA_SCHEMALOCATION") + rootElement + ".xsd");
        //else
            marshaller.setSchemaLocation("http://www.wldelft.nl http://datools.wldelft.nl/schemas/v1.3/" + rootElement + ".xsd");

        marshaller.marshal(this.uncertaintiesComplexType);
    }
}
