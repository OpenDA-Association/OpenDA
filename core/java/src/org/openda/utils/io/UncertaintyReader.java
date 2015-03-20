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

import nl.wldelft.da.tools.castor.v13.AutoCorrelationComplexType;
import nl.wldelft.da.tools.castor.v13.ProbabiltyDistributionFunctionComplexType;
import nl.wldelft.da.tools.castor.v13.UncertaintiesComplexType;
import nl.wldelft.da.tools.castor.v13.VariationPerParameterComplexType;
import nl.wldelft.da.tools.castor.v13.types.UncertaintyEnumStringType;
import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.ValidationException;
import org.xml.sax.InputSource;
import org.openda.uncertainties.Uncertainties;
import org.openda.uncertainties.UncertainItem;
import org.openda.uncertainties.pdfs.*;
import org.openda.uncertainties.autocorrelationfunctions.*;
import org.openda.uncertainties.variationfunctions.Variation;
import org.openda.uncertainties.variationfunctions.RangeVariation;
import org.openda.uncertainties.variationfunctions.PercentageVariation;

import java.io.File;
import java.io.IOException;
import java.io.Reader;


public class UncertaintyReader {

    private File uncertaintiesFile;
    private Reader uncertaintiesReader = null;

    //integer constants for uncertaintyTypes.

    //TODO check if not both PDF and VPP at same time. AK

    //to store read in data in an Uncertainties object. 
    private Uncertainties uncertainties = null;

    /**
     * Constructor to make an UncertaintyReader.
     *
     * @param uncertaintiesFile file containing uncertainties for reading
     * @throws IOException if given file does not exist
     */
    public UncertaintyReader(File uncertaintiesFile) throws IOException {
        if (uncertaintiesFile == null)
            throw new IllegalArgumentException("Argument File missing");
        if (!uncertaintiesFile.exists())
            throw new IOException("Mappings: " + " (file: " + uncertaintiesFile + ") does not exists");
        this.uncertaintiesFile = uncertaintiesFile;
        this.uncertaintiesReader = null;
        this.uncertainties = null;
    }

    public UncertaintyReader(Reader uncertaintiesReader) throws IOException {
        if (uncertaintiesReader == null)
            throw new IllegalArgumentException("Argument Reader missing");
        this.uncertaintiesFile = null;
        this.uncertaintiesReader = uncertaintiesReader;
        this.uncertainties = null;
    }

    /**
     * Reads in data and converts to an Uncertainties object.
     *
     * @param getActiveUncertaintiesOnly if false then all uncertainties (active and inactive) are read.
     * @return uncertainties object containing read in data.
     * @throws java.io.IOException XML file not found
     */
    public Uncertainties getUncertainties(boolean getActiveUncertaintiesOnly) throws IOException {
        // unMarshall the ua-uncertainties file
        UncertaintiesComplexType castorUncertainties = null;
        if (uncertaintiesFile != null) {
            castorUncertainties = (UncertaintiesComplexType) CastorUtils.parse(uncertaintiesFile, UncertaintiesComplexType.class);
        } else {
            castorUncertainties = (UncertaintiesComplexType) unMarshallString(uncertaintiesReader, UncertaintiesComplexType.class);
        }

        this.uncertainties = new Uncertainties();
        String uncertaintyID;
        String description;
        boolean active;

        int uncertaintyType = castorUncertainties.getUncertaintyType().getType();
        if (uncertaintyType == UncertaintyEnumStringType.PROBABILITYDISTRIBUTIONFUNCTION_TYPE)
            uncertaintyType = Uncertainties.PDF;
        else if (uncertaintyType == UncertaintyEnumStringType.VARIATIONPERPARAMETER_TYPE)
            uncertaintyType = Uncertainties.VARIATION_PER_PARAMETER;
        else
            throw new IllegalStateException("UncertaintyType " + castorUncertainties.getUncertaintyType().toString() + " not supported ");

        uncertainties.setUncertaintyType(uncertaintyType);

        if (uncertaintyType == Uncertainties.PDF) {
            if (castorUncertainties.getProbabilityDistributionFunctionCount() <= 0)
                throw new IOException(this.getClass().getSimpleName() + ": No PDFs defined in " + uncertaintiesFile);
        } else if (uncertaintyType == Uncertainties.VARIATION_PER_PARAMETER) {
            if (castorUncertainties.getVariationPerParameterCount() <= 0)
                throw new IOException(this.getClass().getSimpleName() + ": No VPPs defined in " + uncertaintiesFile);
        } else {
            throw new IOException(this.getClass().getSimpleName() + ": Unknowm Uncertainty Type " + uncertaintyType);
        }

        // PDFs
        int count = castorUncertainties.getProbabilityDistributionFunctionCount();
        for (int itemIndex = 0; itemIndex < count; itemIndex++) {
            ProbabiltyDistributionFunctionComplexType pdfComplexType = castorUncertainties.getProbabilityDistributionFunction(itemIndex);
            active = pdfComplexType.getIsActive();
            if (getActiveUncertaintiesOnly) {// then check if this is active
                if (!active) continue; // do not read
            }
            uncertaintyID = pdfComplexType.getId();
            description = uncertaintyID;
            UncertainItem uncertainItem = new UncertainItem(uncertaintyID, description, active);
            //get basic value if present.
            if (pdfComplexType.hasBasicValue()) {
            	uncertainItem.setBasicValue(pdfComplexType.getBasicValue());
            }

            //find out which PDF distribution type and put the
            //corresponding PDF in the variable pdf.
            //also set the isStdFactor variable in pdf.
            PDF pdf = readPdf(uncertainItem, pdfComplexType);

            this.uncertainties.addPdf(pdf);

            // see if any correlation defined
            // todo for stand alone Correlation
            // todo if Pdfs id and Correlation Id does not match
            if (castorUncertainties.getAutoCorrelationCount() > 0) {
                int correlationCount = castorUncertainties.getAutoCorrelationCount();
                for (int corrItems = 0; corrItems < correlationCount; corrItems++) {
                    AutoCorrelationComplexType correlationComplexType = castorUncertainties.getAutoCorrelation(corrItems);
                    String correlationId = correlationComplexType.getId();
                    if (correlationId.equals(uncertaintyID)) {
                        AutoCorrelationFunction autoCorrelationFunction = readAutoCorrelation(uncertainItem, correlationComplexType);
                        this.uncertainties.addAutoCorrelation(autoCorrelationFunction);
                        break;
                    }
                }
            }
        }

        // parameter variation
        count = castorUncertainties.getVariationPerParameterCount();
        for (int itemIndex = 0; itemIndex < count; itemIndex++) {
            VariationPerParameterComplexType parameterVariation = castorUncertainties.getVariationPerParameter(itemIndex);
            uncertaintyID = parameterVariation.getId();
            description = uncertaintyID;
            active = parameterVariation.getIsActive();
            if (getActiveUncertaintiesOnly) {// then check if this is active
                if (!active) continue; // do not read
            }
            UncertainItem uncertainItem = new UncertainItem(uncertaintyID, description, active);
            //get basic value if present.
            if (parameterVariation.hasBasicValue()) {
            	uncertainItem.setBasicValue(parameterVariation.getBasicValue());
            }

            //find out which variation type and put the
            //corresponding variation function in the variable variationFunction.
            Variation variationFunction = readVPP(uncertainItem, parameterVariation);

            //add variationFunction to uncertainties.
            this.uncertainties.addVariationFunction(variationFunction);
        }

        return uncertainties;
    }


    /**
     * read Variation per Parameters
     *
     * @param parameterVariation
     * @param uncertainItem
     * @return
     */

    private Variation readVPP(UncertainItem uncertainItem, VariationPerParameterComplexType parameterVariation) {
        Variation variationFunction;
        if (parameterVariation.getRange() != null) {
            float max = parameterVariation.getRange().getMax(); 
            float min = parameterVariation.getRange().getMin();
            variationFunction = new RangeVariation(min, max);

        } else if (parameterVariation.getPercent() != null) {
            float percentage = parameterVariation.getPercent().getPercentage();
            variationFunction = new PercentageVariation(percentage);

        } else {
            String parameterVariationErrorString = uncertainItem.getId() + " (file: " + uncertaintiesFile.getAbsolutePath() + ")";
            throw new RuntimeException("First Value, Second Value OR Percentage must be be set for " + parameterVariationErrorString);
        }
        variationFunction.setUncertainItem(uncertainItem);
        return variationFunction;
    }

    /**
     * read AutoCorrelation
     *
     * @param uncertainItem UncertainItem
     * @param choice AutoCorrelationComplexType
     * @return AutoCorrelationFunction
     */
    private AutoCorrelationFunction readAutoCorrelation(UncertainItem uncertainItem, AutoCorrelationComplexType choice) {

        AutoCorrelationFunction correlationFunction = null;
        if (choice.getCircular() != null) {
            float sill = choice.getCircular().getSill();
            float range = choice.getCircular().getRange();
            correlationFunction = new CircularCorrelation(sill, range);
        } else if (choice.getExponential() != null) {
            float sill = choice.getExponential().getSill();
            float range = choice.getExponential().getRange();
            correlationFunction = new ExponentialCorrelation(sill, range);
        } else if (choice.getGaussian() != null) {
            float sill = choice.getGaussian().getSill();
            float range = choice.getGaussian().getRange();
            correlationFunction = new GaussianCorrelation(sill, range);
        } else if (choice.getLinear() != null) {
            float sill = choice.getLinear().getSill();
            float range = choice.getLinear().getRange();
            correlationFunction = new LinearCorrelation(sill, range);
        } else if (choice.getNugget() != null) {
            float sill = choice.getNugget().getSill();
            correlationFunction = new NuggetCorrelation(sill);
        } else if (choice.getPentaSpherical() != null) {
            float sill = choice.getPentaSpherical().getSill();
            float range = choice.getPentaSpherical().getRange();
            correlationFunction = new PentasphericalCorrelation(sill, range);
        } else if (choice.getPeriodic() != null) {
            float sill = choice.getPeriodic().getSill();
            float range = choice.getPeriodic().getRange();
            correlationFunction = new PeriodicCorrelation(sill, range);
        } else if (choice.getSpherical() != null) {
            float sill = choice.getSpherical().getSill();
            float range = choice.getSpherical().getRange();
            correlationFunction = new SphericalCorrelation(sill, range);
        }
        correlationFunction.setUncertainItem(uncertainItem);
        return correlationFunction;


    }

    /**
     * read Probability Distribution Function
     *
     * @param uncertainItem
     * @param choice
     * @return
     */

    private PDF readPdf(UncertainItem uncertainItem, ProbabiltyDistributionFunctionComplexType choice) {
        PDF pdf;
        if (choice.getBeta() != null) {
            float alpha = choice.getBeta().getAlpha();
            float beta = choice.getBeta().getBeta();
            float max = choice.getBeta().getMax();
            float min = choice.getBeta().getMin();
            pdf = new BetaDistribution(alpha, beta, min, max);

        } else if (choice.getCauchy() != null) {
            float location = choice.getCauchy().getLocation();
            float scale = choice.getCauchy().getScale();
            pdf = new CauchyDistribution(location, scale);
            //pdf.setFactor(monteCarlo.getCauchy().getStdvIsFactor());

        } else if (choice.getChisquare() != null) {
            float freedom = choice.getChisquare().getDegreeOfFreedom();
            pdf = new ChiSquareDistribution(freedom);

        } else if (choice.getExponential() != null) {
            float location = choice.getExponential().getLocation();
            float rate = choice.getExponential().getRate();
            pdf = new ExponentialDistribution(location, rate);

        } else if (choice.getGamma() != null) {
            float shape = choice.getGamma().getShape();
            float scale = choice.getGamma().getScale();
            pdf = new GammaDistribution(shape, scale);
        } else if (choice.getGumbelmaximum() != null) {
            float location = choice.getGumbelmaximum().getLocation();
            float scale = choice.getGumbelmaximum().getScale();
            //TODO: added dummy mean. AK
            pdf = new GumbelMaximumDistribution(location, scale);

        } else if (choice.getGumbelminimum() != null) {
            float location = choice.getGumbelminimum().getLocation();
            float scale = choice.getGumbelminimum().getScale();

            //TODO: added dummy mean. AK
            pdf = new GumbelMinimumDistribution(location, scale);

        } else if (choice.getLognormal() != null) {
            float stdv = choice.getLognormal().getStdv();
            //TODO: added dummy mean. AK
            pdf = new LognormalDistribution(0.0, stdv);

        } else if (choice.getNormal() != null) {
            float mean = choice.getNormal().getMean();
            float stdv = choice.getNormal().getStdv();
            int factor = choice.getNormal().getStdvIsFactor() ? 1 : 0 ;
            pdf = new NormalDistribution(mean, stdv, factor);

        } else if (choice.getTriangular() != null) {
            float max = choice.getTriangular().getMax();
            float min = choice.getTriangular().getMin();
            float mode = choice.getTriangular().getMode();
            pdf = new TriangularDistribution(min, mode, max);

        } else if (choice.getUniform() != null) {
            float max = choice.getUniform().getMax();
            float min = choice.getUniform().getMin();

            pdf = new UniformDistribution(min, max);

        } else if (choice.getWeibull() != null) {
            float scale = choice.getWeibull().getScale();
            float shape = choice.getWeibull().getShape();

            pdf = new WeibullDistribution(shape, scale);
        } else {
            String errorString = uncertainItem.getId() + " (file: " + uncertaintiesFile.getAbsolutePath() + ")";
            throw new RuntimeException("No PDF type specified for " + errorString);
        }
        pdf.setUncertainItem(uncertainItem);
        return pdf;
    }

    /**
     * Does nothing yet.
     */
    public void close() {

    }

    public static Object unMarshallString(Reader reader, Class objectClass) throws IOException {
        Unmarshaller unmarshaller = new Unmarshaller(objectClass);
        Object castor;
        try {
            InputSource is = new InputSource(reader);
            is.setSystemId("stringBuffer");
            castor = unmarshaller.unmarshal(is);
            reader.close();
        } catch (MarshalException marshalException) {
            //TODO Juzer throw proper exception so that calling function can handle it
            throw new IOException(marshalException.getMessage());
        } catch (ValidationException validationException) {
            throw new IOException(validationException.getMessage());
        }
        return castor;
    }

}
