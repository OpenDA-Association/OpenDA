/* OpenDA v2.4 
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
package org.openda.uncertainties;

import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;
import org.openda.uncertainties.autocorrelationfunctions.AutoCorrelationFunction;
import org.openda.uncertainties.pdfs.*;
import org.openda.uncertainties.variationfunctions.Variation;
import org.openda.utils.TreeVector;
import org.openda.utils.geometry.GeometryUtils;
import org.openda.utils.io.UncertaintyReader;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * User: Juzer Dhondia
 * Date: 15-nov-2007
 * Time: 13:11:25
 */
public class UncertaintyEngine implements Serializable{

    private static final int MONTE_CARLO = 1;
    private static final int VARIATION_PER_PARAMETER = 2;
    private static final int UNKNOWN = -1;

    public boolean useRandomSeed;
    private int timeStepSeed;
    private int realizationCounter;
    private int uncertaintyType;
    private int currentUncertaintyIndex;
    private int numMonteCarloRuns;
    private Uncertainties uncertainties;

    private LinkedHashMap<String, String> uncertaintyRegexMapping = null;
    private boolean definingRexegMapping = false;
    private boolean hasRexegMappings = false;

    /**
     * Uncertainty Engine Constructor
     */
    public UncertaintyEngine() {
        this.useRandomSeed = true;
        this.timeStepSeed = 1;
        this.realizationCounter = 0;
        this.uncertaintyType = UNKNOWN;
        this.currentUncertaintyIndex = 0;
        this.numMonteCarloRuns = 0;
        this.uncertainties = null;
    }

    /**
     * initialize the uncertainty Engine module
     * @param workingDir Working directory for the uncertainty engine
     * @param args Initialization arguments
     */
    public void initialize(File workingDir, String[] args) {

        if (args.length != 1 && args.length != 2) {
            throw new IllegalArgumentException(this.getClass() +
					": #args. must be 1 or 2 (uncert. file name [ useFixedSeed/useRandomSeed ])");
        }

        String uncertaintiesFile = new File(workingDir, args[0]).getAbsolutePath();
        try {
            readUncertainties(uncertaintiesFile);
        } catch (IOException e) {
            throw new RuntimeException(this.getClass() +  ": Reading uncertainties: " + e.getMessage() + " (file: " + uncertaintiesFile + ")");
        }

		if (args.length == 2)
		{
			if (args[1].equalsIgnoreCase("useFixedSeed")) {
				useRandomSeed = false;
			} else if (args[1].equalsIgnoreCase("useRandomSeed")) {
				useRandomSeed = true;
			} else {
				throw new IllegalArgumentException(this.getClass() +
						": If present, argument 2 must be useFixedSeed or useRandomSeed");
			}
		}
    }

	/**
     * Indicate that the using code (stoch model or stoch observer) will start to check which items are
     * uncertain. Calling this method is required if the uncertainty engine instance is supposed to support
     * the usages of regular expressions in the specification of the uncertain items.
     */
    public void startCheckingUncertainItems() {
        uncertaintyRegexMapping = new LinkedHashMap<String, String>();
        definingRexegMapping = true;
    }

    /**
     * Check if an item is defined as uncertain, eather direct, or by means of an uncertainty specification
     * that contains a regular expression. Note: before the first call to this method, the
     * startCheckingUncertainItems() method must have been called, and after the last call to checkIfItemIsUncertain()
     * the endCheckingUncertainItems() method must be called.
     * @param itemId The item to be checked
     * @return True if the item is defined as uncertain.
     */
    public boolean checkIfItemIsUncertain(String itemId) {
        for (int i = 0; i < this.uncertainties.uncertaintyCount(); i++) {
            if (itemId.equalsIgnoreCase(uncertainties.getUncertainty(i).getUncertainItem().getId())) {
                if (definingRexegMapping && !uncertaintyRegexMapping.containsKey(itemId)) {
                    uncertaintyRegexMapping.put(itemId, itemId);
                }
                return true;
            }
        }
        if (definingRexegMapping) {
            // ID not found. Check if it is defined as reg.expr.
            for (int i = 0; i < this.uncertainties.uncertaintyCount(); i++) {
                String uncertaintyId = uncertainties.getUncertainty(i).getUncertainItem().getId();
                Pattern regExPattern = Pattern.compile(uncertaintyId);
                Matcher patternMatcher = regExPattern.matcher(itemId);
                if (patternMatcher.matches()) {
                    if (uncertaintyRegexMapping.containsKey(itemId)) {
                        throw new RuntimeException("Item \""+ itemId + "\" found in two regular expressions");
                    }
                    uncertaintyRegexMapping.put(itemId, uncertaintyId);
                    hasRexegMappings = true;
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Indicate that the using code (stoch model or stoch observer) is done with checking which items are
     * uncertain. Calling this method is required if the uncertainty engine instance is supposed to support
     * the usages of regular expressions in the specification of the uncertain items.
     */
    public void endCheckingUncertainItems() {
        definingRexegMapping = false;
    }

    public PDF getPdf(String uncertaintyId) {
        return uncertainties.getPdf(determineActualUncertaintyId(uncertaintyId));
    }

    /**
     * evaluate PDFs
     *
     * @param uncertaintyID Identifier of the uncertain item
     * @param inputValue Value for which to evaluate the pdf
     * @return The evaluated PFD-value, given the input value
     */
    public double evaluatePDF(String uncertaintyID, double inputValue) {
        throw new RuntimeException(this.getClass() + ": evaluatePDF Function Not implemented");
    }

    /**
     * get Realizations for a single Uncertain Parameter for a given ensemble en given basicvalues at all locations/points
     * @param uncertaintyId Uncertain Parameter ID
     * @param ensembleIndex ensemble Index
     * @param values all basicValues
     * @return double[]
     */
    public double[] getRealization(String uncertaintyId, int ensembleIndex, double[] values) {
        if (values == null) {
            throw new IllegalArgumentException(this.getClass() + ": getRealization: values == null");
        }

        String actualUncertaintyId = determineActualUncertaintyId(uncertaintyId);
        int uncertaintyIndex = findUncertaintyIndex(actualUncertaintyId);

        int numValues = values.length;
        double[] realization = null;
        boolean isIdFound = false;

        if (uncertaintyType == VARIATION_PER_PARAMETER) {
            //find variationFunction object with id actualUncertaintyId.
            realization = new double[numValues];
            for (int i = 0; i < this.uncertainties.variationFunctionCount(); i++) {
                Variation function = this.uncertainties.getVariationFunction(i);
                if (actualUncertaintyId.equals(function.getUncertainItem().getId())) {
                    isIdFound =true;
                    //get realizations.
                    for (int v = 0; v < numValues; v++) {
                        long seed = determineSeed(v, uncertaintyIndex, ensembleIndex);
                        realization[v] = function.getRealization(values[v], seed);
                    }
                    break;
                }
            }
        } else if (uncertaintyType == MONTE_CARLO) {
            //find pdf object with id actualUncertaintyId.
            for (int i = 0; i < this.uncertainties.pdfCount(); i++) {
                Uncertainty function = this.uncertainties.getPdf(i);
                if (actualUncertaintyId.equals(function.getUncertainItem().getId())) {
                    isIdFound = true;
                    realization = getPdfRealizations(ensembleIndex, values, uncertaintyIndex, function);
                    break;
                }
            }

        } else {// not supported
            throw new RuntimeException(this.getClass() +  ": UncertaintyEngine.getNextRealizations() not implemented for mixed uncertainty types");
        }

        if (!isIdFound) {
            throw new RuntimeException(this.getClass() +  ": UncertaintyEngine.getNextRealizations(): No Uncertainty defined for Uncertainty Item " +  uncertaintyId) ;
        }

        return realization;
    }


    public void increaseTimeStepSeed() {
        //DaLogger.setInfoMessage("UncertaintyEngine: increaseTimeStepSeed()");
        timeStepSeed++;
    }

    public int getTimeStepSeed() {
        return timeStepSeed;
    }

    public void setTimeStepSeed(int timeStepCount) {
        timeStepSeed = timeStepCount;
    }

    public void setNumMonteCarloRuns(int numMonteCarloRuns) {
        this.numMonteCarloRuns = numMonteCarloRuns;
    }

    public double[] getNoise(String id, int ensembleIndex, double[] values) {
        if (values == null) {
            throw new IllegalArgumentException(this.getClass() + ": getNoise: values == null");
        }

        int uncertaintyIndex = findUncertaintyIndex(id);
        Uncertainty uncertainty = this.uncertainties.getUncertainty(uncertaintyIndex );
        return getPdfNoises(ensembleIndex, values, uncertaintyIndex, uncertainty);
    }

    public double getStdDev(String uncertaintyID) {
        PDF pdf = uncertainties.getPdf(determineActualUncertaintyId(uncertaintyID));
        if (!(pdf instanceof NormalDistribution)) {
            throw new IllegalArgumentException("UncertaintyEngine: uncertainty for \"" + uncertaintyID + "\" not defined as Normal Distribution");
        }
        return ((NormalDistribution) pdf).getStd();
    }

    // finalize
    public void finish() {
    }

    public String[] getUncertaintyIDs() {

        if (uncertaintyRegexMapping != null) {
            return uncertaintyRegexMapping.keySet().toArray(new String[uncertaintyRegexMapping.keySet().size()]);
        }

        int count  = this.uncertainties.uncertaintyCount();
        String[] uncertaintyIDs = new String[count];
        for (int i = 0; i < count; i++) {
            uncertaintyIDs[i] = this.uncertainties.getUncertainty(i).getUncertainItem().getId();
        }
        return uncertaintyIDs;
    }

    public double[] getStandardDeviations(List<String> uncertaintyIds, HashMap<String, Integer> selectedObservationValueCounts, int totalSelectedObservationValueCount) {
        return getStandardDeviations(uncertaintyIds, selectedObservationValueCounts, totalSelectedObservationValueCount, null);
    }

    public double[] getStandardDeviations(List<String> uncertaintyIds, HashMap<String, Integer> selectedObservationValueCounts,
			int totalSelectedObservationValueCount, double[] selectedObservationValues) {

        double[] stdDevs = new double[totalSelectedObservationValueCount];

        int index=0;
        for (String uncertaintyId : uncertaintyIds) {
            PDF pdf = uncertainties.getPdf(determineActualUncertaintyId(uncertaintyId));
            if (!(pdf instanceof NormalDistribution)) {
                throw new RuntimeException(this.getClass() +  "getStandardDeviations(): Uncertainty " +
                        uncertaintyId + " is not an Normal Distribution");
            }
            double stdDev = ((NormalDistribution) pdf).getStd();
            boolean stdDevIsFactor = ((NormalDistribution) pdf).isStdFactor();
            if (stdDevIsFactor && selectedObservationValues == null) {
                    throw new RuntimeException(this.getClass() +  "getStandardDeviations(): Uncertainty " +
                            " is defined as factor, but no actual values is provided");
            }

			//for each value that belongs to the current exchangeItem calculate and store the standard deviation in array stdDevs.
			int exchangeItemSelectedValueCount = (selectedObservationValueCounts == null || selectedObservationValueCounts.isEmpty()) ? 1 : selectedObservationValueCounts.get(uncertaintyId);
			for (int n = 0; n < exchangeItemSelectedValueCount; n++) {
				stdDevs[index] = stdDevIsFactor ? Math.abs(stdDev*selectedObservationValues[index]) : stdDev;
				index++;
			}
        }

        return stdDevs;
    }

    private String determineActualUncertaintyId(String uncertaintyId) {
        if (uncertaintyRegexMapping == null) {
            return uncertaintyId;
        } else {
            // regular expressions used
            return uncertaintyRegexMapping.get(uncertaintyId);
        }
    }

    public IVector getRealization(String uncertaintyID, IVector values) {
        IVector realization = values.clone();
        double [] realizationAsDoubles = getRealization(uncertaintyID, realizationCounter++, values.getValues());
        realization.setValues(realizationAsDoubles);
        return realization;
    }

    public IVector getRealization(IVector values) {
        if (values instanceof ITreeVector) {
            ITreeVector parameters = (ITreeVector) values ;
            TreeVector realizations = new TreeVector("obs-realization");
            for (String uncertaintyID : parameters.getSubTreeVectorIds()) {
                realizations.addChild(new TreeVector(uncertaintyID,
                        getRealization(uncertaintyID, parameters.getSubTreeVector(uncertaintyID))));
            }
            return realizations;
        }
        throw new RuntimeException("UncertaintyEngine: getRealizations() supported only for TreeVector");
    }

    public boolean hasMoreRealizations() {

        realizationCounter++;

        if (uncertaintyType == VARIATION_PER_PARAMETER) {
            if (this.uncertainties.variationFunctionCount() <= 0) {
                return false;
            }

            // store the currently handled uncertainty index (zero-based, run number is 1-based)
            currentUncertaintyIndex = (realizationCounter - 1) / 2;
            // still runs to go?
            return realizationCounter <= 2 * this.uncertainties.variationFunctionCount();

        } else if (uncertaintyType == MONTE_CARLO) {
            return this.uncertainties.pdfCount() > 0 && realizationCounter <= numMonteCarloRuns;

        } else {
            // not supported
            throw new RuntimeException(this.getClass() + ":  UncertaintyEngine.hasMoreRealizations() not implemented for mixed uncertainty types");
        }
    }

    public String[] getNextIds() {
        if (this.uncertainties.pdfCount() <= 0 && this.uncertainties.variationFunctionCount()<=0 ) return null;
        String[] nextIDs;
        if (uncertaintyType == VARIATION_PER_PARAMETER) {
            nextIDs = new String[1];
            nextIDs[0] = this.uncertainties.getVariationFunction(currentUncertaintyIndex).getUncertainItem().getId();
        } else if (uncertaintyType == MONTE_CARLO) {
            nextIDs = new String[this.uncertainties.pdfCount()];
            for (int i = 0; i < nextIDs.length; i++) {
                nextIDs[i] = this.uncertainties.getPdf(i).getUncertainItem().getId();
            }
        } else {
            // not supported
            throw new RuntimeException(this.getClass()+ ":   UncertaintyEngine.getNextIds() not implemented for mixed uncertainty types");
        }
        return nextIDs;
    }

    public double[] getNextRealization(String uncertaintyId, double[] values) {
        int uncertaintyIndex = findUncertaintyIndex(uncertaintyId);
        Uncertainty uncertainty = this.uncertainties.getUncertainty(uncertaintyIndex );
        return getPdfRealizations(realizationCounter, values, uncertaintyIndex, uncertainty);
    }

    private int findUncertaintyIndex(String id) {
        int count = uncertainties.uncertaintyCount();
        for (int i = 0; i < count; i++) {
            if (this.uncertainties.getUncertainty(i).getUncertainItem().getId()
                    .equals(id)) {
                return i;
            }
        }
        throw new RuntimeException("Uncertainty not defined for var. " + id);
    }

    private long determineSeed(int i, int uncertaintyIndex, int currentRunIndex) {
        long seed;
        if (useRandomSeed) {
            seed = ((Double) (Math.random() * 10000.0)).longValue();
        } else {
            seed = this.timeStepSeed +
                    (long) i * 10000 + (long) uncertaintyIndex * 100 + (long) currentRunIndex;
        }
        return seed;
    }

    private void readUncertainties(String uncertaintiesFile) throws IOException {
        File file = new File(uncertaintiesFile);
        UncertaintyReader reader = new UncertaintyReader(file);
        uncertainties = reader.getUncertainties(true);
        if (uncertainties == null ||
                (uncertainties.pdfCount() <= 0 && uncertainties.variationFunctionCount() <= 0)) {
            throw new RuntimeException(this.getClass()+ ":   No uncertainties specified in file " + file.getAbsolutePath());
        }
        this.uncertaintyType = uncertainties.getUncertaintyType();
        addAutoCorrelationToNormalDistribution();
    }

    // get PDF Realizations
    private double[] getPdfNoises(int ensembleIndex, double[] actualValues, int uncertaintyIndex, Uncertainty function) {

        int numValues = actualValues.length;
        double[] realizations = new double[numValues];
        double[] noises = new double[numValues];
        boolean doMultiply = false;
        boolean stdvIsFactor = false;
        double[] valuesToBeUsed = actualValues;
        double basicValue = function.getUncertainItem().getBasicValue();
        if ( ! Double.isNaN(basicValue) ) {
            valuesToBeUsed = new double[actualValues.length];
            for (int i = 0; i < actualValues.length; i++) {
                valuesToBeUsed[i] = basicValue;
            }
        }
        if ( function instanceof NormalDistribution) {
            stdvIsFactor = ((NormalDistribution)function).isStdFactor();
        } else if (function instanceof JointNormalDistribution) {
            stdvIsFactor = ((JointNormalDistribution)function).isStdFactor();
        }
        if ( ( function instanceof ChiSquareDistribution) ||
             ( function instanceof GammaDistribution) ||
             ( function instanceof LognormalDistribution) ||
             ( function instanceof WeibullDistribution)   ) {
            // for all above function multiply the basicvalue or actual value with the noise
            doMultiply = true;
        }
        //get seperate realizations.
        for (int v = 0; v < numValues; v++) {
            long seed = determineSeed(v, uncertaintyIndex, ensembleIndex);
            realizations[v] = function.getRealization(seed);
            if (! (function instanceof JointNormalDistribution)) {
                if (doMultiply) {
                    // multiply the basicvalue or actual value with the noise
                    realizations[v] *= valuesToBeUsed[v];
                } else {
                    if ( stdvIsFactor ) {
                        // if std is a factor then first update the realizations with the correct std value
                        realizations[v] *= valuesToBeUsed[v];
                    }
                    // add the basicvalue or actual value with the noise
                    realizations[v] += valuesToBeUsed[v];
                }
                //DaLogger.setDebugMessage(this.getClass().getSimpleName() + ": " + realizations[v] +
                //        " = getRealization(" + function.getUncertainItem().getId() + ", " + ensembleIndex + ", " + String.valueOf(valuesToBeUsed[v]) + ")"
                //        + "  :  Noise " + (realizations[v] - valuesToBeUsed[v]));
           }
        }

        if (function instanceof JointNormalDistribution) {
            realizations = ((JointNormalDistribution) function).getRealization(realizations, valuesToBeUsed); // here instead of realizations only noises are passed.
            // Actual realizsations are generated here based on autocorrelation.
            for (int v = 0; v < numValues; v++) { // for debug purpose
                //DaLogger.setDebugMessage(this.getClass().getSimpleName() + ": " + realizations[v] +
                //        " = getRealization(" + function.getUncertainItem().getId() + ", " + ensembleIndex + ", " + String.valueOf(actualValues[v]) + ")"
                //        + "  :  Noise " + (realizations[v] - valuesToBeUsed[v]));
            }
        }
        for (int v = 0; v < numValues; v++) {
            noises[v] =  realizations[v] - valuesToBeUsed[v];
        }

        return noises;
    }


    // get PDF Realizations
    private double[] getPdfRealizations(int ensembleIndex, double[] actualValues, int uncertaintyIndex, Uncertainty function) {

        int numValues = actualValues.length;
        double[] realizations = new double[numValues];
        boolean doMultiply = false;
        boolean stdvIsFactor = false;
        double[] valuesToBeUsed = actualValues;
        double basicValue = function.getUncertainItem().getBasicValue();
        if ( ! Double.isNaN(basicValue) ) {
            valuesToBeUsed = new double[actualValues.length];
            for (int i = 0; i < actualValues.length; i++) {
                valuesToBeUsed[i] = basicValue;
            }
        }
        if ( function instanceof NormalDistribution) {
            stdvIsFactor = ((NormalDistribution)function).isStdFactor();
        } else if (function instanceof JointNormalDistribution) {
            stdvIsFactor = ((JointNormalDistribution)function).isStdFactor();
        }
        if ( ( function instanceof ChiSquareDistribution) ||
             ( function instanceof GammaDistribution) ||
             ( function instanceof LognormalDistribution) ||
             ( function instanceof WeibullDistribution)   ) {
            // for all above function multiply the basicvalue or actual value with the noise
            doMultiply = true;
        }
        //get seperate realizations.
        for (int v = 0; v < numValues; v++) {
            long seed = determineSeed(v, uncertaintyIndex, ensembleIndex);
            realizations[v] = function.getRealization(seed);
            if (! (function instanceof JointNormalDistribution)) {
                if (doMultiply) {
                    // multiply the basicvalue or actual value with the noise
                    realizations[v] *= valuesToBeUsed[v];
                } else {
                    if ( stdvIsFactor ) {
                        // if std is a factor then first update the realizations with the correct std value
                        realizations[v] *= valuesToBeUsed[v];
                    }
                    // add the basicvalue or actual value with the noise
                    realizations[v] += valuesToBeUsed[v];
                }
                //DaLogger.setDebugMessage(this.getClass().getSimpleName() + ": " + realizations[v] +
                //        " = getRealization(" + function.getUncertainItem().getId() + ", " + ensembleIndex + ", " + String.valueOf(valuesToBeUsed[v]) + ")"
                //        + "  :  Noise " + (realizations[v] - valuesToBeUsed[v]));
           }
        }

        if (function instanceof JointNormalDistribution) {
            realizations = ((JointNormalDistribution) function).getRealization(realizations, valuesToBeUsed); // here instead of realizations only noises are passed.
            // Actual realizsations are generated here based on autocorrelation.
            for (int v = 0; v < numValues; v++) { // for debug purpose
                //DaLogger.setDebugMessage(this.getClass().getSimpleName() + ": " + realizations[v] +
                //        " = getRealization(" + function.getUncertainItem().getId() + ", " + ensembleIndex + ", " + String.valueOf(actualValues[v]) + ")"
                 //       + "  :  Noise " + (realizations[v] - valuesToBeUsed[v]));
            }
        }
        return realizations;
    }

    // if correlation defined then make the JointNormalDistribution object instead of NormalDistribution
    private void addAutoCorrelationToNormalDistribution() {
        for (int i = 0; i < uncertainties.pdfCount(); i++) {
            PDF pdf = uncertainties.getPdf(i);
            if (pdf instanceof NormalDistribution) {
                // replace NormalDistribution with JointNormal where autocorrelation is defined
                UncertainItem uncertainItem = pdf.getUncertainItem();
                for (int j = 0; j < uncertainties.autoCorrelationFunctionCount(); j++) {
                    AutoCorrelationFunction autoCorrelationFunction = uncertainties.getAutoCorrelationFunction(j);
                    UncertainItem autoCorrelationItem = autoCorrelationFunction.getUncertainItem();
                    if (autoCorrelationItem.compare(uncertainItem)) {
                        // remove NormalDistribution and add JointNormal
                        pdf = new JointNormalDistribution((NormalDistribution) pdf, autoCorrelationFunction);
                        // replace the JointNormalDistribution pdf with NormalDistribution
                        uncertainties.setPDF(i, pdf);
                        break;
                    }
                }
            }
        }
    }
}
