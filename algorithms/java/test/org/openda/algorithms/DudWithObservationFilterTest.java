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
package org.openda.algorithms;
import junit.framework.TestCase;
import org.openda.localization.LocalizationDomainsSimpleModel;
import org.openda.observationOperators.ObservationOperatorDeprecatedModel;
import org.openda.interfaces.*;
import org.openda.models.oscillator.OscillatorStochModelFactory;
import org.openda.observers.NoosTimeSeriesStochObserver;
import org.openda.utils.*;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Test of Dud, which uses observation filter for selecting observed data based on a user defined discharge range
 * and type of observation (assimilation; validation type observation is filtered out).
 */
public class DudWithObservationFilterTest extends TestCase {
    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
        testData = new OpenDaTestSupport(DudWithObservationFilterTest.class, "algorithms");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testDudWithObservationFilterTest_1() {
        Dud dudAlgorithm = new Dud();
        dudAlgorithm.initialize(testRunDataDir, new String[]{"dudAlgorithm.xml"});
        NoosTimeSeriesStochObserver stochObserver = new NoosTimeSeriesStochObserver();
        stochObserver.initialize(testRunDataDir, new String[]{"noosObservationsWithDischarge.xml"});
        IStochModelFactory stochModelFactory = new DummyStochModelFactory();
        stochModelFactory.initialize(null, null);
        IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.ModelDefault);
        IVector modelPrd = stochModelInstance.getObservationOperator().getObservedValues(stochObserver.getObservationDescriptions());
        System.out.println(modelPrd.toString());

        DummyDud dummyDud = new DummyDud();
        dummyDud.initialize(testRunDataDir, new String[]{"dudAlgorithm.xml"});
        dummyDud.setStochComponents(stochObserver,stochModelFactory);
        double cost = dummyDud.evaluateJ();
        assertEquals("cost: ",0.4000000000000006,cost);

        dummyDud.finish();
    }

    /**
     * Test for DUD : filtered observations
     */
	public void testDudCalibration() {
	    System.out.println("========================================================");
	    System.out.println("Optimization with DUD and oscillator model - filter obs");
	    System.out.println("========================================================");
        IStochModelFactory fact1 = new OscillatorStochModelFactory();
        fact1.initialize(null, new String[]{""});
        
        NoosTimeSeriesStochObserver obsGenerated = new NoosTimeSeriesStochObserver();
        obsGenerated.initialize(testRunDataDir, new String[]{"noosOscillatorWithFilter.xml"});
        		
        IVector pTrue = new Vector("[8.5,1.7]"); //default [8.0,1.5707963267948966]
  	    // Now start calibration through proper algorithm-class
	    String dudConfig = "dudOscillator.xml";
        Dud algorithm = new Dud();
        algorithm.initialize(testRunDataDir,new String[]{dudConfig});
        algorithm.setStochComponents(obsGenerated,fact1);
        algorithm.prepare();
        algorithm.run();
        IVector pCal=new Vector(algorithm.getBestEstimate().getParameters());
        System.out.println("pCal = "+pCal);
        System.out.println("Should be pCal = [8.499788331592638,1.6999968989080512]");
        System.out.println("This is close to pTrue = "+pTrue);
        assertEquals("pCal",pCal.toString(),"[8.499788331592638,1.6999968989080512]");
		algorithm.finish();
	}
    private class DummyStochModelFactory implements IStochModelFactory {

        public void initialize(File workingDir, String[] arguments) {
            // no action
        }

        public IStochModelInstance getInstance(OutputLevel outputLevel) {
            return new DummyModelInstance();
        }

        public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
            throw new UnsupportedOperationException("org.openda.models.oscillator.DummyStochModelFactory.getPostprocessorInstance(): Not implemented yet.");
        }

		public void finish() {
			// no action needed
		}
	}

    private class DummyModelInstance implements IStochModelInstance, IStochModelInstanceDeprecated {

        public IObservationOperator getObservationOperator() {
            return new ObservationOperatorDeprecatedModel(this);
        }

        public IVector getObservedValues(IObservationDescriptions observationDescriptions) {

            TreeVector treeVector = new TreeVector("predictions");
            List<IPrevExchangeItem> items = observationDescriptions.getExchangeItems();
            for(IPrevExchangeItem item : items) {
                double[] prd = item.getValuesAsDoubles();
                for (int i=0; i<prd.length; i++){
                    prd[i] += 0.01;
                }
                treeVector.addChild(item.getId(),prd);
            }
            return treeVector;
        }

        public void initialize(File workingDir, String[] arguments) {
            // no action needed (handled by constructors)
        }

        public ILocalizationDomains getLocalizationDomains(){
            return new LocalizationDomainsSimpleModel();
        }

        public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getObservedLocalization(): Not implemented yet.");
        }

        public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance, int iDomain) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getObservedLocalization(): Not implemented yet.");
        }

        public IVector getState(int iDomain) {
            return this.getState();
        }

        public IVector getState() {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getState(): Not implemented yet.");
        }

        public void axpyOnState(double alpha, IVector vector) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.axpyOnState(): Not implemented yet.");
        }

        public void axpyOnState(double alpha, IVector vector, int iDomain) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.axpyOnState(): Not implemented yet.");
        }

        public IVector getParameters() {
            return null;
        }

        public void setParameters(IVector parameters) {
            // do nothing
        }

        public void axpyOnParameters(double alpha, IVector vector) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.axpyOnParameters(): Not implemented yet.");
        }

        public IStochVector getStateUncertainty() {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getStateUncertainty(): Not implemented yet.");
        }

        public IStochVector getParameterUncertainty() {
        	int length=2; // TODO: how do I know the length is 2?
            IVector zeroVector = new Vector(length);
            zeroVector.setConstant(0.0);
    		return new StochVector(zeroVector,zeroVector);
        }

        public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getWhiteNoiseUncertainty(): Not implemented yet.");
        }

        public boolean isWhiteNoiseStationary() {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.isWhiteNoiseStationary(): Not implemented yet.");
        }

        public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getWhiteNoiseTimes(): Not implemented yet.");
        }

        public IVector[] getWhiteNoise(ITime timeSpan) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getWhiteNoise(): Not implemented yet.");
        }

        public void setWhiteNoise(IVector[] whiteNoise) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.setWhiteNoise(): Not implemented yet.");
        }

        public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.axpyOnWhiteNoise(): Not implemented yet.");
        }

        public void setAutomaticNoiseGeneration(boolean value) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.setAutomaticNoiseGeneration(): Not implemented yet.");
        }

        public void announceObservedValues(IObservationDescriptions observationDescriptions) {
            // do nothing
        }

        public IVector getStateScaling() {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getStateScaling(): Not implemented yet.");
        }

        public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getStateScaling(): Not implemented yet.");
        }

        public ITime getTimeHorizon() {
            return  new Time(54465.99965277778,54466.12534722222);
        }

        public ITime getCurrentTime() {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getCurrentTime(): Not implemented yet.");
        }

        public void compute(ITime targetTime) {
			//Do nothing
        }

        public String[] getExchangeItemIDs() {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getExchangeItemIDs(): Not implemented yet.");
        }

        public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getExchangeItemIDs(): Not implemented yet.");
        }

		public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
			throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getDataObjectExchangeItem(): Not implemented yet.");
		}

		public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getExchangeItem(): Not implemented yet.");
        }

        public IModelState saveInternalState() {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.saveInternalState(): Not implemented yet.");
        }

        public void restoreInternalState(IModelState savedInternalState) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.restoreInternalState(): Not implemented yet.");
        }

        public void releaseInternalState(IModelState savedInternalState) {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.releaseInternalState(): Not implemented yet.");
        }

		public IModelState loadPersistentState(File persistentStateFile) {
			throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.loadPersistentState(): Not implemented yet.");
		}

		public File getModelRunDir() {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getModelRunDir(): Not implemented yet.");
        }

	    public void finish() {
//            do nothing
        }

        public IInstance getParent() {
            throw new UnsupportedOperationException("org.openda.algorithms.DudWithObservationFilterTest.DummyModelInstance.getParent(): Not implemented yet.");
        }
    }

    private class DummyDud extends Dud {
        private File workingDir=null;
        private String configString=null;

        ConfigTree dudConfig = null;
        private SimulationKwadraticCostFunction J=null;
        private DudCoreOptimizer dudOptimizer = null;


        public void initialize(File workingDir, String[] arguments) {
            this.workingDir = workingDir;
            this.configString = arguments[0];
        }

        public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory) {
			this.stochObserver = stochObserver;

            Results.putProgression(this, "Dud initialisation (stoch. obs. and stoch. model have been set)");

            //parse and store config
            Results.putMessage(this, "configString = "+ configString);
            dudConfig = new ConfigTree(workingDir, configString);

            // Get initial parameters from model
            Results.putProgression(this, "Retrieving initial parameters from model");
            IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
            pInit = stochModelInstance.getParameters();

            Results.putProgression(this, "Starting optimizer");
            // Create costFunction
            // <costFunction weakParameterConstraint="off" class="org.openda.algorithms.SimulationKwadraticCostFunction"
            //     factor="0.5" />
            Results.putMessage(this, "costFunction@class="+ dudConfig.getAsString("costFunction@class","SimulationKwadraticCostFunction"));
            if(dudConfig.getAsString("costFunction@class","SimulationKwadraticCostFunction").contains("SimulationKwadraticCostFunction")){
                //create selection of relevant observations
                ITime selectionTimes = stochModelInstance.getTimeHorizon();
                IStochObserver obsSelection = stochObserver.createSelection(selectionTimes);

                this.J = new SimulationKwadraticCostFunction(stochModelFactory,obsSelection);
                //this.J = new SimulationKwadraticCostFunction(stochModelFactory,stochObserver);
            }else{
                throw new RuntimeException("Only implemented for one costfunction yet: org.openda.algorithms.SimulationKwadraticCostFunction");
            }

            // options for costFunctions
            J.addBackgroundTerm = dudConfig.getAsBoolean("costFunction@weakParameterConstraint",false);
            Results.putMessage(this, "costFunction@weakParameterConstraint="+J.addBackgroundTerm);
            J.factor = dudConfig.getAsDouble("costFunction@factor",J.factor);
            Results.putMessage(this, "costFunction@factor="+J.factor);
            //  tryParallel="true"
            J.setTryParallel(dudConfig.getAsBoolean("costFunction@tryParallel",false));
            Results.putMessage(this, "costFunction@tryParallel="+J.getTryParallel());

            // create optimizer
            this.dudOptimizer = new DudCoreOptimizer(J);
            // options for optimizer
            /*
                <outerLoop maxIterations=10 absTolerance=0.01 relTolerance=0.01 relToleranceLinearCost=0.01 />
               <lineSearch maxIterations=5 maxRelStepSize=10.0 >
                  <backtracking shorteningFactor=0.5 startIterationNegativeLook=3 />
            */
            //outerloop
            this.dudOptimizer.maxit = dudConfig.getAsInt("outerLoop@maxIterations", dudOptimizer.maxit);
            Results.putMessage(this, "outerLoop@maxIterations="+this.dudOptimizer.maxit);
            this.dudOptimizer.absTol= dudConfig.getAsDouble("outerLoop@absTolerance",this.dudOptimizer.absTol);
            Results.putMessage(this, "outerLoop@absTolerance="+this.dudOptimizer.absTol);
            this.dudOptimizer.relTol= dudConfig.getAsDouble("outerLoop@relTolerance",this.dudOptimizer.relTol);
            Results.putMessage(this, "outerLoop@relTolerance="+this.dudOptimizer.relTol);
            this.dudOptimizer.relTolLinCost = dudConfig.getAsDouble("outerLoop@relToleranceLinearCost",this.dudOptimizer.relTolLinCost);
            Results.putMessage(this, "outerLoop@relToleranceLinearCost="+this.dudOptimizer.relTolLinCost);
            //linesearch
            this.dudOptimizer.maxInnerIter = dudConfig.getAsInt("lineSearch@maxIterations", dudOptimizer.maxInnerIter);
            Results.putMessage(this, "lineSearch@maxIterations="+this.dudOptimizer.maxInnerIter);
            this.dudOptimizer.maxStep = dudConfig.getAsDouble("lineSearch@maxRelStepSize", dudOptimizer.maxStep);
            Results.putMessage(this, "lineSearch@maxRelStepSize="+this.dudOptimizer.maxStep);
            this.dudOptimizer.minInnerNegativeLook = dudConfig.getAsInt("lineSearch/backtracking@startIterationNegativeLook", dudOptimizer.minInnerNegativeLook);
            Results.putMessage(this, "lineSearch/backtracking@startIterationNegativeLook="+this.dudOptimizer.minInnerNegativeLook);
            this.dudOptimizer.innerScaleFac = dudConfig.getAsDouble("lineSearch/backtracking@shorteningFactor", dudOptimizer.innerScaleFac);
            Results.putMessage(this, "lineSearch/backtracking@shorteningFactor="+this.dudOptimizer.innerScaleFac);
            // additional stopCriteria
            ConfigTree parts[] = dudConfig.getSubTrees("stopCriteria/stopCriterion");
            if (parts != null) {
                String className = null;
                double threshold;
                Class javaClass;
                Object object = null;
                for (ConfigTree part : parts) {
                    className = part.getAsString("@class", null);
                    threshold = part.getAsDouble("@threshold", this.dudOptimizer.stopCritThresDefault);
                    Results.putMessage(this, "stopCriteria/stopCriterion@class=" + className + ", @threshold=" + threshold);
                    try {
                        javaClass = Class.forName(className);
                        try {
                            object = javaClass.newInstance();
                        } catch (InstantiationException e) {
                            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                        } catch (IllegalAccessException e) {
                            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                        }
                        this.dudOptimizer.stopCriteria.add((IStopCriterion) object);
                        this.dudOptimizer.stopCriteriaThreshold.add(threshold);
                    } catch (ClassNotFoundException e) {
                        throw new RuntimeException("Could not create instance for " + className + ": " + e.getMessage());
                    }
                }
                ITime selectionTimes = stochModelInstance.getTimeHorizon();
                this.dudOptimizer.obsDescr = stochObserver.createSelection(selectionTimes).getObservationDescriptions();
            }
            ConfigTree partsObsFilter[] = dudConfig.getSubTrees("observationFilters/observationFilter");
            if (partsObsFilter != null) {
                List<IObservationSpaceFilter> obsFilters = new ArrayList<IObservationSpaceFilter>();
                List<File> obsFilterWorkingDirectory = new ArrayList<File>();
                List<String[]> obsFilterArguments = new ArrayList<String[]>();
                String obsFilterString = null;
                String className = null;
                File obsFilterWorkingDir = null;
                String[] arguments;
                Class javaClass;
                Object object = null;
                for (ConfigTree part : partsObsFilter) {
                    className = part.getAsString("@class", null);
                    String temp = part.getAsString("workingDirectory", obsFilterString);
                    if (temp!=null){
                        obsFilterWorkingDir = new File(this.workingDir,temp);
                    }
                    arguments = new String[]{part.getAsString("configFile", obsFilterString)};
                    Results.putMessage(this, "observationFilters/observationFilter@class=" + className +
                            ", workingDirectory=" + obsFilterWorkingDir.toString() + ", configFile=" + arguments[0]);
                    System.out.println("observationFilters/observationFilter@class=" + className +
                            ", workingDirectory=" + obsFilterWorkingDir + ", configFile=" + arguments[0]);
                    try {
                        javaClass = Class.forName(className);
                        try {
                            object = javaClass.newInstance();
                        } catch (InstantiationException e) {
                            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                        } catch (IllegalAccessException e) {
                            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                        }
                        obsFilters.add((IObservationSpaceFilter) object);
                        obsFilterWorkingDirectory.add(obsFilterWorkingDir);
                        obsFilterArguments.add(arguments);
                    } catch (ClassNotFoundException e) {
                        throw new RuntimeException("Could not create instance for " + className + ": " + e.getMessage());
                    }
                }
                initializeObservationFilters(obsFilters, obsFilterWorkingDirectory, obsFilterArguments);
                this.J.setListObservationFilter(obsFilters);
            }
            stochModelInstance.finish();

        }

        
        protected void initializeObservationFilters(List<IObservationSpaceFilter> obsFilters, List<File> obsFilterWorkingDirectory,List<String[]> obsFilterArguments) {
            int i = 0;
            for (IObservationSpaceFilter obsFilter : obsFilters){
                File configFile = obsFilterWorkingDirectory.get(i);
                String[] arguments = obsFilterArguments.get(i);
                obsFilter.initialize(configFile,arguments);
                i++;
            }
        }

        public double evaluateJ(){
            IVector dummyParam = new Vector(new double[]{0.1,0.2});
            return J.evaluate(dummyParam,"initialization");
        }
    }
}
