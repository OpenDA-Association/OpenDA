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
package org.openda.algorithms.kalmanFilter;
import org.openda.algorithms.BFGSCoreOptimizer;
import org.openda.algorithms.ConjugateGradientCoreOptimizer;
import org.openda.interfaces.*;
import org.openda.utils.ConfigTree;
import org.openda.utils.Results;
import org.openda.utils.Vector;

import java.io.File;

/**
 * 3D-VAR is an sequential method that updates the state of the model at each
 * assimilation time with a cot function like
 * J(x) = (x- x_b)' inv(B) (x - x_b) + (y - H x)' inv(R) (y - H x)
 * where:
 * x_b is prior estimate based on previous forecast
 * y are the observations.
 *  
 * The method should work with FGAT, but this should be tested.
 *
 * Input syntax
 *  
 * <ThreeDVarConfig>
 * 		<costFunction factor="0.5" /> 
 * 		<bfgsLoop limitedMemory="true" numOfVectors="3" maxIterations="20" /> 
 * </ThreeDVarConfig>
 * 
 */

public class ThreeDVar extends AbstractSequentialAlgorithm{
	// configuration data for reading
    public int ThreeDVarTimeCounter = 0;


	private double factor = 0.5;
	private int algorithm = 2; // BFGS = 1 , CG = 2
	private boolean BFGSlimitedMemory = true;
	private int BFGSnStore = 3;
	private int CGmethod = 1;
	private int maxitAlgorithm = 25;
	private int maxitBrent = 200;
	private int maxitBracket = 20;
	private double relTolGrad = 0.01;
	private double absTolGrad = 0.01;
	private double relTolStep = 0.001;
	private double absTolStep = 0.001;
	private double relTolBrent = 0.01;
	private double absTolBrent = 0.01;
	private double bracketFirstTry = 1.0;
	private double limit = 100.0;
    
    
	public void initialize(File workingDir, String[] arguments) {
		super.initialize(workingDir, arguments);
		
		/*
         * Input syntax
         *  
		 * <ThreeDVarConfig>
		 * 		<costFunction factor="0.5" /> 
		 * 		<algorithm type="BFGS" limitedMemory="true" numOfVectors="3" maxIterations="20" absTolGrad="0.01" relTolGrad="0.01" absTolStep="0.01" relTolStep="0.01" /> 
		 *      <lineSearch type="brent" maxIterations="200" absTolBrent="0.01" relTolBrent="0.01">
		 *           <brent startBracketValue="1.0" maxItBracket="20.0" maxExtension="100.0"/>
		 *      </lineSearch>
		 * </ThreeDVarConfig>
		 */

		// Extend with tolerances and options for line search?
		
		ConfigTree ThreeDVarConf = new ConfigTree(workingDir, configString);
		
		this.factor = ThreeDVarConf.getAsDouble("costFunction@factor", this.factor);
		//Results.putMessage("costFunction@factor="+this.factor);
        String algorithm = ThreeDVarConf.getAsString("algorithm@type","BFGS");
        if (algorithm.equalsIgnoreCase("bfgs")|algorithm.equals("1")){
			this.algorithm = 1;
        	this.BFGSlimitedMemory = ThreeDVarConf.getAsBoolean("algorithm@limitedMemory",this.BFGSlimitedMemory);
			//Results.putMessage("algorithm@limitedMemory="+this.limitedMemory);
			this.BFGSnStore = ThreeDVarConf.getAsInt("algorithm@numOfVectors", this.BFGSnStore);		
			//Results.putMessage("algorithm@numOfVectors="+this.nStore);
        } else {
        	if (algorithm.equalsIgnoreCase("conjugategradient")|algorithm.equalsIgnoreCase("cg")|algorithm.equals("2")){
        		this.algorithm = 2;
        		String method = ThreeDVarConf.getAsString("algorithm@method","fletcher-reeves");
    	        if ((method.equalsIgnoreCase("fletcher reeves")|method.equalsIgnoreCase("fletcher-reeves"))
    	        	|method.equals("1")){
    	        	this.CGmethod = 1;
    	        	Results.putMessage("algorithm@method=Fletcher-Reeves=1");
    	        } else {
    	        	if ((method.equalsIgnoreCase("polak ribiere")|method.equalsIgnoreCase("polak-ribiere"))
    	        		|method.equals("2")){
    	            	this.CGmethod = 2;
    	            	Results.putMessage("algorithm@method=Polak-Ribiere=2");
    	        	} else {
    	        		this.CGmethod = 3;
    	            	Results.putMessage("algorithm@method=Steepest Descent=3");
    	        	}
    	        }
        	} else {
            	throw new RuntimeException("Only method 'BFGS' and 'ConjugateGradient' are supported for 3D-VAR at this time.");
        	}
        }
        this.maxitAlgorithm = ThreeDVarConf.getAsInt("algorithm@maxIterations", this.maxitAlgorithm);
		//Results.putMessage("algorithm@maxIterations="+this.maxitAlgorithm);
        this.absTolGrad=ThreeDVarConf.getAsDouble("algorithm@absTolGrad",this.absTolGrad);
        //Results.putMessage("algorithm@absTolGrad="+this.absTolGrad);
        this.relTolGrad=ThreeDVarConf.getAsDouble("algorithm@relTolGrad",this.relTolGrad);
        //Results.putMessage("algorithm@relTolGrad="+this.relTolGrad);
        this.absTolStep=ThreeDVarConf.getAsDouble("algorithm@absTolStep",this.absTolStep);
        //Results.putMessage("algorithm@absTolStep="+this.absTolStep);
        this.relTolStep=ThreeDVarConf.getAsDouble("algorithm@relTolStep",this.relTolStep);
        //Results.putMessage("algorithm@relTolStep="+this.relTolStep);
        String lineSearchMethod = ThreeDVarConf.getAsString("lineSearch@type","brent");
        if(!lineSearchMethod.equalsIgnoreCase("brent")){
        	throw new RuntimeException("Only method 'Brent' supported for linesearch at this time.");
        }
        this.maxitBrent = ThreeDVarConf.getAsInt("lineSearch@maxIterations",this.maxitBrent);
        //Results.putMessage("lineSearch@maxIterations="+this.maxitBrent);
        this.absTolBrent = ThreeDVarConf.getAsDouble("lineSearch@absTolBrent",this.absTolBrent);
        //Results.putMessage("lineSearch@absTolBrent="+this.absTolBrent);
        this.relTolBrent = ThreeDVarConf.getAsDouble("lineSearch@relTolBrent",this.relTolBrent);
        //Results.putMessage("lineSearch@relTolBrent="+this.relTolBrent);
       this.bracketFirstTry = ThreeDVarConf.getAsDouble("lineSearch/brent@startBracketValue",this.bracketFirstTry);
        //Results.putMessage("lineSearch/brent@startBracketValue="+this.bracketFirstTry);
        this.maxitBracket = ThreeDVarConf.getAsInt("lineSearch/brent@maxItBracket",this.maxitBracket);
        //Results.putMessage("lineSearch/brent@maxItBracket="+this.maxitBracket);
        this.limit = ThreeDVarConf.getAsDouble("lineSearch/brent@maxExtension",this.limit);
        //Results.putMessage("lineSearch/brent@maxExtension="+this.limit);
		
		this.ThreeDVarTimeCounter++;
		
	}
	
	
	public void prepare() {
		// Auto-generated method stub
	}
	
	
	public void forecast(IStochObserver observations, ITime targetTime) {
		// nothing to do, because forecast of mainModel is done automatically
	}
	
	public void analysis(IStochObserver observations, IVector obsValues, IVector predMainModel,
			IStochModelInstance mainModel, ITime analysisTime) {
		
		if (!(this.mainModel instanceof IModelAdjoint)) {
			throw new RuntimeException("Error: The model is not an instance of IModelAdjoint. 3D-Var needs an adjoint to compute the gradient.");
		}
		
		IModelState savedState = this.mainModel.saveInternalState();
		ISqrtCovariance L = this.mainModel.getStateUncertainty().getSqrtCovariance();
		
		AnalysisLeastSquaresCostWithGradient f = new AnalysisLeastSquaresCostWithGradient(mainModel, observations, obsValues, savedState);
		f.factor = this.factor;
		
		System.out.println("x_f = "+ mainModel.getState());
		System.out.println("pred_f = "+predMainModel);
		
        Results.putValue("x_f", mainModel.getState(), mainModel.getState().getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
        Results.putValue("pred_f", predMainModel, predMainModel.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);

		// initialize with forecast which scales to 0
		IVector pInit = new Vector(L.getNumberOfColumns());  //TODO : is a java vector now, replace.
		
		if (this.algorithm == 1) {
			BFGSCoreOptimizer bfgs = new BFGSCoreOptimizer(f);
			bfgs.maxitBfgs = this.maxitAlgorithm;
			bfgs.limitedMemory = this.BFGSlimitedMemory;
			bfgs.nStore = this.BFGSnStore;
			bfgs.initialize(pInit);
			bfgs.optimize();
		} else {
			ConjugateGradientCoreOptimizer cg = new ConjugateGradientCoreOptimizer(f);
			cg.maxitConGrad = this.maxitAlgorithm;
			cg.method = this.CGmethod;
			cg.initialize(pInit);
			cg.optimize();		
		}
		
		//set model state to optimal state
		IVector xiOpt = f.getOptimalState();
		//IVector deltaState = this.mainModel.getState();
		//L.rightMultiply(1.0, xiOpt, 0.0, deltaState);
		xiOpt.axpy(-1.0,mainModel.getState());
		mainModel.axpyOnState(1.0, xiOpt);
		
        Results.putValue("x_opt", mainModel.getParameters(), mainModel.getParameters().getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);

	}
	
}
