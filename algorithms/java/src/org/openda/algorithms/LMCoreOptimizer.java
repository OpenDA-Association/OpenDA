/* OpenDA v2.4.3 
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
package org.openda.algorithms;

import org.openda.interfaces.*;
import org.openda.utils.Matrix;
import org.openda.utils.Results;
import org.openda.utils.Vector;

import java.util.ArrayList;
import java.util.List;

public class LMCoreOptimizer {

	// fields of this class
	protected IVector pCurrent = null;    // parametervalues under consideration
	protected IVector predCurrent = null; // predictions for each parameter vector
	protected double fCurrent;     // costs for each
	protected LeastSquaresCostFunction f = null;

	protected int number_of_stored_runs=1;
	protected int number_of_evaluations;  //number of evaluations (without initial evaluation)
	protected double initialStep = 1.0; // scaling for initial stepsize
	protected double costFactor;            // multiplication factor for cost

	// stopping criteria
	public int maxit = 3;                // maximum number of outer iterations
	public int maxInnerIter = 6;         // maximum number of inner iterations
//	public double innerScaleFac = 0.5;   // scaling factor for bactracking
//	public int minInnerNegativeLook = 3; // when to start looking into the negative direction
	public double maxStep = 10.0;        // maximum relative step size (compared to the spread of the parameters)
	public double absTol = 0.01;         // absolute criterion
	public double relTol = 0.01;         // relative criterion
	public double absTolGrad = 0.01;     // absolute tolerance of gradient
	public double relTolGrad = 0.01;     // relative tolerance of gradient
	public double tiny = 1.0e-15;    		// value to avoid division by zero

	// settings
//	public double lambda = 0.00000001;         // default Levenberg parameter
//	public double lambda = 0.00000001;         // default Levenberg parameter
	public double lambda = 0.001;         // default Levenberg parameter
	public double nu = 10;				  // default Marquardt parameter
	public IVector fwdEps = null;		  // relative error used in forward difference calculation of Jacobian matrix

	// required for the additional stopping criteria:
	public List<IStopCriterion> stopCriteria = new ArrayList<IStopCriterion>();
	public List<Double> stopCriteriaThreshold = new ArrayList<Double>();
	public double stopCritThresDefault = 0.01;
	public IObservationDescriptions obsDescr=null;

	// caching
	protected ISqrtCovariance LPar = null;
	protected IVector parInit = null;
	protected IStochVector svObs = null;
	protected IVector obs = null;
	protected IVector sigmaObs = null;
	private double costGrad; // caching the norm of cost gradient of the so far last iteration


	// stopping
	boolean moreToDo = true; // is this optimization finished
	int imain = 0;           // main iterations done

	protected IMatrix paramCovEst = null;   // estimate of square root of error covariance
	public double relTolLinCost = 0.01;  // compares linearity error to improvement
	public boolean computeErrorEstimate=true;
	public boolean computeStateJacobian = false;
	private Matrix stateCovEst;
	private Matrix Js; // Jacobian wrt to the whole state variable
	private IVector[] Jp; // Jacobian wrt to observed/forecast variable

	public LMCoreOptimizer(LeastSquaresCostFunction f) {
		this.f = f;
//		int m = numberOfSearchDirections();
//		number_of_stored_runs = m+1;
//		number_of_evaluations = m;
	}
//	protected int numberOfSearchDirections() {
//		IStochVector parameterUncertainty = f.getParameterUncertainty();
//		ISqrtCovariance sqrtCovariance = parameterUncertainty.getSqrtCovariance();
//		IVector[] vectorArray = sqrtCovariance.asVectorArray();
//		return vectorArray.length;
//	}

	protected double InitialSearchStep(int i, int j){
		return (i==j ? 1 : 0);
	}
	
//	/**
//	 * Initialization for LM minimization - used for restarting the optimization process
//	 * @param pars : parameters as Vector at each node of LM
//	 * @param costValues : corresponding cost values, so there is no need to recompute them (saves time)
//	 * @param predictions : corresponding predictor values
//	 */
//	public void initialize(IVector pars[], double costValues[], IVector[] predictions){
//
//		this.pCurrent = new IVector[number_of_stored_runs];
//		this.fCurrent = new double[number_of_stored_runs];
//		this.predCurrent = new IVector[number_of_stored_runs];
//		for (int i = 0; i < pars.length; i++) {
//			this.pCurrent[i] = pars[i].clone();
//			this.fCurrent[i] = costValues[i];
//			this.predCurrent[i] = predictions[i].clone();
//		}
//
//		// Initialize the cost functions with invalid values (so they'll be at the end when sorted)
//		for (int i = pars.length; i < number_of_stored_runs ; i++) { this.fCurrent[i] = 1.0E30 ;}
//
//		f.evaluate(pars[number_of_evaluations], "initialization node " + number_of_evaluations);
//
//		number_of_evaluations = pars.length-1;
//		IStochVector parameterUncertainty = f.getParameterUncertainty();
//		this.LPar = parameterUncertainty.getSqrtCovariance();
//		this.parInit = parameterUncertainty.getExpectations();
//		this.svObs = f.getObservationUncertainty();
//		sortByCostAndCashObservationData();
//	}

	public void initialize(IVector pInit){
		// Get uncertainty parameters
		IStochVector parameterUncertainty = f.getParameterUncertainty();
		this.LPar = parameterUncertainty.getSqrtCovariance();
		this.parInit = parameterUncertainty.getExpectations();

		// Evaluate the initial cost function and prediction
		this.pCurrent = pInit.clone();
		this.fCurrent = f.evaluate(pCurrent, "initialization node 1");
		this.predCurrent = f.getLastPredictions();
		this.svObs = f.getObservationUncertainty();
		this.obs = this.svObs.getExpectations();
		this.sigmaObs = this.svObs.getStandardDeviations();

		//TODO: evaluate initial gradient, uncertainty, stop criteria

	}


	/**
	 * Get the optimal parameters, i.e. the result of the minimization
	 *
	 * @return parameters as Vector
	 */
	public IVector getOptimalValue() {
		return this.f.getOptimalParameters();
	}

	/**
	 * Get the cost value at the optimum
	 *
	 * @return cost
	 */
	public double getOptimalCost() {
		return this.f.getOptimalCost();
	}

	/**
	 * Get parameters for each point of the LM. The current LM equals the initial
	 * LM if requested before optimization and final
	 * LM if f.optimize() has been called.
	 *
	 * @return Vector for parameters at each nod of the LM vectors
	 */
	public IVector getCurrentValues() {
		IVector result;
		result = this.pCurrent.clone();
		return result;
	}

	/**
	 * Get the current cost values corresponding to the nodes of the current
	 * LM.
	 *
	 * @return Cost at each node as array of doubles
	 */
	public double getCurrentCosts() {
		double result = this.fCurrent;
		return result;
	}

	/**
	 * Main optimization routine
	 */
	public void optimize() {
		while (this.hasNext()) {
			this.next();
		}
	}

	/**
	 * Are there any more steps for this algorithm
	 *
	 * @return has next step
	 */
	boolean hasNext() {
		return this.moreToDo;
	}

	/**
	 * Return an estimate of the uncertainty of the parameters as square-root of the covariance
	 * @return sqrt cov
	 */
	public IMatrix getParamCovariance(){
		IMatrix result = null;
		if(this.paramCovEst !=null){
			result=this.paramCovEst.clone();
		}
		return result;
	}
	
	
	boolean isThereMoreToDo(double costCurrent, double cost, double costGrad, IVector predTry, IVector pars, int innerIter) {
		/* This function checks using all the criteria whether another iteration is still needed. */
		double diff = Math.abs(costCurrent - cost);
		double relDiff=diff/Math.abs(costCurrent);
		double relCostGrad = Math.abs(costGrad-this.costGrad)/this.costGrad;
		Results.putMessage("stop criterion 1, imain                            > maxit :\t " + imain   + " < " + maxit);
		Results.putMessage("stop criterion 2, |new - previous cost|            < abstol:\t " + diff    + " > " + absTol);
		Results.putMessage("stop criterion 3, |new - previous cost|/|new cost| < reltol:\t " + relDiff + " > " + relTol);
		Results.putMessage("stop criterion 4, inner iteration:                 > maxInnerIt :\t " + innerIter + " < " + this.maxInnerIter);
		Results.putMessage("stop criterion 5, |costGrad|                       > absTolGrad :\t "+costGrad+" < "+absTolGrad);
		Results.putMessage("stop criterion 6, |new costGrad - costGrad|/|costGrad|:\t "+relCostGrad+" < "+relTolGrad);

		boolean moreToDo =
				imain < this.maxit
						&& diff > this.absTol
						&& diff > relTol * Math.abs(costCurrent)
						&& costGrad>absTolGrad
						&& relCostGrad>relTolGrad;

		// additional stop criteria:
		if (stopCriteria.size()>0) {
			IVector residual = obs.clone();
			residual.axpy(-1.0,predTry);
			boolean isStop = false;
			for (int i=0; i<stopCriteria.size(); i++){
				IStopCriterion object = stopCriteria.get(i);
				double threshold = stopCriteriaThreshold.get(i);
				if (obsDescr!=null) {
					isStop = object.checkForStop(pars,residual,obsDescr,costCurrent,threshold);
				} else {
					isStop = object.checkForStop(pars,residual,costCurrent,threshold);
				}
				Results.putMessage(object.toString());
				this.moreToDo = this.moreToDo & !isStop;
			}
		}

		return moreToDo;
	}
	
	protected Matrix CalculateGradSimu(){
		//Calculate Jacobian here using forward difference, scaled by obs error std: sqrt(R)^-1*J
		//NOTE: only diagonal R is supported so far.
		LeastSquaresCostFunction fJacobian = f.clone();
		int nParam = this.pCurrent.getSize();
		IVector currentState = null;
		IVector[] Js = new IVector[0];
		try {
			currentState = ((SimulationKwadraticCostFunction) f).getState();
			computeStateJacobian = true;
			Js = new IVector[nParam];
		} catch (RuntimeException e) {
			Results.putMessage("NOTE:");
			Results.putMessage("   Either model failed or Jacobian w.r.t. the whole state vector is not computed (not specified in the stochModel input file): " + e.getMessage());
		}
		IVector[] J = new IVector[nParam];
		Jp = new IVector[nParam];
		for (int i=0; i<nParam; i++){
			IVector param = this.pCurrent.clone();
			//Note that here 'param' is actually 'deltaParam'. It's not possible for the moment to
			//access the actual parameter value. Hence, it is not possible to use relative error for
			//the forward difference computation, i.e. p = (1+eps)*p. Instead, we can only use here
			//an absolute error: p = eps+p
			param.setValue(i,this.fwdEps.getValue(i)+param.getValue(i));
			double dummy = fJacobian.evaluate(param, "Forward difference calculation of Jacobian matrix");
			J[i] = fJacobian.getLastPredictions();
			J[i].axpy(-1.0, this.predCurrent);
			J[i].scale(1.0d / this.fwdEps.getValue(i));
			Jp[i] = J[i].clone();
			//Write the non-scaled Jacobian in the output file:
			Results.putValue("Jacobian_param" + i , J[i], J[i].getSize(), "Forward difference calculation of Jacobian matrix of the forecast variables", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Instance);
			J[i].pointwiseDivide(this.sigmaObs);
//			J[i].scale(1.0d / this.sigmaObs.getValue(i));
			param.free();
			if (computeStateJacobian){
			//Write jacobian wrt the whole state vector: (supported only for kwadratic cost function)
				IVector state = ((SimulationKwadraticCostFunction) fJacobian).getState();
				state.axpy(-1.0,currentState);
				state.scale(1.0d / this.fwdEps.getValue(i));
				Js[i] = state.clone();
				Results.putValue("StateJacobian_param" + i , state, state.getSize(), "Forward difference calculation of Jacobian matrix of the whole model state variables", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Instance);
			}

		}
		if (computeStateJacobian){
			this.Js = new Matrix(Js);
		}
		return new Matrix(J);
	}

	/**
	 * Run next step of the algorithm
	 */
	void next() {
		
		if (!this.moreToDo) {
			f.writeResults();
			return;
		}

		/*
		 * This function performs one outer iteration for the minimization of the 
		 * nonlinear least squares system, given by
		 * 
		 *      [ sigmaObs\f.evaluate(pars) ]   [ sigmaObs\obs  ]
		 *      [                           ] = [               ]
		 *      [        LPar\pars          ]   [  LPar\parInit ]
		 * 
		 * The system involves the following notations:
		 * 
		 * Solvable values:
		 *    p          - the parameters: the values which are the solution of this optimization process
		 *    
		 * The observation-prediction equations:
		 *    f.evaluate - the function which calculates the predictions from the parameters. These 
		 *                 predictions should correspond to the observations obs
		 *    obs        - the observations, which the predictions should be made to correspond to by
		 *                 choosing the parameters right
		 *    sigmaObs   - a diagonal matrix: standard deviations of observations, used to scale 
		 *                 the observation-prediction equations
		 *                 
		 * The background term (optional):
		 *    LPar       - sqrt-covariance matrix for parameters: scaling for background term 
		 *    parInit    - the values for the parameters which the parameters should be as close as possible to:
		 *                 a reasonable parameter set.
		 */
		
		// get data needed by the algorithm
		double costs    = this.fCurrent; // local copies
		IVector pars    = this.pCurrent;
		IVector preds   = this.predCurrent;
		int npars         = pars.getSize();     // number of elements for a parameter vector
		this.costFactor = this.f.getMultiplicationFactor();

		imain++;
		Results.putProgression("======================================================");
		Results.putProgression("Levenberg-Marquardt outer iteration no." + imain);
		Results.putProgression("======================================================");
		Results.putMessage("-----------------------------------------------------");
		Results.putValue("costs", new Vector(new double[]{costs}), 1, "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.OuterIteration);
		Results.putMessage("-----------------------------------------------------");

		IVector rhs_obs = obs.clone();
		rhs_obs.axpy(-1.0, preds);
		rhs_obs.pointwiseDivide(sigmaObs);
		Results.putMessage("RHS norm: "+rhs_obs.norm2());
		// Here rhs_obs is equal to r (scaled residual vector) in J'*R^-1*r

		/* Calculate the Jacobian A_obs scaled by observational error std and the diagonal matrix D
		 * in (J'*R^-1*J + lambda*D) pStep = J'*R^-1*r
		 * <--> (A_obs'*A_obs + lambda*D)*pStep = A_obs'*rhs_obs
		 * In the present of the weak constraint term (p-pInit)'Q^-1(p-pInit), the least square equation becomes
		 * (J'*R^-1*J + (Lq'*Lq)^-1 + lambda*D) pStep = J'*R^-1*r + Lpar^-1'*(pk-pInit), where Lpar'*Lpar=Q
		 * In this case D is defined as D=diag(J'*R^-1*J + (Lpar'*Lpar)^-1)
		 * In general the linear equation can be written as follows:
		 *      A'*A*pStep = rhs
		 * The linear problem is solved by the following steps:
		 *   - compute rhs = J'*R^-1*r or J'*R^-1*r + Lpar^-1'*(pk-pInit)
		 *   - prepare D
		 *   - prepare matrix A = concatenate(Lr^-1*J,Lpar^1,sqrt(lambda*D))
		 *   - solve pStepTemp in A'*pStepTemp=rhs
		 *   - solve pStep in A*pStep=pStepTemp */
		Matrix A_obs = CalculateGradSimu();
		Matrix A_newton; // i.e. Matrix A when no damping lambda*D is used, like in Gauss-Newton method
		Matrix A;
		IVector rhs; // right-hand-side for solve
		// Build Gauss-Newton least square matrices
		if (this.f.doAddBackgroundTerm()) {
			// additional term for background errors
			/*
			 * The background term is added. It is linear, and given in terms of pStep by
			 *
			 *    LPar \ (par - parInit) =
			 *          LPar \ (pars[0] - parInit) +  LPar \ P_matrix * pStep
			 *
			 * The matrix A_back and the vector rhs_back are now calculated so that
			 * the background term is written as
			 *
			 *    LPar \ (par - parInit) =  -rhs_back + A_obs * pStep
			 */
			IVector rhs_back = parInit.clone();
			rhs_back.axpy(-1.0,pars);
			LPar.rightSolve(rhs_back,rhs_back);

			Matrix A_back = (Matrix) LPar.asMatrix();
			A_back = A_back.inverse();

			/* The complete Least Square system for pStep is now composed: */
			A_newton = Matrix.concatVertical(A_obs,A_back);
			Vector rhs_obs_back = Vector.concatenate(rhs_obs,rhs_back);
			rhs = new Vector(A_newton.getNumberOfColumns());
			A_newton.leftMultiply(1.0,rhs_obs_back,0.0,rhs);
		} else {
			IVector rhs_obs_aug = new Vector(A_obs.getNumberOfColumns());
			A_obs.leftMultiply(1.0,rhs_obs,0.0,rhs_obs_aug);
			A_newton = A_obs;
			rhs = new Vector(A_obs.getNumberOfColumns());
			A_obs.leftMultiply(1.0,rhs_obs,0.0,rhs);
		}

		// Evaluate 'cost gradient'
		IVector costGradVector = rhs.clone();
		double costGradNorm = costGradVector.norm2();
		costGradNorm = costGradNorm*costGradNorm*this.costFactor;
		Results.putValue("costGradientNorm", costGradNorm, 1, "outer iteration "+imain, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.OuterIteration);

		// Beginning of inner loop
		int innerIter = 0;
		double scale_step = 1.0;
		IVector predTry, pTry;
		double costTry=0;
		double relErrorLinCost = 0;
		while (true) {
			// Build Levenberg-Marquardt least square matrices
			//   construct damping matrix D
			Matrix D;
			D = CalculateDampingMatrix(A_newton);
			D.scale(lambda);
			D = D.sqrt();
			//   construct A
			A = Matrix.concatVertical(A_newton,D);

			// Solve for pStep
			IVector pStep = new Vector(npars);
			IVector pStepTemp = new Vector(A.getNumberOfRows());
			A.leftSolve(rhs,pStepTemp);
			A.rightSolve(pStepTemp, pStep);

			Results.putProgression("Start search until improvement,");

			/* Reduce step size if any of the parameter steps is too large
			 * The scaling is with respect to the pStep, which means we prefer to stay
			 * inside or not too far outside the convex hull of the available parameter sets.
			 */
			double maxStep = infNorm(pStep);
			if (maxStep > this.maxStep) {
				Results.putMessage("% Reducing stepsize! Relative step was:"+pStep);
				pStep.scale(this.maxStep / maxStep);
			}

			/* Calculate the new parameter set */
			pTry = pars.clone();
			pTry.axpy(1.0,pStep);
			Results.putMessage("Next try p="+pTry.printString("   "));

			/* Evaluate predictions for this parameter */
			if (innerIter==0) {
				costTry = this.f.evaluate(pTry,"outer iteration "+imain);
				System.out.println("non-linear cost for new estimate "+costTry);
				relErrorLinCost = Math.abs((costTry-costGradNorm)/(costs-costGradNorm));
			} else {
				costTry = f.evaluate(pTry,"inner iteration "+innerIter);
			}
			predTry = f.getLastPredictions();

			if (costTry <= costs || innerIter > this.maxInnerIter) break;
			// Increase Levenberg parameter if costTry is larger than cost:
			this.lambda = this.lambda * this.nu;
			Results.putMessage("Increasing lambda, lambda="+this.lambda);
			innerIter++;

		} // end of inner loop

		// Decrease Levenberg parameter if costTry is smaller than cost:
		this.lambda = this.lambda / this.nu;
		Results.putMessage("Reducing lambda, lambda="+this.lambda);

		// compute analysis error covariance from approximate Hessian
		// Pest = (J'*R^-1*J + Q^-1)^-1 = (A_newton' * A_newton)^-1
		if(this.computeErrorEstimate){
			boolean transpose = true;

			Matrix Pinv = Matrix.mult(A_newton,A_newton,transpose,!transpose);
			Matrix Pest = Pinv.inverse();
			paramCovEst = Pest.clone();

			// standard deviations
			Vector std = (Vector) Pest.diag();
			std.sqrt();
			
			// now make this pretty for treeVectors
			IVector stdWithStructure = pTry.clone();
			stdWithStructure.setValues(std.getValues());
			Results.putMessage("Error estimate for this outer iteration");
			Results.putValue(	"parameterErrorEstimateStd", stdWithStructure, stdWithStructure.getSize(), 
								"outer iteration "+imain, IResultWriter.OutputLevel.Normal,
								IResultWriter.MessageType.OuterIteration);
			/*
			 * compute correlations
			 */
			Vector stdInverse = std.clone();
			stdInverse.invert();
			IMatrix stdOnDiag = Matrix.diag(stdInverse);
			IMatrix correlations = Matrix.mult(stdOnDiag, Matrix.mult(Pest,stdOnDiag));
            int correlationsSize = correlations.getNumberOfColumns() * correlations.getNumberOfRows();
			Results.putValue("parameterErrorCorrelations", correlations, correlationsSize,
                    "outer iteration "+imain, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.OuterIteration);

			/*
			 * If requested, compute covariance of the state vector
			 */
			if (computeStateJacobian){
				// stateCov = Js*parCov*Js', where Js is the model Jacobian, Js = [dx1/dp1 dx1/dp2 ... dx1/dp_nParam; dx2/dp1 dx2,dp2 ... dx2/dp_nParam; ... ; dx_nState/dp1 dx_nState/dp2 ... dx_nState/dp_nParam];
				Matrix stateCovarianceLeft = Matrix.mult(Js,Pest);
				stateCovEst = Matrix.mult(stateCovarianceLeft,Js,false,true);
				Vector stateSTD = (Vector) stateCovEst.diag();
				stateSTD.sqrt();
				Results.putValue("stateErrorStd", stateSTD, stateSTD.getSize(),
						"outer iteration "+imain, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.OuterIteration);

			}

		}

		//setup for next iteration
		preds = predTry;
		costs = costTry;
		pars = pTry;

		this.moreToDo = isThereMoreToDo(this.fCurrent,costs, costGradNorm, predTry, pars, innerIter);

		// Display final results:
		if (!this.moreToDo) this.f.writeResults();
//		if (!this.moreToDo) {
//			double costDummy = f.evaluate(pTry,"final step");
//			Matrix last_A_newton = buildGaussNewtonMatricesAndCostGrad();
//			computeErrorCovariance(pars,last_A_newton);
//			this.f.writeResults();
//		}

		// store results to object
		this.pCurrent = pars;
		this.predCurrent = preds;
		this.fCurrent = costs;
		this.costGrad = costGradNorm;

	}

	public IMatrix getStateCovariance(){
		if (!computeStateJacobian) {
			return null;
		}
		return this.stateCovEst.clone();
	}

	private void computeErrorCovariance(IVector pTry, Matrix A_newton){
//		Matrix A_newton; // i.e. Matrix A when no damping lambda*D is used, like in Gauss-Newton method

		// compute analysis error covariance from approximate Hessian
		// Pest = (J'*R^-1*J + Q^-1)^-1 = (A_newton' * A_newton)^-1
		if(this.computeErrorEstimate){
			boolean transpose = true;

			Matrix Pinv = Matrix.mult(A_newton,A_newton,transpose,!transpose);
			Matrix Pest = Pinv.inverse();

			/* Singular value decomposition of Pinv:
			 *  Pinv =: svd[2] * svd[1] * svd[0]' */
			Matrix svd[] = Pinv.svd();

			// svd[1] = inv(svd[1])
			int np = A_newton.getNumberOfColumns();
			for(int i=0;i<np;i++){
				svd[1].setValue(i, i, 1.0/svd[1].getValue(i,i));
			}

			// standard deviations
			Vector std = (Vector) Pest.diag();
			std.sqrt();

			// now make this pretty for treeVectors
			IVector stdWithStructure = pTry.clone();
			stdWithStructure.setValues(std.getValues());
			Results.putMessage("Error estimate for this outer iteration");
			Results.putValue(	"parameterErrorEstimateStd", stdWithStructure, stdWithStructure.getSize(),
					"outer iteration "+imain, IResultWriter.OutputLevel.Normal,
					IResultWriter.MessageType.OuterIteration);
			/*
			 * compute correlations
			 */
			Vector stdInverse = std.clone();
			stdInverse.invert();
			IMatrix stdOnDiag = Matrix.diag(stdInverse);
			IMatrix correlations = Matrix.mult(stdOnDiag, Matrix.mult(Pest,stdOnDiag));
			int correlationsSize = correlations.getNumberOfColumns() * correlations.getNumberOfRows();
			Results.putValue("parameterErrorCorrelations", correlations, correlationsSize,
					"outer iteration "+imain, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.OuterIteration);

			/*
			 * If requested, compute covariance of the state vector
			 */
			if (computeStateJacobian){
				// stateCov = Js*parCov*Js', where Js is the model Jacobian, Js = [dx1/dp1 dx1/dp2 ... dx1/dp_nParam; dx2/dp1 dx2,dp2 ... dx2/dp_nParam; ... ; dx_nState/dp1 dx_nState/dp2 ... dx_nState/dp_nParam];
				Matrix stateCovarianceLeft = Matrix.mult(Js,Pest);
				Matrix stateCovariance = Matrix.mult(stateCovarianceLeft,Js,false,true);
				Vector stateSTD = (Vector) stateCovariance.diag();
				stateSTD.sqrt();
				Results.putValue("stateErrorStd", stateSTD, stateSTD.getSize(),
						"outer iteration "+imain, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.OuterIteration);
			}

		}
	}

	private Matrix buildGaussNewtonMatricesAndCostGrad(){
		IVector pars    = this.pCurrent;
		IVector preds   = this.predCurrent;
		Matrix A_newton;
		Matrix A_obs = CalculateGradSimu();
		IVector rhs; // right-hand-side for solve
		IVector rhs_obs = obs.clone();
		rhs_obs.axpy(-1.0, preds);
		rhs_obs.pointwiseDivide(sigmaObs);
		Results.putMessage("RHS norm: "+rhs_obs.norm2());

		// Build Gauss-Newton least square matrices
		if (this.f.doAddBackgroundTerm()) {
			// additional term for background errors
			/*
			 * The background term is added. It is linear, and given in terms of pStep by
			 *
			 *    LPar \ (par - parInit) =
			 *          LPar \ (pars[0] - parInit) +  LPar \ P_matrix * pStep
			 *
			 * The matrix A_back and the vector rhs_back are now calculated so that
			 * the background term is written as
			 *
			 *    LPar \ (par - parInit) =  -rhs_back + A_obs * pStep
			 */
			IVector rhs_back = parInit.clone();
			rhs_back.axpy(-1.0,pars);
			LPar.rightSolve(rhs_back,rhs_back);

			Matrix A_back = (Matrix) LPar.asMatrix();
			A_back = A_back.inverse();

			/* The complete Least Square system for pStep is now composed: */
			A_newton = Matrix.concatVertical(A_obs,A_back);
			Vector rhs_obs_back = Vector.concatenate(rhs_obs,rhs_back);
			rhs = new Vector(A_newton.getNumberOfColumns());
			A_newton.leftMultiply(1.0,rhs_obs_back,0.0,rhs);
		} else {
			IVector rhs_obs_aug = new Vector(A_obs.getNumberOfColumns());
			A_obs.leftMultiply(1.0,rhs_obs,0.0,rhs_obs_aug);
			A_newton = A_obs;
			rhs = new Vector(A_obs.getNumberOfColumns());
			A_obs.leftMultiply(1.0,rhs_obs,0.0,rhs);
		}

		// Evaluate 'cost gradient'
		IVector costGradVector = rhs.clone();
		double costGradNorm = costGradVector.norm2();
		costGradNorm = costGradNorm*costGradNorm*this.costFactor;
		Results.putValue("costGradientNorm", costGradNorm, 1, "outer iteration "+imain, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.OuterIteration);

		return A_newton;

	}


	private Matrix CalculateDampingMatrix(Matrix J) {
		int nColumn = J.getNumberOfColumns();
		Vector diagVector = new Vector(nColumn);
		IVector[] JVector = J.asVectorArray();
		for (int i=0; i<nColumn; i++){
			double thisElement = JVector[i].dotProduct(JVector[i]);
			diagVector.setValue(i,thisElement);
		}
		Matrix D = Matrix.diag(diagVector);
		return D;
	}

	// ============================================================================================
	// only supporting routines below this line



	/**
	 * Sort an array and return the result as an array of indices
	 *
	 * @param values : to be sorted
	 * @return indices : first value points to smallest value and last one to largest
	 */
	private int[] sortedIndex(double[] values) {

		class indexValuePair {
			int index;
			double value;

			public indexValuePair(int index, double value) {
				this.index = index;
				this.value = value;
			}
		}
		class ValueComparator implements java.util.Comparator<indexValuePair> {
			public int compare(indexValuePair o1, indexValuePair o2) {
				return new Double(o1.value).compareTo(o2.value);
			}
		}

		int[] result = new int[values.length];
		java.util.ArrayList<indexValuePair> sortedValues = new java.util.ArrayList<indexValuePair>();
		for (int i = 0; i < values.length; i++) {
			sortedValues.add(new indexValuePair(i, values[i]));
		}
		ValueComparator vc = new ValueComparator();
		java.util.Collections.sort(sortedValues, vc);
		int j = 0;
		for (java.util.Iterator<indexValuePair> it = sortedValues.iterator(); it.hasNext();) {
			result[j] = it.next().index;
			j++;
		}
		return result;
	}

	/**
	 * Use indices from sorting to sort an additions array of Vectors
	 * Vectors are not copied, only the references are switched
	 * @param vs Vectors to be sorted
	 * @param index indices from sorting
	 * @return sorted vectors
	 */
	private IVector[] applyIndexToVectors(IVector[] vs, int[] index) {
		IVector[] result = new IVector[vs.length];
		for (int i = 0; i < vs.length; i++) {
			result[i] = vs[index[i]];
		}
		return result;
	}

	/**
	 * Use indices from sorting to sort an additions array of doubles
	 * Sorts on a copy.
	 * @param cs doubles to be sorted
	 * @param index indices from sorting
	 * @return sorted doubles
	 */
	private double[] applyIndexToDoubles(double[] cs, int[] index) {
		double[] result = new double[cs.length];
		for (int i = 0; i < cs.length; i++) {
			result[i] = cs[index[i]];
		}
		return result;
	}

	private double infNorm(IVector v) {
		double result = 0.0;
		int n = v.getSize();
		double temp;
		for (int i = 0; i < n; i++) {
			temp = Math.abs(v.getValue(i));
			if (temp > result) { result = temp; }
		}
		return result;
	}

	public IVector getPredCurrent() {
		IVector result = this.predCurrent.clone();
		return result;
	}
}
