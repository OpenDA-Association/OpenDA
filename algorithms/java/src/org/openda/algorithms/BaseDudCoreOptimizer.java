/* MOD_V2.0
* Copyright (c) 2013 OpenDA Association
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
import org.openda.utils.*;

import java.util.ArrayList;
import java.util.List;

public abstract class BaseDudCoreOptimizer {
	final double EPSILON = 1E-14;
	// fields of this class
	protected IVector pCurrent[] = null;    // parametervalues under consideration
	protected IVector predCurrent[] = null; // predictions for each parameter vector
	protected double fCurrent[] = null;     // costs for each
	protected LeastSquaresCostFunction f = null;

	protected int number_of_stored_runs;
	protected int number_of_evaluations;  	//number of evaluations (without initial evaluation)
	protected double initialStep = 1.0; 	// scaling for initial stepsize
	protected double costFactor;            // multiplication factor for cost

	// settings
	public int maxit = 3;                // maximum number of outer iterations
	public int maxInnerIter = 6;         // maximum number of inner iterations
	public double innerScaleFac = 0.5;   // scaling factor for bactracking
	public int minInnerNegativeLook = 3; // when to start looking into the negative direction
	public double maxStep = 10.0;        // maximum relative step size (compared to the spread of the parameters)
	public double absTol = 0.01;         // absolute criterion
	public double relTol = 0.01;         // relative criterion

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
	protected IVector b_lower = null;
	protected IVector b_upper = null;

	// stopping
	private boolean moreToDo = true; // is this optimization finished
	private int imain = 0;           // main iterations done

	protected IMatrix sqrtCovEst = null;   // estimate of square root of error covariance
	public double relTolLinCost = 0.01;  // compares linearity error to improvement
	public boolean computeErrorEstimate=true;
	public boolean add_constraints = false;
	public String lower = null;
	public String upper = null;

	protected int numberOfSearchDirections() {
		IStochVector parameterUncertainty = f.getParameterUncertainty();
		ISqrtCovariance sqrtCovariance = parameterUncertainty.getSqrtCovariance();
		IVector[] vectorArray = sqrtCovariance.asVectorArray(); 
		return vectorArray.length; 
	}

	protected abstract double InitialSearchStep(int i, int j);
	
	/**
	 * Initialization for Dud minimization - used for restarting the optimization process
	 * @param pars : parameters as Vector at each node of Dud
	 * @param costValues : corresponding cost values, so there is no need to recompute them (saves time)
	 * @param predictions : corresponding predictor values
	 */
	public void initialize(IVector pars[], double costValues[], IVector[] predictions){

		this.pCurrent = new IVector[number_of_stored_runs];
		this.fCurrent = new double[number_of_stored_runs];
		this.predCurrent = new IVector[number_of_stored_runs];
		for (int i = 0; i < pars.length; i++) {
			this.pCurrent[i] = pars[i].clone();
			this.fCurrent[i] = costValues[i];
			this.predCurrent[i] = predictions[i].clone();
		}

		// Initialize the cost functions with invalid values (so they'll be at the end when sorted)
		for (int i = pars.length; i < number_of_stored_runs ; i++) { this.fCurrent[i] = 1.0E30 ;}

		f.evaluate(pars[number_of_evaluations], "initialization node " + number_of_evaluations);

		number_of_evaluations = pars.length-1;
		IStochVector parameterUncertainty = f.getParameterUncertainty();
		this.LPar = parameterUncertainty.getSqrtCovariance();
		this.parInit = parameterUncertainty.getExpectations();
		this.svObs = f.getObservationUncertainty();
		sortByCostAndCashObservationData();
		
		// Initialize for constraints
		if (add_constraints) {
			/* inequality constraints:
			C_lower * x => b_lower
			C_upper * x <= b_upper*/
			int npars = pars[0].getSize();

			if (lower == null){
				b_lower = new Vector(0);
			}

			/*pars[new] = pars[0] + P_matrix * pStep >= b_lower , so
			P_matrix * pStep >= b_lower - pars[0]:= b_lower*/
			else{
				b_lower = new Vector(lower);
				if (npars != b_lower.getSize()) {
					throw new RuntimeException(
							"Dud initializing algorithm: the number of constraints should be equal to the number of parameters.");
				}

				if (b_lower.getSize()==1) {
					double value = b_lower.getValue(0);
					b_lower = new Vector(npars);
					for (int i = 0; i < npars; i++) {
						b_lower.setValue(i,value);
					}
				}
			}

			if (upper == null){
				b_upper = new Vector(0);
			}

			/*pars[new] = pars[0] + P_matrix * pStep =< b_upper , so
			-P_matrix * pStep >= pars[0] - b_upper:= b_upper */
			else{
				b_upper = new Vector(upper);
				if (npars != b_lower.getSize()) {
					throw new RuntimeException(
							"Dud initializing algorithm: the number of constraints should be equal to the number of parameters.");
				}
				if (b_upper.getSize()==1) {
					double value = b_upper.getValue(0);
					b_upper = new Vector(npars);
					for (int i = 0; i < npars; i++) {
						b_upper.setValue(i,value);
					}
				}
			}
		}
	}

	public void initialize(IVector pInit){

		// Initialize for constraints
		int npars = pInit.getSize();
		if(add_constraints){

			/*inequality constraints:
			C_lower * x => b_lower
			C_upper * x <= b_upper*/

			if (lower == null){
				b_lower = new Vector(0);
			}

			/*pars[new] = pars[0] + P_matrix * pStep >= b_lower , so
			P_matrix * pStep >= b_lower - pars[0]:= b_lower*/
			else{
				b_lower = new Vector(lower);
				if (npars != b_lower.getSize()) {
					throw new RuntimeException(
							"Dud initializing algorithm: the number of constraints should be equal to the number of parameters.");
				}
				if(b_lower.getSize()==1){
					double value = b_lower.getValue(0);
					b_lower = new Vector(npars);
					for(int i = 0; i < npars; i++){
						b_lower.setValue(i,value);
					}
				}

				// check if pInit satisfies constraints; otherwise, set it equal to constraint
				for (int i=0; i<npars; i++){
					if (pInit.getValue(i)<b_lower.getValue(i)){
						Results.putProgression("WARN: Initial parameter ["+i+"]="+pInit.getValue(i)+" is smaller than lowerbound["+i+"]="+b_lower.getValue(i)+". Parameter is set equal to lowerbound.");
						pInit.setValue(i, b_lower.getValue(i));
					}
				}

			}

			if(upper == null){
				b_upper = new Vector(0);
			}

			/*pars[new] = pars[0] + P_matrix * pStep =< b_upper , so
			-P_matrix * pStep >= pars[0] - b_upper:= b_upper */
			else{
				b_upper = new Vector(upper);
				if (npars != b_upper.getSize()) {
					throw new RuntimeException(
							"Dud initializing algorithm: the number of constraints should be equal to the number of parameters.");
				}
				if(b_upper.getSize()==1){
					double value = b_upper.getValue(0);
					b_upper = new Vector(npars);
					for(int i = 0; i < npars; i++){
						b_upper.setValue(i,value);
					}
				}
				// check if pInit satisfies constraints; otherwise, set it equal to constraint
				for (int i=0; i<npars; i++){
					if (pInit.getValue(i)>b_upper.getValue(i)){
						Results.putProgression("WARN: Initial parameter ["+i+"]="+pInit.getValue(i)+" is bigger than upperbound["+i+"]="+b_upper.getValue(i)+". Parameter is set equal to upperbound.");
						pInit.setValue(i, b_upper.getValue(i));
					}
				}
			}
		}

		IStochVector parameterUncertainty = f.getParameterUncertainty();
		this.LPar = parameterUncertainty.getSqrtCovariance();
		this.parInit = parameterUncertainty.getExpectations();

		IVector[] searchDirections = this.LPar.asVectorArray();
		int m = searchDirections.length; // Number of independent directions in uncertainty; often equal to number of pars

		// generate initial perturbations
		this.pCurrent = new IVector[number_of_stored_runs];
		this.predCurrent = new IVector[number_of_stored_runs];
		this.fCurrent = new double[number_of_stored_runs];

		// Initialize the cost functions with invalid values (so they'll be at the end when sorted)
		for (int i = 1;	i < number_of_stored_runs ; i++) { this.fCurrent[i] = 1.0E30 ;}

		// Calculate a number of parameter sets to be evaluated. 
		this.pCurrent[0] = pInit.clone();
		for (int i = 0; i < number_of_evaluations; i++) {
			pCurrent[i+1] = pInit.clone();
			for (int j = 0; j < m; j++) {
				pCurrent[i+1].axpy(initialStep * InitialSearchStep(i,j), searchDirections[j]);
				// Check if initial parameters satisfies constraints.
				// pInit is assumed to satisfy the constraints. Here the checks are performed only to the other initial pars.
				if (add_constraints){
					IVector pDiff;
					double eps_zero = 10E-5;
					for (int k = 0; k < pInit.getSize(); k++){
						if (lower!=null && pCurrent[i+1].getValue(k)<b_lower.getValue(k)){
							pCurrent[i+1].setValue(k,b_lower.getValue(k));
						}
						if (upper!=null && pCurrent[i+1].getValue(k)>b_upper.getValue(k)){
							pCurrent[i+1].setValue(k,b_upper.getValue(k));
							// check if pCurrent doesn't collapse to pInit, that can happen in case of pInit == upper_bound
							pDiff = pCurrent[i+1].clone();
							pDiff.axpy(-1.0,pInit);
							if (pDiff.norm2()<eps_zero){
								if (lower!=null){
									// set the param to the corresponding lower bound (it can actually be any value
									// between the lower and upper bounds)
									pCurrent[i+1].setValue(k,b_lower.getValue(k));
								} else {
									// use negative searchDirections
									pCurrent[i+1].setValues(pInit.getValues());
									pCurrent[i+1].axpy(-1.0 * initialStep * InitialSearchStep(i,j), searchDirections[j]);
								}
							}
						}
					}
				}
			}
		}
		
		// evaluate for these parameters
		if(this.f.getTryParallel()){ 
			//start models in parallel by splitting loop in two loops
			LeastSquaresCostFunction costFunctions[] = new LeastSquaresCostFunction[number_of_evaluations+1];
			
			for(int i=0;i<=number_of_evaluations;i++){ //start all the evaluations in parallel
				Results.putMessage("Prepare for evaluating with parameters "+pCurrent[i].printString(" "));
				if (i>0) { costFunctions[i]=f.clone();}
				else     { costFunctions[i]=f;} //no need to copy first one
				costFunctions[i].prepare(pCurrent[i]);
			}
			
			for (int i=0;i<=number_of_evaluations;i++) { //collect the results
				Results.putMessage("Evaluating with parameters "+pCurrent[i].printString(" "));
				fCurrent[i] = costFunctions[i].evaluate(pCurrent[i],"initialization node "+i);
				predCurrent[i] = costFunctions[i].getLastPredictions();
			}
			
		} else { //this loop uses a sequential blocking call to each model
			
			for (int i=0;i<=number_of_evaluations;i++) {
				Results.putMessage("Evaluating with parameters "+pCurrent[i].printString(" "));
				fCurrent[i] = f.evaluate(pCurrent[i],"initialization node "+i);
				predCurrent[i] = f.getLastPredictions();
			}
		}
		this.svObs = f.getObservationUncertainty();
		sortByCostAndCashObservationData();
		
	}


	protected void sortByCostAndCashObservationData(){

		// Sort parameters tried so far by cost
		this.costFactor = this.f.getMultiplicationFactor();
		double[] costs = this.fCurrent;
		IVector[] pars = this.pCurrent;
		IVector[] preds = this.predCurrent;
		int[] isort=this.sortedIndex(costs);
		double[] costs_sort = this.applyIndexToDoubles(costs, isort);
		IVector[] pars_sort=this.applyIndexToVectors(pars, isort);
		IVector[] preds_sort=this.applyIndexToVectors(preds, isort);
		this.pCurrent  = pars_sort;
		this.fCurrent = costs_sort;
		this.predCurrent = preds_sort;

		// cache some data from observations
		this.obs = this.svObs.getExpectations();
		if (this.svObs.hasCorrelatedElements()) {
			throw new RuntimeException(
					"Dud optimization not implemented for correlated observations yet.");
		}
		this.sigmaObs = this.svObs.getStandardDeviations();
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
	 * Get parameters for each point of the Dud. The current Dud equals the initial
	 * Dud if requested before optimization and final 
	 * Dud if f.optimize() has been called.
	 *
	 * @return Vector for parameters at each nod of the Dud vectors
	 */
	public IVector[] getCurrentValues() {
		IVector result[] = new IVector[number_of_evaluations+1];
		for (int i = 0; i <= number_of_evaluations; i++) {
				result[i] = this.pCurrent[i].clone();
		}
		return result;
	}

	/**
	 * Get the current cost values corresponding to the nodes of the current
	 * Dud.
	 *
	 * @return Cost at each node as array of doubles
	 */
	public double[] getCurrentCosts() {
		double result[] = new double[number_of_evaluations+1];
		System.arraycopy(this.fCurrent, 0, result, 0, number_of_evaluations + 1);
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
	public IMatrix getSqrtCovariance(){
		IMatrix result = null;
		if(this.sqrtCovEst!=null){
			result=this.sqrtCovEst.clone();
		}
		return result;
	}
	
	
	boolean isThereMoreToDo(double costs[], double relErrorLinCost, IVector predTry, IVector pars[], int innerIter) {
		/* This function checks using all the criteria whether another iteration is still needed. */
		double diff = costs[1] - costs[0];
		double relDiff=diff/Math.abs(costs[0]);
		Results.putMessage("stop criterion 1, imain                            > maxit :\t " + imain   + " < " + maxit);
		Results.putMessage("stop criterion 2, |new - previous cost|            < abstol:\t " + diff    + " > " + absTol);
		Results.putMessage("stop criterion 3, |new - previous cost|/|new cost| < reltol:\t " + relDiff + " > " + relTol);
		Results.putMessage("stop criterion 4, linearized cost relative error: "+relErrorLinCost+" < "+relTolLinCost);

		boolean moreToDo = 
				imain < this.maxit  
				&& diff > this.absTol
				&& diff > relTol * Math.abs(costs[0])
				&& relErrorLinCost>relTolLinCost;

		// additional stop criteria:
		if (stopCriteria.size()>0) {
			IVector residual = obs.clone();
			residual.axpy(-1.0,predTry);
			boolean isStop;
			for (int i=0; i<stopCriteria.size(); i++){
				IStopCriterion object = stopCriteria.get(i);
				double threshold = stopCriteriaThreshold.get(i);
				if (obsDescr!=null) {
					isStop = object.checkForStop(pars[0],residual,obsDescr,costs[0],threshold);
				} else {
					isStop = object.checkForStop(pars[0],residual,costs[0],threshold);
				}
				Results.putMessage(object.toString());
				this.moreToDo = this.moreToDo & !isStop;
			}
		}
		
		// do not stop on a backtracking iteration
		if (innerIter>0 && (imain < maxit)) moreToDo = true;

		return moreToDo;
	}
	
	protected abstract Matrix CalculateGradSimu();

	protected abstract Matrix CalculateParsMatrix();

	protected abstract Matrix BackgroundMatrix();


	/**
	 * Solve the equivalent minimization problem with inequality constraints
	 * (more details are found in the documentation, paragraph on Dud with Constraints).
	 * @param G                 Matrix A'*A
	 * @param A_transpose_x_rhs Vector A'*Delta_x
	 * @param npars             number of parameters in calibration process
	 * @param pars              Vector pars in relation pars = pars[0] + P_matrix * pStep
	 * @param P_matrix          P_matrix in relation pars = pars[0] + P_matrix * pStep
	 * @return    				resulting Vector pstep in relation pars = pars[0] + P_matrix * pStep for problem with inequality constraints.
	 */
	public IVector parameterConstraints(Matrix G, IVector A_transpose_x_rhs, int npars, IVector pars, Matrix P_matrix){
		
		IVector b_low = b_lower.clone();
		IVector b_up = b_upper.clone();
		IVector c = A_transpose_x_rhs.clone();  // c = A'*rhs
		
		// Scale G and c for computational reasons
		double scale = 0;
		IVector scale_tool = G.diag();
		for (int i = 0; i < scale_tool.getSize(); i++){
			scale = scale + scale_tool.getValue(i);
		}
		scale = scale_tool.getSize()/scale;
		c.scale(-scale);
		G.scale(scale);

		IVector x = new Vector(npars);		// most optimal set of parameters for linearized problem, a.k.a. pStep
		
		int n_equal = 1; 					// Number of equality constraints
		int n_inequal;						// Number of inequality constraints
		
		// equality constraints: C_equal * x = b_equal
		Matrix C_equal = new Matrix(n_equal,npars);	
		Vector b_equal = new Vector(n_equal);
		Vector flag_equal = new Vector(n_equal);
		Vector flag_param_equal = new Vector(n_equal);

		/*pars[new] = pars[0] + P_matrix * pStep >= b_lower , so
		P_matrix * pStep >= b_lower - pars[0]:= b_lower*/
		
		Matrix C_low;
		IVector flag_low = b_low.clone();
		IVector flag_param_low = b_low.clone();
		if (b_low.getSize() > 0 ) {
			b_low.axpy(-1,pars);
			C_low = P_matrix.clone();
			flag_low.setConstant(-1.0);
			for (int i=0; i<flag_param_low.getSize(); i++){
				flag_param_low.setValue(i, i);
			}
		}
		else {
			C_low = new Matrix(0,0);
		}
		
		/*pars[new] = pars[0] + P_matrix * pStep =< b_upper , so
		-P_matrix * pStep >= pars[0] - b_upper:= b_upper */
		
		Matrix C_up;
		IVector flag_up = b_up.clone();
		IVector flag_param_up = b_up.clone();
		if (b_up.getSize() > 0 ) {
			b_up.axpy(-1,pars);
			b_up.scale(-1);
			C_up = P_matrix.clone();
			C_up.scale(-1.0);
			flag_up.setConstant(1.0);
			for (int i=0; i<flag_param_up.getSize(); i++){
				flag_param_up.setValue(i, i);
			}
		}
		else {
			C_up = new Matrix(0,0);
			C_up.scale(-1);
		}
		
		n_inequal = b_low.getSize()+b_up.getSize();			 
		
		// inequality constraints: C_inequal * x => b_inequal
		Matrix C_inequal = Matrix.concatVertical(C_low,C_up);
		Vector b_inequal = Vector.concatenate(b_low, b_up);
		Vector flag_inequal = Vector.concatenate(flag_low, flag_up);
		Vector flag_param_inequal = Vector.concatenate(flag_param_low, flag_param_up);
		
		int n_sum = n_equal + n_inequal;	            // total number of constraints
		
		Matrix C_working_set = C_equal.clone();		    // sub matrix containing constraints in working set
		Vector b_working_set = b_equal.clone();	        // sub vector containing constraints in working set
		
		Matrix C_not_working_set = C_inequal.clone();	// sub matrix containing constraints not in working set
		Vector b_not_working_set = b_inequal.clone();	// sub matrix containing constraints not in working set
		
		int n_working_set = b_working_set.getSize();	// amount of constraints in the working set (must be greater or equal to n_equal)
		int n_not_working_set = n_sum - n_working_set;  // amount of constraints not in the working set
		
		Vector x_step;                  			    // step in most optimal direction
		Vector Lagrange;							    // Lagrange multipliers
		Vector v;									    // [x_step ; Lagrange]

		while (true){

			IVector Gx_c = c.clone(); 		//G*x +c
			G.rightMultiply(1,x,1,Gx_c);

			Matrix zero_mat = new Matrix(n_working_set,n_working_set,0.0);
			TreeVector temp_vec = new TreeVector("temp_vec");
			TreeVector subTreeVector_zero = new TreeVector("zero_vec",new Vector(n_working_set));
			TreeVector subTreeVector_gx_c = new TreeVector("Gx_c",Gx_c);

			Matrix temp1 = Matrix.concatHorizontal(G,C_working_set.simpletranspose()); 
			Matrix temp2 = Matrix.concatHorizontal(C_working_set,zero_mat);	
			Matrix temp_mat = Matrix.concatVertical(temp1,temp2);		// Matrix to solve v

			temp_vec.addChild(subTreeVector_gx_c);
			temp_vec.addChild(subTreeVector_zero);
			v = new Vector(temp_mat.getNumberOfColumns());
			temp_mat.rightSolve(temp_vec,v);  						// v is solved

			// vector v is separated into x_step and Lagrange
			x_step = v.get_selection(0,npars-1);

			x_step.scale(-1);
			Lagrange = v.get_selection(npars, v.getSize()-1);
			
			double eps1 = 1e-4; // JS: should we make this configurable? for checking if x_step==0.
			double eps2 = 1e-5; // JS: should we make this configurable? for checking if lagrange<0

			// If x_step = 0...
			if(x_step.norm2() < eps1){

				// find index and value of the smallest Lagrange multiplier
				int low_index = -1;
				double low_value = Double.NaN;

				for ( int ix = 0 ; ix < Lagrange.getSize() ; ix++) {
					if (low_index == -1 || Lagrange.getValue(ix) < low_value) {
						low_index = ix;
						low_value = Lagrange.getValue(ix);
					}
				}

				// If one ore more multipliers is < 0, the constraint corresponding to the smallest multiplier is taken out of the working set. 
				// then, redo algorithm

				if (low_value < -eps2) {
					int index = low_index;

					// removes constraint from working_set
					Matrix row = new Matrix(C_working_set.getMatrixSelection(index, index, 0 , npars-1));
					C_not_working_set = Matrix.concatVertical(C_not_working_set,row);
					C_working_set = C_working_set.remove_row(index);
					
					b_not_working_set = Vector.concatenate(b_not_working_set,b_working_set.get_selection(index,index));
					b_working_set = b_working_set.remove_entry(index);

					//MVL
					//flag_inequal = flag_inequal.remove_entry(index);
					flag_inequal = Vector.concatenate(flag_inequal,flag_equal.get_selection(index,index));
					flag_equal = flag_equal.remove_entry(index);

					//flag_param_inequal = flag_param_inequal.remove_entry(index);
					flag_param_inequal = Vector.concatenate(flag_param_inequal,flag_param_equal.get_selection(index,index));
					flag_param_equal = flag_param_equal.remove_entry(index);

					n_working_set = n_working_set -1;
					n_not_working_set = n_not_working_set + 1;
					
				}
				
				// If they are all > 0, finished!!
				else {
					return x;
				}	
			}
			
			// if x_step > 0...
			else {
				
				int index = -1;
				double tool_alpha;
				IVector check = new Vector(1);
				
				double alpha = 1;  // allowed length of the step in direction of x_step
				
				// compute alpha
				for (int i = 0; i< n_not_working_set; i++){

					// ith constraint not in working set
					Matrix tool = new Matrix(C_not_working_set.getMatrixSelection(i,i,0,npars-1));
					// x_step filled in, in the ith constraint: check = tool*x_step
					tool.rightMultiply(1, x_step, 0, check);

					if(check.getValue(0) < b_not_working_set.getValue(i)){

						double check_d = check.getValue(0);
						tool_alpha = Math.abs(b_not_working_set.getValue(i)/check_d);
						
						// checks which alpha is the smallest and remembers its index 
						if (tool_alpha < alpha){
							alpha = tool_alpha;
							index = i;
						}
					}
				}

				x_step.scale(alpha);
				x.axpy(1,x_step);

				// if alpha < 1 add the constraint with lowest alpha to working set and redo the algorithm
				if (alpha < 1){

					// we should update the constraints here because x is already updated
					// original lower bound = b_lower - pars
					//         at this stage the best parameter is equal to parsNew = pars + P_matrix * x
					//         hence, the new lower bound is equal to
					//             lower bound = b_lower - parsNew
					//     The same applies to upper bound:
					//             upper bound = parsNew - b_upper
					IVector xTry = x.clone();
					IVector pTry = pars.clone();
					P_matrix.rightMultiply(1.0, xTry, 1.0, pTry);
					for (int i=0; i<b_not_working_set.getSize(); i++){
						if (flag_inequal.getValue(i)<0){
							// low bound
							int indexParam = (int) flag_param_inequal.getValue(i);
							double newBound = b_lower.getValue(indexParam) - pTry.getValue(indexParam);
							b_not_working_set.setValue(i,newBound);
						} else {
							// upper bound
							int indexParam = (int) flag_param_inequal.getValue(i);
							double newBound = pTry.getValue(indexParam) - b_upper.getValue(indexParam);
							b_not_working_set.setValue(i,newBound);
						}
					}

					Matrix help = new Matrix(C_not_working_set.getMatrixSelection(index,index,0,npars-1));
					C_working_set = Matrix.concatVertical(C_working_set,help);
					b_working_set = Vector.concatenate(b_working_set, b_not_working_set.get_selection(index, index));

					C_not_working_set = C_not_working_set.remove_row(index);
					b_not_working_set = b_not_working_set.remove_entry(index);

					flag_equal = Vector.concatenate(flag_equal,flag_inequal.get_selection(index,index));
					flag_inequal = flag_inequal.remove_entry(index);

					flag_param_equal = Vector.concatenate(flag_param_equal,flag_param_inequal.get_selection(index,index));
					flag_param_inequal = flag_param_inequal.remove_entry(index);

					n_working_set = n_working_set + 1;
					n_not_working_set = n_not_working_set - 1;

				}

			}
		}
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
		double[] costs    = this.fCurrent; // local copies
		IVector[] pars    = this.pCurrent;
		IVector[] preds   = this.predCurrent;
		int ndirs         = number_of_evaluations; // number of search directions
		int npars         = pars[0].getSize();     // number of elements for a parameter vector
		
		imain++;
		Results.putProgression("======================================================");
		Results.putProgression("DUD outer iteration no." + imain);
		Results.putProgression("======================================================");
		Results.putMessage("-----------------------------------------------------");
		Results.putValue("costs", new Vector(costs), costs.length, "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.OuterIteration);
		Results.putMessage("-----------------------------------------------------");

		/* 
		 * Build least squares problem.
		 * The best iterand we have is pars[0] (with predictions preds[0] = f.evaluate(pars[0]))
		 * 
		 * A linear system will be set up for pStep, which from which the next iterand pars can be 
		 * calulated as
		 * 
		 *    pars = pars[0] + P_matrix * pStep
		 * or (when backtracking)
		 *    pars = pars[0] + scale_step * P_matrix * pStep
		 */
		Matrix P_matrix = CalculateParsMatrix();

		/* The linearization of the observation-prediction equations wrt pStep is
		 * 
		 *    sigmaObs \ (f.evaluate(pars) - obs) =.= 
		 *    
		 *    sigmaObs \ (preds[0]-obs) + 
		 *    sigmaObs \ (preds[1]-preds[0], ... , preds[n]-preds[0]) * pStep.
		 *    
		 * The matrix A_obs and the vector rhs_obs are now calculated, so that this linearization 
		 * can be written as
		 * 
		 *    sigmaObs \ (f.evaluate(pars) - obs) =.= -rhs_obs + A_obs * pStep
		 */		
		IVector rhs_obs = obs.clone(); 
		rhs_obs.axpy(-1.0, preds[0]);
		rhs_obs.pointwiseDivide(sigmaObs);
		Results.putMessage("RHS norm: "+rhs_obs.norm2());
		
		/* Calculate the gradient A_obs using the selected scaling */
		Matrix A_obs = CalculateGradSimu();
		
		Matrix A;
		IVector rhs; // right-hand-side for solve

		// additional term for background errors
		if (this.f.doAddBackgroundTerm()) {
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
			rhs_back.axpy(-1.0,pars[0]);
			LPar.rightSolve(rhs_back,rhs_back);

			Matrix A_back = BackgroundMatrix(); 

			/* The complete Least Square system for pStep is now composed: */
			A = Matrix.concatVertical(A_obs,A_back);
			rhs = Vector.concatenate(rhs_obs,rhs_back);
		} else {
			A = A_obs; 
			rhs = rhs_obs;
		}
	
		// solve for next step (relative)
		IVector pStep = new Vector(npars);
		
		if (!add_constraints){
			A.rightSolve(rhs, pStep);
		}
		else{
			// If gradient is zero, stop the algorithm
			double eps_zero = 0.00001d;
			if (Math.abs(A.norm())<eps_zero){
				Results.putMessage("% WARNING: gradient is approximately zero. Iteration is stopped. ");
				this.moreToDo = false;
				return;
			}

			// G is A'*A
			Matrix G = A.simpletranspose().mult(A);
			
			// A_transpose_x_rhs = A'*rhs
			IVector A_transpose_x_rhs = new Vector(npars); 
			A.transpose().rightMultiply(1,rhs,0,A_transpose_x_rhs); 

			//
			pStep = parameterConstraints(G, A_transpose_x_rhs, npars, pars[0], P_matrix);
		}
	
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

		// Evaluate 'linear cost'
		IVector res_lin = rhs.clone();
		A.rightMultiply(1.0, pStep, -1.0, res_lin);  // res_lin = A * pStep - rhs
		double costLinTry = res_lin.norm2();         // costLin = |res_lin|^2
		costLinTry = costLinTry*costLinTry*this.costFactor;
		
		Results.putValue("linearCost", costLinTry, 1, "outer iteration "+imain, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.OuterIteration);
		System.out.println("linearized cost for new estimate "+costLinTry);

		// use backtracking (smaller steps) if cost has not improved
		int innerIter = 0;
		double scale_step = 1.0;
		IVector predTry, pTry;
		double costTry;
		double relErrorLinCost = 0;
		int count = 0;
		int loopcount_max = 3;
		
		while (true) {
			
			/* Calculate the new parameter set */
			pTry = pars[0].clone();
			P_matrix.rightMultiply(scale_step, pStep, 1.0, pTry);
			if (count == 1 && add_constraints){

				// sort lower
				IVector dif_low = b_lower.clone() ;
				dif_low.axpy(-1,pTry);
				double[] vals_low = dif_low.getValues();
				int[] isort_low = SortUtils.sortedIndex(vals_low);
				vals_low = applyIndexToDoubles(vals_low, isort_low);
				IVector check_low = new Vector(vals_low);

				// sort upper
				IVector dif_up = b_upper.clone() ;
				dif_up.axpy(-1,pTry);
				double[] vals_up = dif_up.getValues();
				int[] isort_up = SortUtils.sortedIndex(vals_up);
				vals_up = applyIndexToDoubles(vals_up, isort_up);
				IVector check_up = new Vector(vals_up);
				
				int loopcounter = 0;
				while(check_up.getValue(0) < 0 || check_low.getValue(check_low.getSize()-1) > 0){
					scale_step *= innerScaleFac;
					pTry = pars[0].clone();
					P_matrix.rightMultiply(scale_step, pStep, 1.0, pTry);

					// sort lower
					dif_low = b_lower.clone() ;
					dif_low.axpy(-1,pTry);
					vals_low = dif_low.getValues();
					isort_low = SortUtils.sortedIndex(vals_low);
					vals_low = applyIndexToDoubles(vals_low, isort_low);
					check_low = new Vector(vals_low);

					// sort upper
					dif_up = b_upper.clone() ;
					dif_up.axpy(-1,pTry);
					vals_up = dif_up.getValues();
					isort_up = SortUtils.sortedIndex(vals_up);
					vals_up = applyIndexToDoubles(vals_up, isort_up);
					check_up = new Vector(vals_up);

					loopcounter = loopcounter + 1;
					if(loopcounter > loopcount_max){
						System.out.println("Search in negative direction is not possible because of the bounds");
						Results.putMessage("Search in negative direction is not possible because of the bounds");
						Results.putMessage("-----------------------------------------------------");	
						Results.putMessage("Algorithm fails!");
						Results.putMessage("-----------------------------------------------------");
						System.out.println("Algorithm fails!");
						break;
					}
				}
			}
			Results.putMessage("Next try p="+pTry.printString("   "));

			/* Evaluate predictions for this parameter */
			if (innerIter==0) {
				costTry = this.f.evaluate(pTry,"outer iteration "+imain);
				System.out.println("non-linear cost for new estimate "+costTry);
				if (Math.abs(costs[0]-costLinTry) <= EPSILON) {
					relErrorLinCost = 0.;
				} else {
					relErrorLinCost = Math.abs((costTry - costLinTry) / (costs[0] - costLinTry));
				}
			} else {
				costTry = f.evaluate(pTry,"inner iteration "+innerIter);
			}
			predTry = f.getLastPredictions();

			if (costTry <= costs[0] || innerIter > this.maxInnerIter) break;
			innerIter++;

			Results.putMessage("Cost is worse! Reducing stepsize.");
			if ( innerIter == minInnerNegativeLook+2 && innerScaleFac > 0.0) {
				System.out.println("This was inner it " + innerIter + ": start looking in negative direction");
				Results.putMessage("Start looking in negative direction");
				innerScaleFac *= -1.0;
				count = 1;
			}
			scale_step *= innerScaleFac;
		}

		// compute analysis error covariance from approximate Hessian
		// Pest = P_matrix*inv(A'*A)*P_matrix'
		if(this.computeErrorEstimate){
			boolean transpose = true;
			Matrix Pinv = Matrix.mult(A,A,transpose,!transpose);

			/* Singular value decomposition of Pinv:
			 *  Pinv =: svd[2] * svd[1] * svd[0]' */
			Matrix svd[] = Pinv.svd();
			
			// svd[1] = inv(svd[1])
			int np = A.getNumberOfColumns();
			for(int i=0;i<np;i++){
				svd[1].setValue(i, i, 1.0/svd[1].getValue(i,i));
			}
						
			/* Calculate the covariance matrix 
			 *   Pest = P_matrix* svd[0] * svd[1] * svd[2]' *P_matrix'
			 */
			Matrix Pest = Matrix.mult(Matrix.mult(Matrix.mult(P_matrix,svd[0]),svd[1]),
									Matrix.mult(svd[2],P_matrix,transpose,transpose));
			
			// standard deviations
			Vector std = (Vector) Pest.diag();
			std.sqrt();
			
			// now make this pretty for treeVectors
			IVector stdWithStructure = pTry.clone();
			stdWithStructure.setValues(std.getValues());
			Results.putMessage("Error estimate for this outer iteration");
			Results.putValue(	"parameterErrorEstimateStd", stdWithStructure, stdWithStructure.getSize(), 
								"outer iteration "+imain, IResultWriter.OutputLevel.Verbose,
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
                    "outer iteration "+imain, IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.OuterIteration);

			// sqrtP : required as output for some algorithms that extend DUD
			for(int i=0;i<np;i++){
				svd[1].setValue(i, i, Math.sqrt(svd[1].getValue(i, i)));
			}
			this.sqrtCovEst = Matrix.mult(Matrix.mult(Matrix.mult(P_matrix,svd[0]),svd[1]),svd[2],!transpose, transpose);
		}

		//setup for next iteration   (worst evaluation is overwritten!)
		preds[ndirs] = predTry;
		costs[ndirs] = costTry;
		pars[ndirs]  = pTry;

		// Sort parameters tried by cost
		int[] isort  = this.sortedIndex(costs);
		costs = applyIndexToDoubles(costs, isort);
		pars  = applyIndexToVectors(pars, isort);
		preds = applyIndexToVectors(preds, isort);
		
		this.moreToDo = isThereMoreToDo(costs, relErrorLinCost, predTry, pars, innerIter);

		// Display final results:
		if (!this.moreToDo) this.f.writeResults();

		// store results to object
		this.pCurrent = pars;
		this.predCurrent = preds;
		this.fCurrent = costs;

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
		for (indexValuePair sortedValue : sortedValues) {
			result[j] = sortedValue.index;
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

	public IVector[] getPredCurrent() {
		IVector result[] = new IVector[number_of_evaluations+1];
		for (int i = 0; i <= number_of_evaluations; i++) {
				result[i] = this.predCurrent[i].clone();
		}
		return result;
	}
}

