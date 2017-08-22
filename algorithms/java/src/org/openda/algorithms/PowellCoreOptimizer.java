/* OpenDA v2.4.1 
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


import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.IVector;
import org.openda.utils.Results;

import java.util.ArrayList;
import java.util.List;

public class PowellCoreOptimizer{

	// fields of this class
	IVector pMin = null;     // best sofar
	double fMin = Double.MAX_VALUE;
	IVector pCurrent = null; //value under consideration
	double fCurrent;
	IVector xiCurrent[] = null; // current search directions

	ICostFunction f = null;
	// settings for algorithm
	int maxitPowell = 200;    // maximum number of outer iterations for Powell
	int maxitBrent = 200;     // maximum number of iterations for Brent linesearch
	int maxitBracket = 20;     // maximum number of iterations for Bracketing
	double relTolPowell = 0.01; // relative convergence-check for Powell
	double absTolPowell = 0.01; // absolute convergence-check for Powell
	double relTolBrent = 0.01;   // relative convergence-check with respect to decrease in Brent-linesearch
	double absTolBrent = 0.001; // dito for absolute tolerance
	double bracketFirstTry=1.0; // first value evaluated for bracketing during linesearch
	double limit = 100.0;     // max. extension for bracketing
	double tiny = 1.0e-15;    // value to avoid division by zero in bracketing

    // required for the additional stopping criteria:
    public List<IStopCriterion> stopCriteria = new ArrayList<IStopCriterion>();
    public List<Double> stopCriteriaThreshold = new ArrayList<Double>();
    public double stopCritThresDefault = 0.01;
    public IObservationDescriptions obsDescr=null;

	//stopping
	boolean moreToDo = true;         // is this optimization finished
	int imain=0;                     // main iterations done


    /**
     * Constructor for Powell minimization
     * @param f : costFunction to be minimized
     */
    public PowellCoreOptimizer(ICostFunction f){
        this.f = f;
    }

	/**
	 * Initializer without any restart options and minimal configuration
	 * @param pInit : initial parameters as a verctor
	 * @param width : magnitude of the initial perturbations
	 */
	public void initialize(IVector pInit, double width){
		this.pMin = pInit;
		this.fMin = this.f.evaluate(pInit,"initialization");
		// generate initial perturbations
		int n = pInit.getSize();
		this.pCurrent = pInit;
		this.fCurrent = this.fMin;
        this.xiCurrent = new IVector[n];
        IVector zeroVector = pInit.clone();zeroVector.scale(0.0);
		for(int i=0;i<n;i++){
			IVector xi = zeroVector.clone();
			xi.setValue(n-i-1, xi.getValue(n-i-1)+width);
			xiCurrent[i]=xi;
		}
	}

	/**
	 * Initializer with start directions
	 * @param pInit initial parameters as a verctor
	 * @param searchInit initial perturbations
	 */
	public void initialize(IVector pInit, IVector[] searchInit){
		this.pMin = pInit;
		this.fMin = this.f.evaluate(pInit,"initialization");
		// generate initial perturbations
		int n = searchInit.length;
		this.pCurrent = pInit;
		this.fCurrent = this.fMin;
        this.xiCurrent = new IVector[n];
		for(int i=0;i<n;i++){
			xiCurrent[i]=searchInit[i].clone();
		}
	}

    /**
     * Initializer with restart options
     * @param pCurrent Parameters to be restored
     * @param fCurrent Cost function evaluation to be restored
     * @param xiCurrent Search direction values to be restored
     */
    public void initialize(IVector pCurrent, double fCurrent, IVector xiCurrent[]){
        this.fCurrent = fCurrent;
        this.pCurrent = pCurrent.clone();
        this.xiCurrent = new IVector[xiCurrent.length];
        for(int i=0;i<xiCurrent.length;i++){
            this.xiCurrent[i] = xiCurrent[i].clone();
        }
        if(fCurrent<this.fMin){ //if current is best then store this
            this.fMin = fCurrent;
            this.pMin = this.pCurrent.clone();
        }
    }

	public IVector getOptimalValue(){
        return this.pMin.clone();
	}

	public double getOptimalCost(){
        return this.fMin;
	}

	public IVector getCurrentValue(){
        return this.pCurrent.clone();
	}

	public IVector[] getCurrentSearchDirections(){
		IVector result[] = new IVector[this.xiCurrent.length];
        for(int i=0;i<this.xiCurrent.length;i++){
        	result[i] = this.xiCurrent[i].clone();
        }
        return result;
	}

	public double getCurrentCost(){
        return this.fCurrent;
	}

	/**
	 * Main optimization routine
	 */
	public void optimize(){
		while(this.hasNext()){
			this.next();
		}
 	}
	
	/**
	 * Are there any more steps for this algorithm
	 * @return has next step
	 */
	boolean hasNext(){
        return this.moreToDo;
	}
	
	/**
	 * Run next step of the algorithm
	 */
	void next(){
		/* Assume that current parameter and searchdirections are set.
		 * Also the cost for the current parameter must be computed before calling
		 * this method.
		 */
	    int n=this.xiCurrent.length; // number of parameters

        // Main iteration
	    if(imain>=this.maxitPowell){this.moreToDo=false;}
	    if(this.moreToDo){
	    	this.imain++;
	    	Results.putProgression("=================================================================");
	    	Results.putProgression("Outer loop iteration no. "+ imain);
	    	Results.putProgression("=================================================================");
		    int ibig = 0;  // keep track of biggest improvement
		    double del = 0.0;
	    	IVector p = this.pCurrent.clone();
	    	double f_p = this.fCurrent;
	    	double f_prev = f_p;
            Results.putValue("p_start", p, p.getSize(), "outer iteration "+imain, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.OuterIteration);
            Results.putValue("f_p_start", f_p, 1, "outer iteration "+imain, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.OuterIteration);
		    // foreach search direction
		    for (int i=0;i<n;i++){
		    	Results.putMessage("direction "+i);
		    	Results.putProgression("direction "+i);
		    	// optimize in each direction and keep track of largest decrease in f
		    	IVector p_opt_i = p.clone();
		    	IVector xi_i = this.xiCurrent[i];
                Results.putValue("searchDir", xi_i, xi_i.getSize(), "inner iteration "+i, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.InnerIteration);
	            Results.putProgression("Start line optimization in point "+p+" in direction:"+ xi_i);
		    	double f_opt_i = this.LineSearch(p,xi_i,p_opt_i); //p_opt_i is return value
		    	if(Math.abs(f_p-f_opt_i)>del){ // bigger decrease found
		    		del  = Math.abs(f_p-f_opt_i);
		    		ibig = i;
		    	}
		    	// continue with previous estimate
		    	p   = p_opt_i;
		    	f_p = f_opt_i;
                Results.putValue("p_dir", p, p.getSize(), "inner iteration "+i, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.InnerIteration);
                Results.putValue("f_p_dir", f_p, 1, "inner iteration "+i, IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.InnerIteration);
            }
		    // store if improvement
		    if(f_p < this.fCurrent){
		    	this.fMin = f_p;
		    	this.pMin = p.clone();
		    }
		    Results.putProgression("End point of this outer loop has cost "+f_p+" at "+p);
		    // exit iteration loop on convergence
		    // Convergence if relative improvement less than threshold
		    double relChange = (Math.abs(this.fCurrent-f_p)/(0.5*Math.abs(this.fCurrent+f_p)));
		    double absChange = (Math.abs(this.fCurrent-f_p));
		    Results.putProgression("Relative convergence check for outer loop: "+relChange+"<"+this.relTolPowell);
		    Results.putProgression("Absolute convergence check for outer loop: "+absChange+"<"+this.absTolPowell);
		    if((relChange<this.relTolPowell)|(absChange<this.absTolPowell)){this.moreToDo=false;}
            Results.putMessage("stop criterion 1, imain > maxit:\t "+imain+" < "+Math.round(this.maxitPowell));
            Results.putMessage("stop criterion 2, diff < abstol:\t "+absChange+" > "+this.absTolPowell);
            Results.putMessage("stop criterion 3, relDiff < reltol:\t "+relChange+" > "+this.relTolPowell);

            // additional stop criteria:
            if (stopCriteria.size()>0) {
                // To do: generalize for non leastsquarecostfunction too!
                IVector residual = null;
                if(this.f instanceof LeastSquaresCostFunction){
                    residual = ((LeastSquaresCostFunction)f).getObservationUncertainty().getExpectations();
                    residual.axpy(-1.0,((LeastSquaresCostFunction)f).getLastPredictions());
                } else {
                    throw new RuntimeException("Additional stop criterion is only implemented for LeastSquaresCostFunction..");
                }
                boolean isStop = false;
                for (int i=0; i<stopCriteria.size(); i++){
                    IStopCriterion object = stopCriteria.get(i);
                    double threshold = stopCriteriaThreshold.get(i);
                    if (obsDescr!=null) {
                        isStop = object.checkForStop(this.pMin,residual,obsDescr,this.fMin,threshold);
                    } else {
                        isStop = object.checkForStop(this.pMin,residual,this.fMin,threshold);
                    }
                    Results.putMessage(object.toString());
                    this.moreToDo = this.moreToDo & !isStop;
                }
            }

		    if(this.moreToDo){
		    	// update search directions
		    	IVector xi = p.clone();        //average search direction over this iter
		    	xi.axpy(-1.0, this.pCurrent);
		    	IVector pExtrapolate = p.clone(); //What happens further in this direction
		    	pExtrapolate.axpy(1.0, xi);
		    	double fExtrapolate = this.f.evaluate(pExtrapolate,"update search directions");
		    	// Now several options exist
		    	double temp= (2.0*(f_prev-2.0*f_p+fExtrapolate)*Math.pow((f_prev-f_p-del),2.0))
		    	- del*Math.pow(f_prev-fExtrapolate,2.0); // check if new direction is a good one, i.e. temp<0
		    	Results.putProgression("Candidate direction :"+xi);
		    	Results.putProgression("Is new direction a good one,i.e "+(fExtrapolate-f_prev)+"<0 and "+temp+"<0");
		    	if(fExtrapolate>f_p){ //was fExtrapolate>f_prev i.e worse than start of iteration in numrec
		    		this.pCurrent = p;
		    		this.fCurrent = f_p;
		    		// do not modify search directions
		    	}else if(temp>=0){
		    		this.pCurrent = p;
		    		this.fCurrent = f_p;
		    		// do not modify search directions
		    	}else{
		    		IVector p_opt = p.clone();
		    		double f_opt_xi = this.LineSearch(p,xi,p_opt); //p_opt is return value
		    		this.pCurrent = p_opt;
		    		this.fCurrent = f_opt_xi;
		    		// replace search direction as last entry
		    		this.xiCurrent[ibig] = this.xiCurrent[n-1];
		    		this.xiCurrent[n-1]  = xi.clone();
		    		//store if better
		    		if(f_opt_xi<this.fMin){
		    			this.fMin = f_opt_xi;
		    			this.pMin = p_opt.clone();
		    		}
		    	}
		    }
		}
	    if(!this.moreToDo){
           //Display final results:
           this.f.writeResults();
	    }

	}	

	// @param minVector is output
	private double LineSearch(IVector baseVector, IVector direction, IVector minVector){
		// Find a region in which you know a minumum must lie
        double initBracket[] = {0.0,this.bracketFirstTry,0.0};
		double[] fBracket = {0.0,0.0,0.0}; //function values initially unknown
		for(int i=0;i<2;i++){
	        IVector pTemp = baseVector.clone();pTemp.axpy(initBracket[i],direction);
	        fBracket[i] = this.f.evaluate(pTemp,"line search");
		}
		double fStart=fBracket[1];
	    double[] alphaBracket = this.bracketLineMinimum(baseVector,direction,initBracket,fBracket);
	    // Find a local minimum within this range, using parabolic fitting where
	    // possible
	    Results.putMessage("---bracket "+alphaBracket[0]+","+alphaBracket[1]+","+alphaBracket[2]);
	    Results.putMessage("--- f values "+fBracket[0]+","+fBracket[1]+","+fBracket[2]);
        double optVal = this.brentLineSearch(baseVector,direction, alphaBracket, fBracket,fStart);
	    Results.putMessage("---linesearch "+alphaBracket[0]+","+alphaBracket[1]+","+alphaBracket[2]);
	    Results.putMessage("--- f values "+fBracket[0]+","+fBracket[1]+","+fBracket[2]);
        IVector p_opt = baseVector.clone();
        p_opt.axpy(alphaBracket[1], direction); //p_opt = p+alpha*xi
        minVector.setValues(p_opt.getValues());
	    
	    //scale this search direction with alpha
        double alpha = Math.abs(alphaBracket[1]);
        if(alpha<0.1) alpha=0.1;
	    direction.scale(alpha);
	    
        return optVal;
	}


	private double[] bracketLineMinimum(IVector baseVector, IVector xi, double[] initBracket, double[] fBracket){
		double goldFactor=1.618034;
        //double alphaBracket[] = {0.0,this.bracketFirstTry,0.0};
        double ax = initBracket[0];
        double bx = initBracket[1];
        //Vector  pa = baseVector.clone();pa.axpy(ax,xi);
        //double fa = this.f.evaluate(pa);
        double fa=fBracket[0];
        //Vector  pb = baseVector.clone();pb.axpy(bx,xi);
        //double fb = this.f.evaluate(pb);
        double fb=fBracket[1];
        if (fb > fa){ //swap to get decreasing direction
    	   double dum;
    	   dum=ax;ax=bx;bx=dum; //swap ax,bx
    	   dum=fb;fb=fa;fa=dum; //swap fa,fb
        }
        // extrapolate to find 3rd point for bracket
        double cx = bx + (bx-ax) * goldFactor;
        IVector pc = baseVector.clone();pc.axpy(cx,xi);
        double fc = this.f.evaluate(pc,"bracket line minimum");
        int iter = 0;

        while(fb>=fc){ 
        	if (iter>this.maxitBracket) {
        		throw new RuntimeException("Bracketing: parameter is running away. Your problem is ill posed \n or initial parameters or their uncenrtainty are off by orders of magnitude.");
        	}

        	double r=(bx-ax)*(fb-fc);
        	double q=(bx-cx)*(fb-fa);
        	double q_min_r=(q-r);
        	if(Math.abs(q_min_r)<this.tiny){ //avoid division by zero
        		if(q_min_r>=0.0)
        			q_min_r = this.tiny;
        		else
        			q_min_r = -this.tiny;
        	}
        	double u=bx - ((bx-cx)*q-(bx-ax)*r)/(2.0*q_min_r); //extrapolate parabolic fit
        	double ulim = bx+(cx-bx)*this.limit;
        	IVector pu;
        	double fu;
        	if ((bx-u)*(u-cx) > 0.0){  // u between bx and cx
        		pu = baseVector.clone();pu.axpy(u,xi);
        	    fu =this.f.evaluate(pu,"bracket line minimum");
        	    if (fu<fc) { //u new minimum between bx and cx
        		   ax=bx;bx=u;
        		   fa=fb;fb=fu;
        		   continue;
        	    }else if(fu>fb){ // b new minimum between a and u
        		   cx=u;
        		   fc=fu;
        		   continue;
        	    }else{
        	       u=cx+(cx-bx)*goldFactor;  //parabolic fit did not work, just grow
        		   pu = baseVector.clone();pu.axpy(u,xi);
        	       fu =this.f.evaluate(pu,"bracket line minimum");
        	    }
        	}else if((cx-u)*(u-ulim)>0.0){ //u>cx but allowed
        		pu = baseVector.clone();pu.axpy(u,xi);
        	    fu =this.f.evaluate(pu,"bracket line minimum");
        	    if (fu<fc){ // still going down
        		   bx=cx;cx=u;
        		   u=cx+(cx-bx)*goldFactor; // grow again
        		   fb=fc;fc=fu;
             	   pu = baseVector.clone();pu.axpy(u,xi);
        	       fu =this.f.evaluate(pu,"bracket line minimum");
        	    }
        	}else if((u-ulim)*(ulim-cx)>=0.0){ //u>ulim
        	    u=ulim;
        		pu = baseVector.clone();pu.axpy(u,xi);
        	    fu =this.f.evaluate(pu,"bracket line minimum");
        	}else{ // dont use parabolic fit
        	    u=cx+(cx-bx)*goldFactor; // grow
        		pu = baseVector.clone();pu.axpy(u,xi);
        	    fu =this.f.evaluate(pu,"bracket line minimum");
        	}
        	ax=bx;bx=cx;cx=u;   // u > cx now replace (ax,bx,cx) <- (bx,cx,u)
        	fa=fb;fb=fc;fc=fu;
        	iter++;
        } //while
        double alphaBracket[] = {ax,bx,cx};
        fBracket[0] = fa;
        fBracket[1] = fb;
        fBracket[2] = fc;

        return alphaBracket;
	}

	private double brentLineSearch(IVector baseVector, IVector xi,
			                        double alphaBracket[],double[] fBracket, double fStart){
		double fOpt;
		double ax = alphaBracket[0];
		double bx = alphaBracket[1];
		double cx = alphaBracket[2];

	    double a = Math.min(ax,cx);
	    double b = Math.max(ax,cx);
	    double v = bx; //previous value of w
	    double w = v;
	    double x = v;
	    double e = 0.0; //previous delta x_opt
	    double d = 0.0; //last delta x_opt
	    IVector px = baseVector.clone();px.axpy(x,xi);
	    double fx = this.f.evaluate(px,"Brent line search");
	    double fv = fx;
	    double fw = fx;
	    double fLast = fx+1.0; //last evaluated value, initially larger to avoid early stops
	    double fPrevious = fx;
	    double r,p,q,u,fu,etemp;
	    for (int iter =1;iter<=this.maxitBrent;++iter){
			double xm = (a + b) * 0.5;
			double tol1 = this.relTolBrent * Math.abs(xm) + this.tiny;
			double tol2 = tol1 * 2.0;
			//if (Math.abs(x-xm) <= tol2 -(b-a)*0.5) break; // original stop criterion
			double absChange = Math.abs(fLast - fPrevious);
			double relChange = absChange/Math.abs(fStart-fLast);
			//Results.putProgression("Brent absChange="+absChange+" absTolBrent="+absTolBrent);
			//Results.putProgression("Brent relChange="+relChange+" relTolBrent="+relTolBrent);
			if((fLast<fStart)&((absChange<absTolBrent)|(relChange<relTolBrent))) break;
			/* Minimum of parabolic fit through x=a,b,c and y=f(z),f(b),f(c) is
			 *       1 (b-a)^2(f(b)-f(c))-(b-c)^2(f(b)-f(a))
			 * x=b - - -------------------------------------
			 *       2 (b-a)(f(b)-f(c))  -(b-c)(f(b)-f(a))
			 */
		    r = (x - w) * (fx - fv); //test with parabolic fit w,x,v
		    q = (x - v) * (fx - fw);
		    p = (x - v) * q - (x - w) * r;
		    q = (q - r) * 2.0;
		    if (q > 0.0) p = -p; //make sure q>0 and p contains sign, working to p/q
		    q = Math.abs(q);
		    etemp = e;
		    e = d;
		    if((Math.abs(etemp)>tol1)&(Math.abs(p) < Math.abs(q * 0.5 * etemp)&p>q*(a-x)&p<q*(b-x))){
		        // optimum x+p/q in [v,w] and steps decreasing by factor >=2
		    	// fit is accepted
			    d = p / q;
			    u = x + d;
			    if (u - a < tol2 || b - u < tol2) {
				   d = (xm-x>0?tol1:-tol1); //d_sign(tol1, xm-x);
			    }
		    }else{
		    	//try golden section
				if (x >= xm) {
				    e = a - x;
				} else {
				    e = b - x;
				}
				d = e * .381966;
		    }
		    /*
		     *  Now we have a new ?optimum? x+d from parabolic fit or golden section
		     */
			if (Math.abs(d) >= tol1) { // make steps of size at least tol1
			    u = x + d;
			} else {
			    u = x + (d>0?tol1:-tol1); //sign(tol1,d);
			}
		    IVector pu = baseVector.clone();pu.axpy(u,xi);
		    fu = this.f.evaluate(pu,"Brent line search");
		    fPrevious=fLast;fLast = fu;
            /*
             * is this u->f(u) a good point?
             */
			if (fu<=fx){  // better than previous best fit
			    if (u>=x) //narrow down bracketing
				   a=x;
			    else
				   b=x;
			    v=w;fv=fw;
			    w=x;fw=fx;
			    x=u;fx=fu;
			}else{ // keep x as best but use u to narrow down anyway
			    if(u<x)
				   a=u;
			    else
			    	b = u;
			    if (fu <= fw || w == x) {
				   v=w;fv=fw;
				   w=u;fw=fu;
			    }else if (fu <= fv || v == x || v == w) {
				   v=u;fv=fu;
			    }
			}
		} //for

        // output
	    fOpt = fx;
	    alphaBracket[0] = v;
	    alphaBracket[1] = x;
	    alphaBracket[2] = w;
	    fBracket[0]     = fv;
	    fBracket[1]     = fx;
	    fBracket[2]     = fw;
	    return fOpt;
	}



}
