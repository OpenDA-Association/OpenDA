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

package org.openda.algorithms;

import org.openda.interfaces.*;
import org.openda.utils.*;

import java.util.ArrayList;
import java.util.List;

public class ConjugateGradientCoreOptimizer{

	// fields of this class
	IVector pCurrent = null;			// value under consideration
	double fCurrent;					// f(parameters)
	IVector gradCurrent = null;			// grad(parameters)
	IVector searchCurrent = null; 		// current search direction
	
	ICostFunctionWithGradient f = null;	// cost function f, with gradient
	      
    // settings for algorithm
	public int method = 2;					// (1)FletcherReeves(2)PolakRibiere(else)SteepestDescent
	public int maxitConGrad = 200;			// maximum number of iterations for Conjugate Gradient
	public int maxitBrent = 200;     		// maximum number of iterations for Brent linesearch
	public int maxitBracket = 20;     		// maximum number of iterations for Bracketing
	public double relTolGrad = 0.01;		// relative tolerance for gradient
	public double absTolGrad = 0.01;		// absolute tolerance for gradient
	public double relTolStep = 0.01;		// relative tolerance for stepsize
	public double absTolStep = 0.01;		// absolute tolerance for stepsize
	public double relTolBrent = 0.01;   	// relative check wrt decrease in Brent linesearch
	public double absTolBrent = 0.01; 		// absolute check wrt decrease in Brent linesearch
	public double bracketFirstTry = 1.0; 	// first value evaluated for bracketing in linesearch
	public double limit = 100.0;     		// max. extension for bracketing
	public double tiny = 1.0e-15;    		// value to avoid division by zero
	
	// required for the additional stopping criteria:
	public List<IStopCriterion> stopCriteria = new ArrayList<IStopCriterion>();
    public List<Double> stopCriteriaThreshold = new ArrayList<Double>();
    public double stopCritThresDefault = 0.01;
    public IObservationDescriptions obsDescr=null;
    
	//stopping
	boolean moreToDo = true;         	// is this optimization finished
	int imain=0;                     	// main iterations done
	
    /**
     * Constructor for Conjugate Gradient minimization
     * @param f : cost function (with gradient) to be minimized
     */
    public ConjugateGradientCoreOptimizer(ICostFunctionWithGradient f){
        this.f = f;
    }

    /**
	 * Initializer without any restart options with minimal configuration
	 */
	public void initialize(IVector pInit){
		this.pCurrent = pInit.clone();
        this.fCurrent = this.f.evaluate(pInit,"initialization");
		this.gradCurrent = this.f.evaluateGradient(pInit);
		this.searchCurrent = this.gradCurrent.clone();
		this.searchCurrent.scale(-1.0);	// use 'minus gradient' as initial search direction
	}
    
	/**
     * Initializer with restart options
     * @param pInit, fInit, gInit and searchInit to be restored
     */
    public void initialize(IVector pInit, double fInit, IVector gInit, IVector searchInit){
    	this.pCurrent = pInit.clone();
    	this.fCurrent = fInit;
    	this.gradCurrent = gInit.clone();
		this.searchCurrent = searchInit.clone();
    }
    
    /**
	 * Get the (current) optimal parameters
	 * @return parameters as Vector
	 */
    public IVector getOptimalValue(){
        return this.pCurrent.clone();
	}

	/**
	 * Get the (current) optimal cost 
	 */
	public double getOptimalCost(){
        return this.fCurrent;
	}
	
	/**
	 * Get the (current) gradient parameters
	 */	
	public IVector getGradient(){
		return this.gradCurrent;
	}
	
	/**
	 * Get the (current) search parameter 
	 */	
	public IVector getSearchDirection(){
		return this.searchCurrent;
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
	 * Are there any more steps for this algorithm?
	 * @return has next step
	 */
	boolean hasNext(){
        return this.moreToDo;
	}
    
    /**
	 * Run next step of the algorithm
	 */
	void next(){

		double gg;									// dot product of gradient
		double ggprev;         						// and its previous value
		IVector sgrad;								// gradient
		IVector sgradprev;							// previous gradient 
		
		IVector slindir;							// normalized (line) search direction
		double val;									// dot product of search direction
		
		double gradnorm;							// norm of gradient (stopping criterion)
		double gradprevnorm;						// norm of prev grad (stopping criterion)
		double stepln;								// step size (stopping criterion)
		double relStep = 0;							// relative step size (stopping criterion)
		double relGrad = 0;							// relative grad size (stopping criterion)
		
		IVector sparam = this.pCurrent.clone(); 	// current parameters
		IVector sparamprev;							// previous parameters
		double cost = this.fCurrent; 				// current optimal cost
		IVector sdir = this.searchCurrent.clone(); 	// search direction  
		
		sgrad = this.gradCurrent;
		sgrad.scale(-1.0);	// 'minus gradient' is used
		
		// Main loopIter

		if(this.moreToDo){
			this.imain++;
	    	Results.putProgression("==============================================================");
	    	Results.putProgression("Iteration no. "+ imain);
	    	Results.putProgression("==============================================================");
			
	    	// perform line search
	    	sparamprev = sparam.clone();
	    	slindir = sdir.clone();
	    	val = slindir.norm2();
	    	slindir.scale(1.0/val);								// normalize search direction
	    	cost = this.LineSearch(sparamprev, slindir, sparam);// @sparam: updated in LineSearch
	    	stepln = slindir.norm2();
			
	    	// update parameters
			sgradprev = sgrad.clone();
			gradprevnorm = sgradprev.norm2();
			ggprev = sgradprev.dotProduct(sgradprev);
			sgrad = this.f.evaluateGradient(sparam);//
			sgrad.scale(-1.0);
			gradnorm = sgrad.norm2();
			
			// compute search direction
			if (this.method==1){
				// Fletcher-Reeves:	  h_{i+1}=g_{i+1} + [gg_{i+1}/gg_i] h_i
				gg = sgrad.dotProduct(sgrad);
				sdir.scale(gg/ggprev); 
				sdir.axpy(1.0,sgrad); 
			} else {
				if (this.method==2){
					// Polak-Ribiere:   gg_{i+1}=(g_{i+1}-g_i).g_{i+1}
					// 				 	h_{i+1}=g_{i+1} + [gg_{i+1}/gg_i] h_i
					sgradprev.scale(-1.0);
					sgradprev.axpy(1.0,sgrad);
					gg = sgradprev.dotProduct(sgrad); 
					sdir.scale(gg/ggprev);
					sdir.axpy(1.0,sgrad); 
				} else {
					// Steepest Descent:   h_{i+1}=g_{i+1}
		            sdir = sgrad.clone();
				}
			}
			
			relStep = stepln/(sparamprev.norm2()+this.tiny);
			relGrad = gradprevnorm/(gradnorm+this.tiny);
			
            Results.putMessage("Step length: "+ stepln);
            Results.putMessage("New parameters for minimum: "+ sparam);
            Results.putMessage("Cost at this minimum: "+ cost);

            this.moreToDo = (this.imain<this.maxitConGrad) & (stepln>this.absTolStep) & 
            (relStep> relTolStep) & (gradnorm>this.absTolGrad) & (relGrad>this.relTolGrad);
            
            Results.putMessage("stop criterion 1,imain > maxitConGrad:\t "+this.imain+" < "+this.maxitConGrad);
            Results.putMessage("stop criterion 2,stepln < absTolStep:\t "+stepln+" > "+this.absTolStep);
            Results.putMessage("stop criterion 3,relStep < relTolStep:\t "+relStep+" > "+this.relTolStep);
            Results.putMessage("stop criterion 4,gradnorm < absTolGrad:\t "+gradnorm+" > "+this.absTolGrad);
            Results.putMessage("stop criterion 5,relGrad < relTolGrad:\t "+relGrad+" > "+this.relTolGrad);
            
		}
		// end of iteration loop
		
        // additional stop criteria:
        if (stopCriteria.size()>0) {
            // To do: generalize for non-leastsquarecostfunction too!
            IVector residual = null;
            if(this.f instanceof LeastSquaresCostFunction){
                residual=((LeastSquaresCostFunction)f).getObservationUncertainty().getExpectations();
                residual.axpy(-1.0,((LeastSquaresCostFunction)f).getLastPredictions());
            } else {
                throw new RuntimeException("Additional stop criterion is only implemented for LeastSquaresCostFunction.");
            }
            boolean isStop = false;
            for (int i=0; i<stopCriteria.size(); i++){
                IStopCriterion object = stopCriteria.get(i);
                double threshold = stopCriteriaThreshold.get(i);
                if (obsDescr!=null) {
                    isStop = object.checkForStop(this.pCurrent,residual,obsDescr,this.fCurrent,threshold);
                } else {
                    isStop = object.checkForStop(this.pCurrent,residual,this.fCurrent,threshold);
                }
                Results.putMessage(object.toString());
                this.moreToDo = this.moreToDo & !isStop;
            }
        }
        
        if(!this.moreToDo){
			// display final results
	    	this.f.writeResults();
		}
		
        // update parameters
		this.pCurrent = sparam.clone();
		this.fCurrent = cost;
		this.gradCurrent = sgrad.clone();
		this.searchCurrent = sdir.clone();
		
	}
	
	// @param minVector is output
	private double LineSearch(IVector baseVector, IVector direction, IVector minVector){
		// Find a region in which you know a minimum must lie
        double initBracket[] = {0.0,this.bracketFirstTry,0.0};
		double[] fBracket = {0.0,0.0,0.0}; //function values initially unknown
		for(int i=0;i<2;i++){
	        IVector pTemp = baseVector.clone();
	        pTemp.axpy(initBracket[i],direction);
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
	    direction.scale(alpha);
	    
        return optVal;
	}
	
	private double[] bracketLineMinimum(IVector baseVector, IVector xi, double[] initBracket,
										double[] fBracket){
		double goldFactor=1.618034;
        //double alphaBracket[] = {0.0,this.bracketFirstTry,0.0};
        double ax = initBracket[0];
        double bx = initBracket[1];
        //Vector pa = baseVector.clone();pa.axpy(ax,xi);
        //double fa = this.f.evaluate(pa);
        double fa=fBracket[0];
        //Vector pb = baseVector.clone();pb.axpy(bx,xi);
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

	private double brentLineSearch(IVector baseVector, IVector xi, double alphaBracket[],
								   double[] fBracket, double fStart){
		double fOpt;
		double ax = alphaBracket[0];
		double bx = alphaBracket[1];
		double cx = alphaBracket[2];

	    double e = 0.0; //previous delta x_opt
	    double d = 0.0; //last delta x_opt
	    
	    double a = Math.min(ax,cx); 
	    double b = Math.max(ax,cx);
	    double v = bx; //previous value of w
	    double w = v;
	    double x = v;

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
			if((fLast<fStart+this.absTolBrent)&((absChange<this.absTolBrent)|
												(relChange<this.relTolBrent))) break;
			/* Minimum of parabolic fit through x=a,b,c and y=f(a),f(b),f(c) is
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
		    fu = this.f.evaluate(pu,"bracket line minimum");
		    fPrevious=fLast;fLast = fu;
            /*
             * is this u->f(u) a good point?
             */
			if (fu<=fx){  // better than previous best fit
			    if (u>=x) // narrow down bracketing
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
