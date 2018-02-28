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
import org.openda.utils.*;

import java.util.ArrayList;
import java.util.List;

public class BFGSCoreOptimizer{

	// fields of this class
	int iStore;						// current number of stored vectors
	IVector pCurrent = null;		// parameters under consideration
	double fCurrent;				// function value at current param
	IVector gradCurrent = null;		// gradient value at current param
	// for L-BFGS (list of vectors)
	IVector ss[] = null;			// vectors s_i = x_{i+1} - x_i
	IVector sy[] = null;			// vectors y_i = g_{i+1} - g_i
	// for BFGS (matrix)
	Matrix Hi = null;				// initial approximate matrix of Hessian^{-1}
	Matrix Ei = null;				// update matrix
	
	ICostFunctionWithGradient f = null;	// cost function f, with gradient
	
    // settings for algorithm
	public boolean limitedMemory = true;	// use either L-BFGS or BFGS
	public int nStore = 3;					// maximum number of stored vectors
	public int maxitBfgs = 200;				// maximum number of iterations for L-BFGS
	public int maxitBrent = 100;     		// maximum number of iterations for Brent linesearch
	public int maxitBracket = 20;     		// maximum number of iterations for Bracketing
	public double relTolGrad = 0.01;		// relative tolerance for gradient
	public double absTolGrad = 0.01;		// absolute tolerance for gradient
	public double relTolStep = 0.001;		// relative tolerance for stepsize
	public double absTolStep = 0.001;		// absolute tolerance for stepsize
	public double relTolBrent = 0.01;  		// relative check wrt decrease in Brent linesearch
	public double absTolBrent = 0.01; 		// absolute check wrt decrease in Brent linesearch
	public double bracketFirstTry = 1.0;	// first value evaluated for bracketing in linesearch
	public double limit = 100.0;     		// max. extension for bracketing
	public double tiny = 1.0e-15;    		// value to avoid division by zero
	
	// required for the additional stopping criteria:
	public List<IStopCriterion> stopCriteria = new ArrayList<IStopCriterion>();
    public List<Double> stopCriteriaThreshold = new ArrayList<Double>();
    public double stopCritThresDefault = 0.01;
    public IObservationDescriptions obsDescr=null;
    
	//stopping
	boolean moreToDo = true;        // is this optimization finished
	int imain=0;                    // main iterations done
	
    /**
     * Constructor for (L)BFGS minimization
     * @param f : cost function (with gradient) to be minimized
     */
    public BFGSCoreOptimizer(ICostFunctionWithGradient f){
        this.f = f;
    }

    /**
	 * Initializer without any restart options and with minimal configuration
	 */
	public void initialize(IVector pInit){
		this.pCurrent = pInit.clone();
		this.fCurrent = this.f.evaluate(pInit,"initialization");
		this.gradCurrent = this.f.evaluateGradient(pInit);
		if (limitedMemory = true) {							// L-BFGS
			this.ss = new IVector[nStore];
	        this.sy = new IVector[nStore];
	        IVector zeroVector = pInit.clone();
	        zeroVector.setConstant(0.0);
			for(int i=0;i<nStore;i++){
				sy[i]=zeroVector.clone();
				ss[i]=zeroVector.clone();
			} 
			this.iStore = 0;
		} else {											// BFGS
			this.Hi = Matrix.eye(pInit.getSize());
			this.Ei = this.Hi.clone();
		}
	}

	/**
     * Initializer with restart options for LBFGS
     * @param pInit, fInit, gInit and updates ssInit and syInit to be restored
     */
	public void initializeLBFGS(IVector pInit, double fInit, IVector gInit,
								IVector[] ssInit, IVector[] syInit){
		this.pCurrent = pInit.clone();
		this.fCurrent = fInit;
		this.gradCurrent = gInit;
		this.ss = new IVector[nStore];
        this.sy = new IVector[nStore];
        IVector zeroVector = pInit.clone();
        zeroVector.setConstant(0.0);
        this.iStore = ssInit.length;
		if (syInit.length < this.iStore) this.iStore = syInit.length;
		if (this.iStore > this.nStore) this.iStore = this.nStore;
        for(int i=0;i<this.iStore;i++){
			sy[i]=syInit[i].clone();
			ss[i]=ssInit[i].clone();			
		}
        for(int i=this.iStore;i<nStore;i++){
			sy[i]=zeroVector.clone();
			ss[i]=zeroVector.clone();
		}
        this.limitedMemory = true;
	}	
	
    /**
     * Initializer with restart options for BFGS (with matrix)
     * @param pInit, fInit, gInit and approximation of Hessian^-1 Hi to be restored
     */
	public void initializeBFGS(IVector pInit, double fInit, IVector gInit, Matrix HInit){
		this.pCurrent = pInit.clone();
		this.fCurrent = fInit;
		this.gradCurrent = gInit.clone();
		this.Hi = HInit.clone();
		this.limitedMemory = false;
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
	 * Get the (current) gradient 
	 */
	public IVector getGradient(){
        return this.gradCurrent;
	}
	
	/**
	 * Get the (current) matrix
	 */
	public Matrix getMatrix(){
        return this.Hi;
	}
	
	/**
	 * Get the (current) list of updates of the parameters
	 */
	public IVector[] getSs(){
        return this.ss;
	}	
	
	/**
	 * Get the (current) list of updates of the gradient
	 */
	public IVector[] getSy(){
        return this.sy;
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

		double gd;									// gradient.sdir (check Hi positive definite)
		IVector ss;									// parameter update
		IVector sy;									// gradient update
		
		IVector sdir;								// search direction
		IVector slindir;							// normalized (line)search direction
		double val;									// dot product of search direction
		
		double gradnorm; 							// norm of gradient (stopping criterion)
		double gradprevnorm;						// norm of prev grad (stopping criterion)
		double stepln;								// step size (stopping criterion)
		double relStep;								// relative step size (stopping criterion)
		double relGrad;								// relative grad size (stopping criterion)
		
		IVector sparam = this.pCurrent.clone(); 	// current parameters
		IVector sparamprev;							// prev parameters	
		double cost = this.fCurrent; 				// current optimal cost
		IVector sgrad = this.gradCurrent.clone(); 	// current gradient
		IVector sgradprev;							// prev gradient

		// Main loopIter

		if(this.moreToDo){
			this.imain++;
	    	Results.putProgression("==============================================================");
	    	Results.putProgression("Iteration no. "+ imain);
	    	Results.putProgression("==============================================================");
			
	    	sdir = sgrad.clone();
	    	sdir.scale(-1.0);
	    	
	    	if (limitedMemory==true) {							// LBFGS
	    		sdir = this.LbfgsRecursive(this.iStore,sdir);	// @sdir: updated in LbfgsRecursive
	    	} else {							    			// BFGS
	    		this.Hi.rightMultiply(1.0,sdir,0.0,sdir);
	    	}
	    	
	    	// check for sdir to be a descent direction
	    	gd = sgrad.dotProduct(sdir);
	    	if (gd >=0) {
	    		//Results.putProgression("Error: search direction is not a descent direction.");
    			sdir.scale(-1.0);
    		}
	    	
	    	// perform line search
	    	sparamprev = sparam.clone();
	    	slindir = sdir.clone();
	    	val = slindir.norm2();
	    	slindir.scale(1.0/val); 							//normalize search direction
	    	cost = this.LineSearch(sparamprev, slindir, sparam);// @sparam: updated in LineSearch
	    	stepln = slindir.norm2();

	    	// update parameters
	    	ss = sparam.clone();
	    	ss.axpy(-1.0,sparamprev);
	    	sgradprev = sgrad.clone();
	    	gradprevnorm = sgradprev.norm2();	
	    	sgrad = this.f.evaluateGradient(sparam);
	        gradnorm = sgrad.norm2();
	        sy = sgrad.clone();
	    	sy.axpy(-1.0, sgradprev);
	    	
	    	if (limitedMemory==true) {
	    		//LBFGS update: add s,y to the list of vectors
		    	this.iStore++;
		    	if (this.iStore<=this.nStore){
		    		// number of stored s,y is less than max
		    		this.ss[iStore-1] = ss.clone();
		    		this.sy[iStore-1] = sy.clone();	
		    	} else {
		    		// move old up and add new
		    		for (int i=1; i<this.nStore; i++){
		    			this.ss[i-1] = this.ss[i].clone();
		    			this.sy[i-1] = this.sy[i].clone();
		    		}
		    		this.ss[nStore-1] = ss.clone();
		    		this.sy[nStore-1] = sy.clone();
		    		this.iStore = this.nStore;
		    	}
		    } else {
		    	// BFGS update: matrix
		    	// H(i+1) = H(i) + (s^T y + y^T H y)/(s^T y)^2 - (H y s^T + s y^T H)/(s^T y)
		    	Matrix ssT = Matrix.outer(ss,ss);	// s*s^T
		    	IVector Hy = ss.clone();
		        this.Hi.rightMultiply(1, sy, 0, Hy);			
		        double yHy = sy.dotProduct(Hy);					
		        double sdy = sy.dotProduct(ss)+this.tiny;		
		        double cons = (sdy + yHy)/(Math.pow(sdy, 2.0));	
		        ssT.scale(cons);				// A := (s.y+yHy)(ss^T)/([s.y]^2)
		        this.Ei = ssT.clone();
		    	Matrix syT = Matrix.outer(ss,sy);
		    	syT = syT.mult(this.Hi);
		    	Matrix ysT = Matrix.outer(sy,ss);
		    	Matrix HysT = this.Hi.mult(ysT);
		    	HysT.axpy(1.0, syT);
		    	HysT.scale(-1.0/sdy);			// B := -(H*y*s^T + s*y^T*H)/(s.y)
		        this.Ei.axpy(1.0,HysT);			// E := A + B
		        
		        Hi.axpy(1.0,this.Ei);			// H(i+1) = Hi + Ei
	        
		    }
	    	
	    	relStep = stepln/(sparamprev.norm2()+this.tiny);
	    	relGrad = gradprevnorm/(gradnorm+this.tiny);

            Results.putMessage("Step length: "+ stepln);
            Results.putMessage("New parameters for minimum: "+ sparam);
            Results.putMessage("Cost at this minimum: "+ cost);

            this.moreToDo = (this.imain<this.maxitBfgs) & (stepln>this.absTolStep) &
            (relStep>this.relTolStep) & (gradnorm>this.absTolGrad) & (relGrad>this.relTolGrad);
    	
            System.out.println("stop criterion 1,imain > maxitBfgs:\t "+this.imain+" < "+this.maxitBfgs);
            System.out.println("stop criterion 2,stepln < absTolStep:\t "+stepln+" > "+this.absTolStep);
            System.out.println("stop criterion 3,relStep < relTolStep:\t "+relStep+" > "+this.relTolStep);
            System.out.println("stop criterion 4,gradnorm < absTolGrad:\t "+gradnorm+" > "+this.absTolGrad);
            System.out.println("stop criterion 5,relGrad < absTolGrad:\t "+relGrad+" > "+this.relTolGrad);  
            
            Results.putMessage("stop criterion 1,imain > maxitBfgs:\t "+this.imain+" < "+this.maxitBfgs);
            Results.putMessage("stop criterion 2,stepln < absTolStep:\t "+stepln+" > "+this.absTolStep);
            Results.putMessage("stop criterion 3,relStep < relTolStep:\t "+relStep+" > "+this.relTolStep);
            Results.putMessage("stop criterion 4,gradnorm < absTolGrad:\t "+gradnorm+" > "+this.absTolGrad);
            Results.putMessage("stop criterion 5,relGrad < absTolGrad:\t "+relGrad+" > "+this.relTolGrad);            

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
		this.fCurrent = cost;
		this.pCurrent = sparam.clone();
		this.gradCurrent = sgrad.clone();
		
	}
	
	// @param sdirect is output
	private IVector LbfgsRecursive(int iStore, IVector sdirect){

		if (iStore!=0) {
			double rho = this.sy[iStore-1].dotProduct(this.ss[iStore-1]);
			rho = 1.0/rho;
			double alpha = this.ss[iStore-1].dotProduct(sdirect);
			alpha = rho*alpha;
			sdirect.axpy(alpha, this.sy[iStore-1]);
			sdirect = this.LbfgsRecursive(iStore-1,sdirect);
			double beta = this.sy[iStore-1].dotProduct(sdirect);
			beta = alpha - rho*beta;
			sdirect.axpy(beta, this.ss[iStore-1]);
		}
		return sdirect;
	}
	
	// @param minVector is output
	private double LineSearch(IVector baseVector, IVector direction, IVector minVector){
		// Find a region in which you know a minimum must lie
        double initBracket[] = {0.0,this.bracketFirstTry,0.0};
		double[] fBracket = {0.0,0.0,0.0}; //function values initially unknown
		for(int i=0;i<2;i++){
	        IVector pTemp = baseVector.clone();
	        pTemp.axpy(initBracket[i],direction);
	        fBracket[i] = this.f.evaluate(pTemp,"line search "+i);
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
		    fu = this.f.evaluate(pu,"Brent line search");
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
