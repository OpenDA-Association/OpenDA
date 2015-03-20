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
package org.costa;

import org.openda.interfaces.Vector;


public class CtaVector extends CtaObject implements Vector {

    public CtaVector(int length) {
        ctaHandle = this.ctaCreate(length);
    }

    public CtaVector() {
    }

    public void pointwiseDivide(Vector otherVector) {
        if ( otherVector instanceof CtaVector ) {
            int hOtherVector = ((CtaVector)otherVector).ctaHandle;
            this.ctaPointwiseDivide(hOtherVector);
        }
        else {
            throw new UnsupportedOperationException("org.costa.CtaVector.pointwiseDivide not implemented yet for other types");
        }
  	}


	public void pointwiseMultiply(Vector otherVector) {
        if ( otherVector instanceof CtaVector ) {
            int hOtherVector = ((CtaVector)otherVector).ctaHandle;
            this.ctaPointwiseMultiply(hOtherVector);
        }
        else {
            throw new UnsupportedOperationException("org.costa.CtaVector.pointwiseMultiply not implemented yet for other types");
        }
    }



	@SuppressWarnings({"CloneDoesntCallSuperClone", "CloneDoesntDeclareCloneNotSupportedException"})
    @Override
    public native Vector clone();

    public native void setConstant(double value);
        

    public native void scale(double alpha);
    
	public native void setValues(double[] values);
    public native double[] getValues();

    public native void setValue(int index, double value);
    public native double getValue(int index);

    public native int getSize();

    public void axpy(double alpha, Vector otherVector) {
        int hOtherVector;


        if ( otherVector instanceof CtaVector ) {
            hOtherVector = ((CtaVector)otherVector).ctaHandle;
        }
        else {
            // Convert the other vector
            Vector other=this.clone();
            other.setValues(otherVector.getValues());
            hOtherVector = ((CtaVector)other).ctaHandle;
         }
        this.ctaAxpy(alpha, hOtherVector);
    }

    public void sqrt() {
            this.ctaSqrt();
    }
    


    public double dotProduct(Vector otherVector) {
        if ( otherVector instanceof CtaVector ) {
            int hOtherVector = ((CtaVector)otherVector).ctaHandle;
            return this.ctaDotProduct(hOtherVector);
        }
        else {
            throw new UnsupportedOperationException("org.costa.CtaVector.dotProduct not implemented yet for other types");
        }
    }

    public native double norm2();

    private native void ctaSqrt()      ;
    private native int ctaCreate(int length);
    private native double ctaDotProduct(int hOtherVector);
    private native void ctaPointwiseDivide(int otherVector);
    private native void ctaPointwiseMultiply(int otherVector);
    private native void ctaSetValues(int otherVector);  
    private native void ctaAxpy(double alpha, int otherVector);


public void setValues(int[] values) {
	double [] dvalues=new double[values.length];
	   for (int i=0;i<values.length;i++){
		 dvalues[i]= values[i];
       }
       this.setValues(dvalues);
   	}

    /**
     * Write Vector to string
     * <p/>
     */
    public String toString(){
        return printString("");
    }
    public String printString(String indent) {
        String temp = indent + "[";
//        double [] values = this.getValues();
        int n=this.getSize();
        int nloop=Math.min(100,this.getSize());
        for(int i=0;i<nloop;i++){
          temp = temp + this.getValue(i);
          if(i<nloop-1){
             temp+= ",";
          }
        }
        temp = temp + "]";
        return temp;
    }
}

