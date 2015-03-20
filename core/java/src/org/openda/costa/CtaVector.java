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
package org.openda.costa;

import org.openda.interfaces.IVector;


public class CtaVector extends CtaObject implements IVector {

    public CtaVector(int length) {
        ctaHandle = this.ctaCreate(length);
    }

    public CtaVector(IVector x) {
       int n=x.getSize();
       ctaHandle = this.ctaCreate(n);
       double[] vals=x.getValues();
       this.setValues(vals);
    }

    public CtaVector() {
    }


    public void printHandles(String location){
         ctaPrintHandles(location);           
    }

    public native void ctaPrintHandles(String location);


    public void pointwiseDivide(IVector otherVector) {

        if ( otherVector instanceof CtaVector ) {
            int hOtherVector = ((CtaVector)otherVector).ctaHandle;
            this.ctaPointwiseDivide(hOtherVector);
        }
        else {
           IVector y= this.clone();
           y.setValues(otherVector.getValues());
           int hY = ((CtaVector)y).ctaHandle;
           this.ctaPointwiseDivide(hY);
           y.free();
        }
  	}


	public void pointwiseMultiply(IVector otherVector) {
        if ( otherVector instanceof CtaVector ) {
            int hOtherVector = ((CtaVector)otherVector).ctaHandle;
            this.ctaPointwiseMultiply(hOtherVector);
        }
        else {
            IVector y= this.clone();
            y.setValues(otherVector.getValues());
            int hY = ((CtaVector)y).ctaHandle;
            this.ctaPointwiseMultiply(hY);
            y.free();
        }
    }

	@SuppressWarnings({"CloneDoesntCallSuperClone", "CloneDoesntDeclareCloneNotSupportedException"})
    @Override
    public native IVector clone();

    public native void setConstant(double value);


    public native void scale(double alpha);

	public native void setValues(double[] values);
    public native double[] getValues();

    public native void setValue(int index, double value);
    public native double getValue(int index);

    public native int getSize();



    public void axpy(double alpha, IVector otherVector) {
        int hOtherVector;


        if ( otherVector instanceof CtaVector ) {
            hOtherVector = ((CtaVector)otherVector).ctaHandle;
            this.ctaAxpy(alpha, hOtherVector);
        }
        else {
            IVector y= this.clone();
            y.setValues(otherVector.getValues());
            int hY = ((CtaVector)y).ctaHandle;
            this.ctaAxpy(alpha, hY);
            y.free();
        }
    }

    public void sqrt() {
            this.ctaSqrt();
    }



    public double dotProduct(IVector otherVector) {
        if ( otherVector instanceof CtaVector ) {
            int hOtherVector = ((CtaVector)otherVector).ctaHandle;
            return this.ctaDotProduct(hOtherVector);
        }
        else {
            IVector y= this.clone();
            y.setValues(otherVector.getValues());
            int hY = ((CtaVector)y).ctaHandle;
            double dot = this.ctaDotProduct(hY);
            y.free();
            return dot;
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

    public int export(int filehandle) {
        IVector otherVector = this.clone();
        CtaTreeVector ctaTreeVector = new CtaTreeVector("ctavector","ctavector",otherVector);
        return ctaTreeVector.export(filehandle);

    }
}

