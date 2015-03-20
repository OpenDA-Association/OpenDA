package org.openda.costa;

import org.openda.interfaces.IArray;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: nils
 * Date: 7/13/12
 * Time: 9:32 AM
 * To change this template use File | Settings | File Templates.
 */
public class CtaArray extends CtaObject implements IArray{

	final static int maxDimensions = 20;

	public CtaArray(){}

	public CtaArray(double[] values, int[] dimensions, boolean copyvalues){
		 this.create(values, dimensions);
	}

	public CtaArray(CtaArray source){
		int[] dimensions=source.getDimensions();
		double[] values=source.getValuesAsDoubles();
		this.create(values,dimensions);
	}


	public CtaArray(String source){
		int stringIndex=0;
		ArrayList<Double> valuesList= new ArrayList<Double>();
		int[] counter = new int[maxDimensions];
		int[] dims    = new int[maxDimensions];
		int curDim=-1; //current dimension
		int rank=0;
		while(stringIndex<source.length()){
			if(source.charAt(stringIndex)=='{'){
				curDim++;
				if(curDim==rank) rank=curDim+1;
				stringIndex++;
			}else if(source.charAt(stringIndex)=='}'){
				if(counter[curDim]>dims[curDim]) dims[curDim]=counter[curDim];
				counter[curDim]=0;
				curDim--;
				if(curDim<-1){
					throw new RuntimeException("Too many closing brackets in array at position="+stringIndex
							+" in string="+source);
				}
				stringIndex++;
			}else if(source.charAt(stringIndex)==','){
				counter[curDim]++;
				stringIndex++;
			}else{ //try to find a number
				int indexEnd=source.indexOf('}', stringIndex);
				// if a comma comes first
				int indexComma=source.indexOf(',', stringIndex);
				if((indexComma>=0)&(indexComma<indexEnd)){
					indexEnd = indexComma;
				}
				String numberString=source.substring(stringIndex, indexEnd);
				double value;
				try {
					value = Double.parseDouble(numberString);
				} catch (NumberFormatException e) {
					throw new RuntimeException("Problems parsing array at position="+stringIndex
							+"while processing number "+numberString);
				}
				valuesList.add(value);
				stringIndex=indexEnd;
			}
		}
		// store values
		int n=valuesList.size();
		double[] values = new double[n];
		for(int i=0;i<n;i++){
			values[i]=valuesList.get(i);
		}
		//store dimensions
		int[] dimensions = new int[rank];
		for(int i=0;i<rank;i++){
			dimensions[i]=dims[i]+1;
		}
		this.create(values,dimensions);
	}


	private native void create(double[] values, int[] dimensions);

	public native int getNumberOfDimensions();

	public native int[] getDimensions();

	public native int length();

	public native double[] getValuesAsDoubles();

	public native double[] getValuesAsDoubles(boolean copyValues);

	public native double[] getValuesAsDoubles(int firstIndex, int lastIndex);

	public native double getValueAsDouble(int index);

	public native double getValueAsDouble(int[] indices);

	public native void setConstant(double value);

	public native void setValuesAsDoubles(double[] values);

	public native void setValuesAsDoubles(int firstIndex, int lastIndex, double[] values);

	public native void setValueAsDouble(int index, double value);

	public native void setValueAsDouble(int[] indices, double value);

	public native void axpyOnValues(double alpha, double[] axpyValues);

	public native void multiplyValues(double[] multiplicationFactors);

	public native void reshape(int[] dimensions);

	public boolean allowsGrowingFirstDimension() {
		return false;  //To change body of implemented methods use File | Settings | File Templates.
	}

	public native IArray getSlice(int dimension, int index);

	public native IArray getSlice(int dimension, int minIndex, int maxIndex);

	public native double[] getSliceAsDoubles(int dimension, int minIndex, int maxIndex);

	public void setSlice(IArray slice, int dimension, int index) {
		if (slice instanceof CtaArray){
			setSliceBySlice(slice, dimension, index);
		}
		else {
			double [] values = slice.getValuesAsDoubles(false);
			this.setSlice(values, dimension, index);
		}
	}

	public native void setSlice(double[] slice, int dimension, int index);

	private native void setSliceBySlice(IArray slice, int dimension, int index);


	public void setSlice(IArray slice, int dimension, int minIndex, int maxIndex) {
		if (slice instanceof CtaArray){
			setSliceBySlice(slice, dimension, minIndex, maxIndex);
		}
		else {
			double [] values = slice.getValuesAsDoubles(false);
			this.setSlice(values, dimension, minIndex, maxIndex);
		}
	}

    private native void setSliceBySlice(IArray slice, int dimension, int minIndex, int maxIndex);

	public native void setSlice(double[] slice, int dimension, int minIndex, int maxIndex);

	public native int valueIndex(int[] indices);


/**
	 * Write contents as String, eg, {{1,2,3},{4,5,6}}
	 */
	public String toString(){
		StringBuffer output = new StringBuffer();
		writeString(0,0,output);
		return output.toString();
	}

	/**
	 * Recusive write routine
	 * @param level
	 * @param output
	 */
	private int writeString(int level, int startIndex, StringBuffer output){
		int index = startIndex;
		int[] dimensions=this.getDimensions();
		double[] values=this.getValuesAsDoubles();


		int k= dimensions.length;
		output.append("{");
		for(int i=0;i<dimensions[level];i++){
			if(i>0){
				output.append(",");
			}
			if(level==k-1){
				output.append(values[index]);
				index++;
			}else{
				index = writeString(level+1,index,output);
			}
		}
		output.append("}");
		return index;
	}



}
