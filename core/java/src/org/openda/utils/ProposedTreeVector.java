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

package org.openda.utils;

import org.openda.interfaces.IArray;
import org.openda.interfaces.IArrayExchangeItem;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;
import org.openda.interfaces.ProposedITreeVector;
import org.openda.interfaces.ProposedIVector;

import java.io.PrintStream;
import java.util.ArrayList;

/**
 * A TreeVector can contain:
 *  - values
 *  - metadata
 *  - sub-TreeVectors
 *  It is the core element for exchange of data between a model and an algorithm. A lot of functionality
 *  is provided by OpenDA here to structure your model data and to pass meta-data.
 *  1)For an algorithm a TreeVector is a vector that may contain additional meta-data and provides 
 *  linear algebra methods.
 *  2)For a model the TreeVector is a method of collecting model data in a structured way.
 *  
 *  In this implementation an ExchangeItem is automatically extended with linear algebra if needed.
 *  Native Vectors and TreeVectors are respected and work is delegated where possible.
 */
public class ProposedTreeVector implements ProposedITreeVector {

	//class data
    private String id;
    private String description;
    private ArrayList<ProposedITreeVector> subTreeVectors = new ArrayList<ProposedITreeVector>();
    private ProposedIVector vector = null;
    private int totalSize = 0;
    private boolean excludeFromVector = false;
    //for ExchangeItems
    private Role role=Role.Input;
    private ITimeInfo timeInfo=null;
    private IQuantityInfo quantityInfo=null;
    private IGeometryInfo geometryInfo=null;
    private IArrayExchangeItem exchangeItem=null;

    public ProposedTreeVector(String id) {
    	if(id==null){
    		throw new RuntimeException("TreeVectors must have an Id");
    	}
		this.id=id;
        this.description = this.id;
    }

    public ProposedTreeVector(String id, String description) {
        this(id);
        this.description = description;
    }

    public ProposedTreeVector(String Id, ProposedIVector vector) {
        this(Id);
        this.vector = vector;
        this.totalSize = vector.length();
    }

    public ProposedTreeVector(String id, String description, ProposedIVector vector) {
        this(id, vector);
        this.description = description;
    }

    public ProposedTreeVector(IArrayExchangeItem item){
    	this.exchangeItem = item;
    	this.id = item.getId();
    	this.description = item.getDescription();
    	this.timeInfo=item.getTimeInfo();
    	this.quantityInfo=item.getQuantityInfo();
    	this.geometryInfo=item.getGeometryInfo();
    	IArray values = item.getArray();
    	if(values instanceof ProposedIVector){
    		this.vector=(ProposedIVector) values;
    	}else{
    		this.vector=new ProposedVector(values,false);
    	}
    	this.totalSize=item.getArray().length();
    }

    /**
     * To simplify creation of a TreeVector from a list of scalar parameters
     * @param id
     * @param parameterIds
     * @param parameterValues
     */
    public ProposedTreeVector(String id, String[] parameterIds, double[] parameterValues) {
        this(id);
        if (parameterIds.length != parameterValues.length) {
            throw new IllegalArgumentException("inconsistent #ids / #values");
        }
        for (int i = 0; i < parameterIds.length; i++) {
            ProposedITreeVector treeVector = new ProposedTreeVector(parameterIds[i], new ProposedVector(new double[]{parameterValues[i]}));
            subTreeVectors.add(treeVector);
            totalSize++;
        }
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getId() {
        return id;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    /**
     * Get the id's of the children in this TreeVector as child to the tree vector.
     * @return The id's of the children in this TreeVector as child to the tree vector.
     */
    public ArrayList<String> getSubTreeVectorIds() {
        ArrayList<String> childIds = new ArrayList<String>();
        for ( ProposedITreeVector treeVector : subTreeVectors) {
            childIds.add(treeVector.getId());
        }
        return childIds;
    }

    /**
     * Add a TreeVector as child to the tree vector.
     * @param subTreeVector The TreeVector to be added as child.
     */
    public void addChild(ProposedITreeVector subTreeVector) {
    	if(this.vector!=null){
    		throw new RuntimeException("addChild: This treevector is a leaf, since it contains a Vector");
    	}
        subTreeVectors.add(subTreeVector);
        totalSize += subTreeVector.length();
    }

    public void addChild(String childId, double[] childValues) {
    	if(this.vector!=null){
    		throw new RuntimeException("addChild: This treevector is a leaf, since it contains a Vector");
    	}
        ProposedTreeVector childTreeVector = new ProposedTreeVector(childId, new ProposedVector(childValues));
        subTreeVectors.add(childTreeVector);
        totalSize += childValues.length;
    }

    /**
     * Get a child in the tree vector, the child is known to be a Vector
     * (otherwise a run time exception will be thrown).
     * @param id indentifier of the child, may contain "/"'s to specify full path in tree.
     * @return Child Vector with identifier <code>id</code>.
     */
    public ProposedITreeVector getSubTreeVector(String id) {
        for ( ProposedITreeVector treeVector : subTreeVectors) {
            if (treeVector.getId().equals(id))  {
                return treeVector;
            }
        }
        throw new RuntimeException("TreeVector.getSubTreeVector(): Child " + id + " not found");
    }

    public void setExcludeFromVector(boolean excludeFromVector) {
        this.excludeFromVector = excludeFromVector;
    }

    public boolean excludeFromVector() {
        return excludeFromVector;
    }

    public void setConstant(double value) {
        if (vector != null) {
            vector.setConstant(value);
        } else {
            for (ProposedITreeVector subTreeVector : subTreeVectors) {
                subTreeVector.setConstant(value);
            }
        }
    }

    public void scale(double alpha) {
        if (vector != null) {
            vector.scale(alpha);
        } else {
            for (ProposedITreeVector subTreeVector : subTreeVectors) {
                subTreeVector.scale(alpha);
            }
        }
    }

    public void setValuesAsDoubles(double[] values) {
        if (vector != null) {
            vector.setValuesAsDoubles(values);
        } else {
            int startOfSubArray = 0;
            for (ProposedITreeVector subTreeVector : subTreeVectors) {
                double[] subArrayValues = new double[subTreeVector.length()];
                System.arraycopy(values, startOfSubArray, subArrayValues, 0, subArrayValues.length);
                subTreeVector.setValues(subArrayValues);
                startOfSubArray += subArrayValues.length;
            }
        }
    }

    public double[] getValuesAsDoubles(){
        if (vector != null) {
            return vector.getValuesAsDoubles();
        } else {
            double[] values = new double[totalSize];
            int startOfSubArray = 0;
            for (ProposedITreeVector subTreeVector : subTreeVectors) {
                double[] subArrayValues = subTreeVector.getValuesAsDoubles();
                System.arraycopy(subArrayValues, 0, values, startOfSubArray, subArrayValues.length);
                startOfSubArray += subArrayValues.length;
            }
            return values;
        }
    }

    public void setValueAsDouble(int index, double value){
        if (vector != null) {
            vector.setValueAsDouble(index, value);
        } else {
            int size = 0;
            for (ProposedITreeVector subTreeVector : subTreeVectors) {
                int prevSize = size;
                size += subTreeVector.length();
                if (index < size) {
                    subTreeVector.setValueAsDouble(index - prevSize, value);
                    return;
                }
            }
            throw new RuntimeException("TreeVector.getValue(): invalid index [" + index + "]");
        }
    }

    public double getValueAsDouble(int index){
        if (vector != null) {
            return vector.getValueAsDouble(index);
        } else {
            int size = 0;
            for (ProposedITreeVector subTreeVector : subTreeVectors) {
                int prevSize = size;
                size += subTreeVector.length();
                if (index < size) {
                    return subTreeVector.getValueAsDouble(index - prevSize);
                }
            }
            throw new RuntimeException("TreeVector.getValue(): invalid index [" + index + "]");
        }
    }

    public int length() {
        return totalSize;
    }

    public void axpy(double alpha, ProposedIVector x) {
        if (vector != null) {
            vector.axpy(alpha, x);
        } else if (x instanceof ProposedITreeVector){
        	// treeVectors are assumed to have identical structure
        	if(this.vector!=null){
        		this.vector.axpy(alpha, ((ProposedITreeVector) x).getVector());
        	}else{ // delegate work where possible
        		for (ProposedITreeVector subTreeVector : subTreeVectors) {
        			String id = subTreeVector.getId();
        			ProposedITreeVector xPart = ((ProposedITreeVector) x).getSubTreeVector(id);
        			subTreeVector.axpy(alpha, xPart);
        		}
        	}
        }else{
            int startOfSubArray = 0;
            for (ProposedITreeVector subTreeVector : subTreeVectors) {
                double[] subArrayX= x.getValuesAsDoubles(startOfSubArray, startOfSubArray+subTreeVector.length()-1);
                subTreeVector.axpy(alpha, new ProposedVector(subArrayX));
                startOfSubArray += subArrayX.length;
            }
        }
    }

    public double dotProduct(ProposedIVector otherVector) {
        if (vector != null) {
            return vector.dotProduct(otherVector);
        } else if(otherVector instanceof ProposedITreeVector){ //assume same structure
            double dotProd = 0;
       		for (ProposedITreeVector subTreeVector : subTreeVectors) {
    			String id = subTreeVector.getId();
    			ProposedITreeVector xPart = ((ProposedITreeVector) otherVector).getSubTreeVector(id);
    			dotProd+=subTreeVector.dotProduct(xPart);
       		}
       		return dotProd;
        } else { 
            double dotProd = 0;
            int startOfSubArray = 0;
            double[] otherValues = otherVector.getValuesAsDoubles();
            for (ProposedITreeVector subTreeVector : subTreeVectors) {
                double[] otherValuesSubArray = new double[subTreeVector.length()];
                System.arraycopy(otherValues, startOfSubArray, otherValuesSubArray, 0, otherValuesSubArray.length);
                dotProd += subTreeVector.dotProduct(new ProposedVector(otherValuesSubArray));
                startOfSubArray += otherValuesSubArray.length;
            }
            return dotProd;
        }
    }

    public double norm2() {
        if (vector != null) {
            return vector.norm2();
        } else {
            double sumSquares = 0;
            for (ProposedITreeVector subTreeVector : subTreeVectors) {
                sumSquares += Math.pow(subTreeVector.norm2(),2);
            }
            return Math.sqrt(sumSquares);
        }
    }

    public void pointwiseDivide(ProposedIVector otherVector) {
        if (vector != null) {
            vector.pointwiseDivide(otherVector);
        } else if(otherVector instanceof ProposedITreeVector){ //assume same structure
       		for (ProposedITreeVector subTreeVector : subTreeVectors) {
    			String id = subTreeVector.getId();
    			ProposedITreeVector xPart = ((ProposedITreeVector) otherVector).getSubTreeVector(id);
    			subTreeVector.pointwiseDivide(xPart);
       		}
        } else {
            int startOfSubArray = 0;
            double[] otherValues = otherVector.getValuesAsDoubles();
            for (ProposedITreeVector subTreeVector : subTreeVectors) {
                double[] otherValuesSubArray = new double[subTreeVector.length()];
                System.arraycopy(otherValues, startOfSubArray, otherValuesSubArray, 0, otherValuesSubArray.length);
                subTreeVector.pointwiseDivide(new ProposedVector(otherValuesSubArray));
                startOfSubArray += otherValuesSubArray.length;
            }
        }
    }

    public void pointwiseMultiply(ProposedIVector otherVector) {
        if (vector != null) {
            vector.pointwiseMultiply(otherVector);
        } else if(otherVector instanceof ProposedITreeVector){ //assume same structure
       		for (ProposedITreeVector subTreeVector : subTreeVectors) {
    			String id = subTreeVector.getId();
    			ProposedITreeVector xPart = ((ProposedITreeVector) otherVector).getSubTreeVector(id);
    			subTreeVector.pointwiseMultiply(xPart);
       		}
        } else {
            int startOfSubArray = 0;
            double[] otherValues = otherVector.getValuesAsDoubles();
            for (ProposedITreeVector subTreeVector : subTreeVectors) {
                double[] otherValuesSubArray = new double[subTreeVector.length()];
                System.arraycopy(otherValues, startOfSubArray, otherValuesSubArray, 0, otherValuesSubArray.length);
                subTreeVector.pointwiseMultiply(new ProposedVector(otherValuesSubArray));
                startOfSubArray += otherValuesSubArray.length;
            }
        }
    }

    public void sqrt() {
        if (vector != null) {
            vector.sqrt();
            return;
        }
        for (ProposedITreeVector subTreeVector : subTreeVectors) {
            subTreeVector.sqrt();
        }
    }

    @SuppressWarnings({"CloneDoesntCallSuperClone", "CloneDoesntDeclareCloneNotSupportedException"})
    @Override
    public ProposedTreeVector clone(){

        ProposedTreeVector out = new ProposedTreeVector(this.id);
        out.id = this.id;
        out.description = this.description;
        for ( ProposedITreeVector treeVector : subTreeVectors) {
            out.subTreeVectors.add(treeVector.clone());
        }
        out.vector = this.vector != null ? this.vector.clone() : null;
        out.totalSize = this.totalSize;
        out.exchangeItem=this.exchangeItem;
        out.timeInfo=this.timeInfo;
        out.geometryInfo=this.geometryInfo;
        out.role=this.role;
        out.excludeFromVector=this.excludeFromVector;
        
        return out;
    }

    public void free() {
        if (vector != null) {
            vector.free();
        } else {
            for (ProposedITreeVector subTreeVector : subTreeVectors) {
                subTreeVector.free();
            }
        }
    }

    public String toString() {
        String string = "TreeVector " + id + ((description!=null)?(!description.equals(id) ? ", " + description : ""):"");
        if ( subTreeVectors.size() > 0 ) {
            string += ", SubTreeVectors:";
            for ( ProposedITreeVector treeVector : subTreeVectors) {
                string += "\n\t" + treeVector.toString();
            }
        } else if ( vector != null ) {
            string += " " + vector.toString();
        }
        return string;
    }

    public String printString(String indent) {
        String string = "\n" + indent + id + (!description.equals(id) ? ", " + description : "");
        if (subTreeVectors.size() > 0) {
            string += " - containing:";
            for (ProposedITreeVector treeVector : subTreeVectors) {
                string += treeVector.printString(indent + "   ");
            }
        } else if (vector != null) {
            string += vector.printString("");
        }
        return string;
    }

//    public String[] getNames(int skip) {
//        String[] string = new String[skip + subTreeVectors.size()];
//        for (int i = 0; i < subTreeVectors.size(); i ++) {
//            string[skip+i] = subTreeVectors.get(i).getId();
//        }
//        return string;
//    }

    public void serialize(PrintStream outputStream) {
        if ( subTreeVectors.size() > 0 ) {
            outputStream.print("[");
            boolean printComma = false;
            for ( ProposedITreeVector treeVector : subTreeVectors) {
                if (printComma) {
                    outputStream.print(", ");
                } else {
                    printComma = true;
                }
                boolean printComma2 = false;
                for (int i = 0; i < treeVector.length(); i++) {
                	if(printComma2){
                    	outputStream.print(", ");
                	}else{
                		printComma2=true;
                	}
                    outputStream.print(treeVector.getValueAsDouble(i));
                }
            }
            outputStream.print("]");
        } else if ( vector != null ) {
            if (vector instanceof Matrix) {
                ((Matrix)vector).serialize(outputStream);
            } else if (vector instanceof Vector) {
                ((Vector)vector).serialize(outputStream);
            } else if (vector instanceof ProposedTreeVector) {
                ((ProposedTreeVector)vector).serialize(outputStream);
            } else {
                outputStream.print(vector.toString());
            }
        }
    }


	public int getNumberOfDimensions() {
		if(this.vector!=null){
			return this.vector.getNumberOfDimensions();
		}else{
			return 1;
		}
	}

	public int[] getDimensions() {
		if(this.vector!=null){
			return this.vector.getDimensions();
		}else{
			return new int[]{this.totalSize};
		}
	}

	public double[] getValuesAsDoubles(boolean copyValues) {
		if(this.vector!=null){
			return this.vector.getValuesAsDoubles(copyValues);
		}else{
			return this.getValuesAsDoubles();
		}
	}

	public double[] getValuesAsDoubles(int firstIndex, int lastIndex) {
		if(this.vector!=null){
			return this.vector.getValuesAsDoubles(firstIndex, lastIndex);
		}else{
			throw new UnsupportedOperationException("getValuesAsDoubles(int firstIndex, int lastIndex) not implented for TreeVector");
		}
	}

	public double getValueAsDouble(int[] indices) {
		if(this.vector!=null){
			return this.vector.getValueAsDouble(indices);
		}else{
			throw new UnsupportedOperationException("getValueAsDouble(int[] indices) not implented for TreeVector");
		}
	}

	public void setValuesAsDoubles(int firstIndex, int lastIndex, double[] values) {
		if(this.vector!=null){
			this.vector.setValuesAsDoubles(firstIndex, lastIndex, values);
		}else{
			throw new UnsupportedOperationException("setValuesAsDoubles(int firstIndex, int lastIndex, double[] values) not implented for TreeVector");
		}
	}

	public void setValueAsDouble(int[] indices, double value) {
		if(this.vector!=null){
			this.vector.setValueAsDouble(indices, value);
		}else{
			throw new UnsupportedOperationException("setValueAsDouble(int[] indices, double value) not implented for TreeVector");
		}
	}

	public void axpyOnValues(double alpha, double[] axpyValues) {
		this.axpy(alpha, new ProposedVector(axpyValues));
	}

	public void multiplyValues(double[] multiplicationFactors) {
		pointwiseMultiply(new ProposedVector(multiplicationFactors));
	}

	public void reshape(int[] dimensions) {
		if(this.vector!=null){
			this.vector.reshape(dimensions);
		}else{
			throw new UnsupportedOperationException("reshape(int[] dimensions) not implented for TreeVector");
		}		
	}

	public boolean allowsGrowingFirstDimension() {
		if(this.vector!=null){
			return this.vector.allowsGrowingFirstDimension();
		}else{
			return false;
		}		
	}

	public IArray getSlice(int dimension, int index) {
		if(this.vector!=null){
			return this.vector.getSlice(dimension, index);
		}else{
			throw new UnsupportedOperationException("IArray getSlice(int dimension, int index) not implented for TreeVector");
		}		
	}

	public IArray getSlice(int dimension, int minIndex, int maxIndex) {
		if(this.vector!=null){
			return this.vector.getSlice(dimension, minIndex, maxIndex);
		}else{
			throw new UnsupportedOperationException("getSlice(int dimension, int minIndex, int maxIndex) not implented for TreeVector");
		}		
	}

	public double[] getSliceAsDoubles(int dimension, int minIndex, int maxIndex) {
		if(this.vector!=null){
			return this.vector.getSliceAsDoubles(dimension, minIndex, maxIndex);
		}else{
			throw new UnsupportedOperationException("getSliceAsDoubles(int dimension, int minIndex, int maxIndex) not implented for TreeVector");
		}		
	}

	public void setSlice(IArray slice, int dimension, int index) {
		if(this.vector!=null){
			this.vector.setSlice(slice, dimension, index);
		}else{
			throw new UnsupportedOperationException("setSlice(IArray slice, int dimension, int index) not implented for TreeVector");
		}		
	}

	public void setSlice(double[] slice, int dimension, int index) {
		if(this.vector!=null){
			this.vector.setSlice(slice, dimension, index);
		}else{
			throw new UnsupportedOperationException("setSlice(double[] slice, int dimension, int index) not implented for TreeVector");
		}
	}

	public void setSlice(IArray slice, int dimension, int minIndex, int maxIndex) {
		if(this.vector!=null){
			this.vector.setSlice(slice, dimension, minIndex, maxIndex);
		}else{
			throw new UnsupportedOperationException("setSlice(IArray slice, int dimension, int minIndex, int maxIndex) not implented for TreeVector");
		}		
	}

	public void setSlice(double[] slice, int dimension, int minIndex, int maxIndex) {
		if(this.vector!=null){
			this.vector.setSlice(slice, dimension, minIndex, maxIndex);
		}else{
			throw new UnsupportedOperationException("setSlice(double[] slice, int dimension, int minIndex, int maxIndex) not implented for TreeVector");
		}		
	}

	public int valueIndex(int[] indices) {
		if(this.vector!=null){
			return this.vector.valueIndex(indices);
		}else{
			throw new UnsupportedOperationException("valueIndex(int[] indices) not implented for TreeVector");
		}		
	}

	public IArray getArray() {
		if(this.vector!=null){
			return this.vector.getArray();
		}else{
			throw new UnsupportedOperationException("getArray() not implented for TreeVector");
		}		
	}

	public void setArray(IArray array) {
		if(this.subTreeVectors.size()>0){
			throw new RuntimeException("setArray: This TreeVector already has children, so it cannot be a leaf.");
		}
		if(array instanceof ProposedIVector){
			this.vector=(ProposedIVector) array;
		}else{
			this.vector=new ProposedVector(array,false);
		}
	}

	public Role getRole() {
		return this.role;
	}

	public void copyValuesFromItem(IExchangeItem sourceItem) {
		//TODO create more complex actions later that make use of the metadata
		this.setValuesAsDoubles(sourceItem.getValuesAsDoubles());
	}

	public ITimeInfo getTimeInfo() {
		return this.timeInfo;
	}

	public void setTimeInfo(ITimeInfo info) {
		this.timeInfo=info;
	}

	public IQuantityInfo getQuantityInfo() {
		return this.quantityInfo;
	}

	public void setQuantityInfo(IQuantityInfo info) {
		this.quantityInfo=info;
	}

	public IGeometryInfo getGeometryInfo() {
		return this.geometryInfo;
	}

	public void setGeometryInfo(IGeometryInfo info) {
		this.geometryInfo=info;
	}

	public Class getValueType() {
		// TODO Remove later, only needed for IPrevExchangeItem
		throw new UnsupportedOperationException("getValueType deprecated");
	}

	public void setValues(Object values) {
		// TODO Remove later, only needed for IPrevExchangeItem
		throw new UnsupportedOperationException("setValues deprecated");
	}

	public double[] getTimes() {
		// TODO Remove later, only needed for IPrevExchangeItem
		throw new UnsupportedOperationException("getTimes deprecated");
	}

	public void setTimes(double[] times) {
		// TODO Remove later, only needed for IPrevExchangeItem
		throw new UnsupportedOperationException("setTimes deprecated");
	}

	public boolean hasSubTreeVectors() {
		if(this.vector==null){
			if(this.subTreeVectors.size()>0){
				return true;
			}else{
				return false;
			}
		}else{
			return false;
		}
	}

	public ProposedIVector getVector() {
		return this.vector.clone();
	}

	public ValueType getValuesType() {
		if(this.vector!=null){
			return ValueType.IArrayType;
		}else{
			return ValueType.doublesType;
		}		
	}

	public Object getValues() {
		if(this.vector!=null){
			return this.vector;
		}else{
			return getValuesAsDoubles();
		}		
	}
}
