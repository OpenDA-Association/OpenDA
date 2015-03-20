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

import org.openda.interfaces.IDimensionIndex;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;

import java.io.PrintStream;
import java.util.ArrayList;

/**
 * Simple implementation of interface : TreeVector
 */
public class TreeVector implements ITreeVector {

    boolean warnInefficient=false;
    private String id;
    private String caption;

    ArrayList<ITreeVector> subTreeVectors = new ArrayList<ITreeVector>();
    IVector vector = null;
    private IDimensionIndex[] dimensions = null;
    private int totalSize = 0;
    private boolean excludeFromVector = false;
    private String description = null;

    public TreeVector(String id) {
        super();
		String actualId = (id != null) ? id : "TV";
        this.caption = this.id = actualId;
    }

    public TreeVector(String id, String caption) {
        this(id);
        this.caption = caption;
    }

    public TreeVector(String Id, IVector vector) {
        this(Id);
        this.vector = vector;
        this.totalSize = vector.getSize();
        this.dimensions = new IDimensionIndex[]{new DimensionIndex(this.totalSize)};
    }

    public TreeVector(String id, String caption, IVector vector) {
        this(id, vector);
        this.caption = caption;
    }

    public TreeVector(String Id, IVector vector, int iSize, int jSize) {
        this(Id);
        this.vector = vector;
        this.totalSize = iSize*jSize;
        this.dimensions = new IDimensionIndex[]{new DimensionIndex(iSize), new DimensionIndex(jSize)};
    }

    public TreeVector(String Id, IVector vector, int iSize, int jSize, int kSize) {
        this(Id);
        this.vector = vector;
        this.totalSize = iSize*jSize*kSize;
        this.dimensions = new IDimensionIndex[]{new DimensionIndex(iSize), new DimensionIndex(jSize), new DimensionIndex(kSize)};
    }

    public TreeVector(String Id, IVector vector, IDimensionIndex[] dimensionIndices) {
        this(Id);
        this.vector = vector;
        this.totalSize = 1;
        for (IDimensionIndex dimensionIndex : dimensionIndices) {
            this.totalSize *= dimensionIndex.getSize();
        }
        this.dimensions = dimensionIndices;
    }

    public TreeVector(String id, String[] parameterIds, double[] parameterValues) {
        this(id);
        if (parameterIds.length != parameterValues.length) {
            throw new IllegalArgumentException("inconsistent #ids / #values");
        }
        for (int i = 0; i < parameterIds.length; i++) {
            ITreeVector treeVector = new TreeVector(parameterIds[i], new Vector(new double[]{parameterValues[i]}));
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

    public void setCaption(String caption) {
        this.caption = caption;
    }

    public String getCaption() {
        return caption;
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
        for ( ITreeVector treeVector : subTreeVectors) {
            childIds.add(treeVector.getId());
        }
        return childIds;
    }

    /**
     * Add a TreeVector as child to the tree vector.
     * @param subTreeVector The TreeVector to be added as child.
     */
    public void addChild(ITreeVector subTreeVector) {
        subTreeVectors.add(subTreeVector);
        totalSize += subTreeVector.getSize();
    }

    public void addChild(String childId, double[] childValues) {
        TreeVector childTreeVector = new TreeVector(childId, new Vector(childValues));
        subTreeVectors.add(childTreeVector);
        totalSize += childValues.length;
    }

    /**
     * Get a child in the tree vector, the child is known to be a Vector
     * (otherwise a run time exception will be thrown).
     * @param id indentifier of the child, may contain "/"'s to specify full path in tree.
     * @return Child Vector with identifier <code>id</code>.
     */
    public ITreeVector getSubTreeVector(String id) {
        for ( ITreeVector treeVector : subTreeVectors) {
            if (treeVector.getId().equals(id))  {
                return treeVector;
            }
        }
        throw new RuntimeException("TreeVector.getSubTreeVector(): Child " + id + " not found");
    }

    public IDimensionIndex[] getDimensionIndices() {
        return dimensions;
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
            for (ITreeVector subTreeVector : subTreeVectors) {
                subTreeVector.setConstant(value);
            }
        }
    }

    public void scale(double alpha) {
        if (vector != null) {
            vector.scale(alpha);
        } else {
            for (ITreeVector subTreeVector : subTreeVectors) {
                subTreeVector.scale(alpha);
            }
        }
    }

    public void setValues(double[] values) {
        if (vector != null) {
            vector.setValues(values);
        } else {
            int startOfSubArray = 0;
            for (ITreeVector subTreeVector : subTreeVectors) {
                double[] subArrayValues = new double[subTreeVector.getSize()];
                System.arraycopy(values, startOfSubArray, subArrayValues, 0, subArrayValues.length);
                subTreeVector.setValues(subArrayValues);
                startOfSubArray += subArrayValues.length;
            }
        }
    }

    public double[] getValues(){
        if (vector != null) {
            return vector.getValues();
        } else {
            double[] values = new double[totalSize];
            int startOfSubArray = 0;
            for (ITreeVector subTreeVector : subTreeVectors) {
                double[] subArrayValues = subTreeVector.getValues();
                System.arraycopy(subArrayValues, 0, values, startOfSubArray, subArrayValues.length);
                startOfSubArray += subArrayValues.length;
            }
            return values;
        }
    }

    public void setValue(int index, double value){
        if (vector != null) {
            vector.setValue(index, value);
        } else {
            int size = 0;
            for (ITreeVector subTreeVector : subTreeVectors) {
                int prevSize = size;
                size += subTreeVector.getSize();
                if (index < size) {
                    subTreeVector.setValue(index - prevSize, value);
                    return;
                }
            }
            throw new RuntimeException("TreeVector.getValue(): invalid index [" + index + "]");
        }
    }

    public double getValue(int index){
        if (vector != null) {
            return vector.getValue(index);
        } else {
            int size = 0;
            for (ITreeVector subTreeVector : subTreeVectors) {
                int prevSize = size;
                size += subTreeVector.getSize();
                if (index < size) {
                    return subTreeVector.getValue(index - prevSize);
                }
            }
            throw new RuntimeException("TreeVector.getValue(): invalid index [" + index + "]");
        }
    }

    public int getSize() {
        return totalSize;
    }

    public void axpy(double alpha, IVector x) {
	  boolean extractMethod=false;
	   if (x instanceof TreeVector){
		   TreeVector xl = (TreeVector) x;
		    if (vector != null){
				if (xl.vector != null){
					vector.axpy(alpha, xl.vector);
				}
				else {
					vector.axpy(alpha, x);
				}
			}
		    // Move into all sub-tree vectors
		   if (xl.subTreeVectors.size()==subTreeVectors.size()){
			   for (int iSub=0; iSub<subTreeVectors.size(); iSub++){
				   subTreeVectors.get(iSub).axpy(alpha, xl.subTreeVectors.get(iSub));
			   }

		   }
		   else {
		      extractMethod=true;
		   }
	   }
	   else {
		   extractMethod=true;
	   }
		//Fallback things are not of the same kind, just make it work but slow
       if (extractMethod){
		   if (warnInefficient){
		      System.out.println("TreeVector.axpy slow method :-( "+this.getSize());
		   }
    		if (vector != null) {
              	vector.axpy(alpha, x);
        	} else {
            	int startOfSubArray = 0;
            	double[] valuesX = x.getValues();
            	for (ITreeVector subTreeVector : subTreeVectors) {
                	double[] subArrayX= new double[subTreeVector.getSize()];
                	System.arraycopy(valuesX, startOfSubArray, subArrayX, 0, subArrayX.length);
                	subTreeVector.axpy(alpha, new Vector(subArrayX));
                	startOfSubArray += subArrayX.length;
            	}
        	}
	   }
    }

    public double dotProduct(IVector otherVector) {
	  boolean extractMethod=false;
		double dotProd=0.0;
	   if (otherVector instanceof TreeVector){
		   TreeVector xl = (TreeVector) otherVector;
		    if (vector != null){
				if (xl.vector != null){
					dotProd=vector.dotProduct(xl.vector);
				}
				else {
					dotProd=vector.dotProduct(otherVector);
				}
			}
		    // Move into all sub-tree vectors
		   if (xl.subTreeVectors.size()==subTreeVectors.size()){
			   for (int iSub=0; iSub<subTreeVectors.size(); iSub++){
				   dotProd+=subTreeVectors.get(iSub).dotProduct(xl.subTreeVectors.get(iSub));
			   }
		   }
		   else {
		      extractMethod=true;
		   }
	   }
	   else {
		   extractMethod=true;
	   }

       if (extractMethod){
		   if (warnInefficient){
		   	System.out.println("TreeVector.dotProduct slow method :-( "+this.getSize());
		   }
			if (vector != null) {
            	dotProd = vector.dotProduct(otherVector);
        	} else {
            	int startOfSubArray = 0;
            	double[] otherValues = otherVector.getValues();
            	for (ITreeVector subTreeVector : subTreeVectors) {
                	double[] otherValuesSubArray = new double[subTreeVector.getSize()];
                	System.arraycopy(otherValues, startOfSubArray, otherValuesSubArray, 0, otherValuesSubArray.length);
                	dotProd += subTreeVector.dotProduct(new Vector(otherValuesSubArray));
                	startOfSubArray += otherValuesSubArray.length;
            	}

        	}
	   }
		return dotProd;
    }

    public double norm2() {
        if (vector != null) {
            return vector.norm2();
        } else {
            double sumSquares = 0;
            for (ITreeVector subTreeVector : subTreeVectors) {
                sumSquares += Math.pow(subTreeVector.norm2(),2);
            }
            return Math.sqrt(sumSquares);
        }
    }

    public void pointwiseDivide(IVector otherVector) {
		boolean extractMethod=false;
		 if (otherVector instanceof TreeVector){
			 TreeVector xl = (TreeVector) otherVector;
			  if (vector != null){
				  if (xl.vector != null){
					  vector.pointwiseDivide(xl.vector);
				  }
				  else {
					  vector.pointwiseDivide(otherVector);
				  }
			  }
			  // Move into all sub-tree vectors
			 if (xl.subTreeVectors.size()==subTreeVectors.size()){
				 for (int iSub=0; iSub<subTreeVectors.size(); iSub++){
					 subTreeVectors.get(iSub).pointwiseDivide(xl.subTreeVectors.get(iSub));
				 }
			 }
			 else {
				extractMethod=true;
			 }
		 }
		 else {
			 extractMethod=true;
		 }
		if (extractMethod){
			if (warnInefficient){
				System.out.println("TreeVector.pointwiseDivide slow method :-( "+this.getSize());
			}
        	if (vector != null) {
            	vector.pointwiseDivide(otherVector);
        	} else {
            	int startOfSubArray = 0;
            	double[] otherValues = otherVector.getValues();
            	for (ITreeVector subTreeVector : subTreeVectors) {
                	double[] otherValuesSubArray = new double[subTreeVector.getSize()];
                	System.arraycopy(otherValues, startOfSubArray, otherValuesSubArray, 0, otherValuesSubArray.length);
                	subTreeVector.pointwiseDivide(new Vector(otherValuesSubArray));
                	startOfSubArray += otherValuesSubArray.length;
            	}
        	}
		}
    }

    public void pointwiseMultiply(IVector otherVector) {
		boolean extractMethod=false;
		 if (otherVector instanceof TreeVector){
			 TreeVector xl = (TreeVector) otherVector;
			  if (vector != null){
				  if (xl.vector != null){
					  vector.pointwiseMultiply(xl.vector);
				  }
				  else {
					  vector.pointwiseMultiply(otherVector);
				  }
			  }
			  // Move into all sub-tree vectors
			 if (xl.subTreeVectors.size()==subTreeVectors.size()){
				 for (int iSub=0; iSub<subTreeVectors.size(); iSub++){
					 subTreeVectors.get(iSub).pointwiseMultiply(xl.subTreeVectors.get(iSub));
				 }
			 }
			 else {
				extractMethod=true;
			 }
		 }
		 else {
			 extractMethod=true;
		 }
		if (extractMethod){
			if (warnInefficient){
				System.out.println("TreeVector.pointwiseMultiply slow method :-( "+this.getSize());
			}
	        if (vector != null) {
    	        vector.pointwiseMultiply(otherVector);
        	} else {
            	int startOfSubArray = 0;
           		double[] otherValues = otherVector.getValues();
            	for (ITreeVector subTreeVector : subTreeVectors) {
                	double[] otherValuesSubArray = new double[subTreeVector.getSize()];
                	System.arraycopy(otherValues, startOfSubArray, otherValuesSubArray, 0, otherValuesSubArray.length);
                	subTreeVector.pointwiseMultiply(new Vector(otherValuesSubArray));
                	startOfSubArray += otherValuesSubArray.length;
            	}
			}
        }
    }

    public void sqrt() {
        if (vector != null) {
            vector.sqrt();
            return;
        }
        for (ITreeVector subTreeVector : subTreeVectors) {
            Vector vector = new Vector(subTreeVector.getValues());
            vector.sqrt();
            subTreeVector.setValues(vector.getValues());
        }
    }

    @SuppressWarnings({"CloneDoesntCallSuperClone", "CloneDoesntDeclareCloneNotSupportedException"})
    @Override
    public TreeVector clone(){

        TreeVector out = new TreeVector(this.id);
        out.id = this.id;
        out.caption = this.caption;
        for ( ITreeVector treeVector : subTreeVectors) {
            out.subTreeVectors.add(treeVector.clone());
        }
        out.vector = this.vector != null ? this.vector.clone() : null;
        out.totalSize = this.totalSize;
        out.dimensions = this.dimensions;
        return out;
    }

    public void free() {
        if (vector != null) {
            vector.free();
        } else {
            for (ITreeVector subTreeVector : subTreeVectors) {
                subTreeVector.free();
            }
        }
    }

    public String toString() {
        String string = "TreeVector " + id + ((caption!=null)?(!caption.equals(id) ? ", " + caption : ""):"");
        if ( subTreeVectors.size() > 0 ) {
            string += ", SubTreeVectors:";
            for ( ITreeVector treeVector : subTreeVectors) {
                string += "\n\t" + treeVector.toString();
            }
        } else if ( vector != null ) {
            string += " " + vector.toString();
        }
        return string;
    }

    public String printString(String indent) {
        String string = "\n" + indent + id + (!caption.equals(id) ? ", " + caption : "");
        if (subTreeVectors.size() > 0) {
            string += " - containing:";
            for (ITreeVector treeVector : subTreeVectors) {
                string += treeVector.printString(indent + "   ");
            }
        } else if (vector != null) {
            string += vector.printString("");
        }
        return string;
    }

    public void serialize(PrintStream outputStream) {
        if ( subTreeVectors.size() > 0 ) {
            outputStream.print("[");
            boolean printComma = false;
            for ( ITreeVector treeVector : subTreeVectors) {
                if (printComma) {
                    outputStream.print(", ");
                } else {
                    printComma = true;
                }
                boolean printComma2 = false;
                for (int i = 0; i < treeVector.getSize(); i++) {
                	if(printComma2){
                    	outputStream.print(", ");
                	}else{
                		printComma2=true;
                	}
                    outputStream.print(treeVector.getValue(i));
                }
            }
            outputStream.print("]");
        } else if ( vector != null ) {
            if (vector instanceof Matrix) {
                ((Matrix)vector).serialize(outputStream);
            } else if (vector instanceof Vector) {
                ((Vector)vector).serialize(outputStream);
            } else if (vector instanceof TreeVector) {
                ((TreeVector)vector).serialize(outputStream);
            } else {
                outputStream.print(vector.toString());
            }
        }
    }
}
