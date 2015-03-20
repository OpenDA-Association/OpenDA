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

import org.openda.interfaces.RelationTable;
import org.openda.interfaces.Vector;

public class CtaRelationTable extends CtaObject implements RelationTable {

	
 	  /** 
	    *  Constructor for a relation table that defines a selection of elements
	    *
	    * @param select    (integer) array with indices of elements
	    *                   from the target set that are selected.
	    *
	    */
	   public CtaRelationTable(int[] select){
           this.ctaHandle=ctaCreate();
		   CtaVector vSelect=new CtaVector(select.length);
           double [] selectAsDouble = new double[select.length];
           for (int i = 0; i < selectAsDouble.length; i++) {
               selectAsDouble[i] = select[i];
           }
           vSelect.setValues(selectAsDouble);
		   this.SetSelect(vSelect);
	   }


	   /** 
	    * Constructor for a relation table that is combination of two  relation tables.
	    *
	    * Set a relation table that is the combination of two existing relation
	    * tables. It is possible to use the inverse of the relation tables when
	    * needed.
	    * A useful application of this method is to create a relation table that
	    * defines a relation between a subset of elements from set1 and a subset of
	    * the elements of set2. In order to set a relation table of this kind first
	    * create two relation tables:
	    * hrel1 elements from set 1 that have a relation with the elements from set 2,
	    * hrel2 elements from set 2 that have a relation with the elements from set 1
	    *
	    * The combined relation table of hrel1 and inverse(hrel2) is a relation
	    * table that specifies the relation of a subset of elements from set1 and a
	    * subset of elements from set2.
	    *
	    * @param hrel1      I first relation table 
	    * @param inverse1   I use inverse of hrel1 (true/false)
	    * @param hrel2      I first relation table 
	    * @param inverse2   I use inverse of hrel2 (true/false)
	    *
	    */
	   public CtaRelationTable(RelationTable hrel1, boolean inverse1, 
	                        RelationTable hrel2, boolean inverse2 ){
		   this.ctaHandle=ctaCreate();
		   this.SetTableCombine(hrel1, inverse1, hrel2, inverse2 );
	   }
	    	   
	   private native int ctaCreate();
       private native void SetTableCombine(RelationTable hrel1, boolean inverse1, 
               RelationTable hrel2, boolean inverse2 );
       
       private native void SetSelect(Vector vselect);
	   public native void apply(Vector hfrom, Vector hto);
	   public native void applyInv(Vector hfrom, Vector hto);

	   public native void free();
		// TODO Auto-generated me

}
