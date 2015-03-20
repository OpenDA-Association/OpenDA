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


/*
 *
 * Interface description of the COSTA default CtaTreeVector component.
 *
 * The CtaTreeVector is an extension of a vector component. A treevector
 * either contains a single vector or is a concatenation of a number of
 * treevectors, called sub-treevectors in this context. The usage of
 * sub-treevectors makes is possible to concatenate models or extend models
 * as is done when a deterministic model is extended into a stochastic model.
 * The sub-treevector are also very useful inside the model source code where the
 * whole treevector is not represented by a single vector.  The default tree-vector
 * component uses COSTA vector-components for storing the values.
 *
 * Each (sub) treevector has a id. CtaTreeVector vectors having the same id are considered
 * to be the same, meaning they have the same buildup in subtreevectors, length and
 * datatypes.
 *
 * ----------------------------------------------------------------
 */

import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IDimensionIndex;
import org.openda.interfaces.IVector;

import java.util.ArrayList;

/**
 * Tree Vector
 */
public class CtaTreeVector extends CtaVector implements ITreeVector {

	// create the treevector only using the handle of a costa treevector.
	// Note we do not change handle count. Function only used for the implementation of native
	// wrapping
	public CtaTreeVector(int ctaHandle){
	   this.ctaHandle=ctaHandle;
    }

	public CtaTreeVector(String id, String tag, IVector otherVector){
         if ( otherVector instanceof CtaVector ) {
            int hOtherVector = ((CtaVector)otherVector).ctaHandle;
            this.ctaHandle=this.ctaCreateFromVector(id,tag,hOtherVector);
         } else {
            throw new RuntimeException("ctatreevector: create-with-vector only allowed with ctaVector ");
         }
    }

   // public CtaTreeVector(String id, String tag, int[] ctaVectorHandles){
   //       int n = ctaVectorHandles.length;
//	      this.ctaHandle=this.ctaCreateFromSubtreevectors(id, tag,n, ctaVectorHandles);
//
//    }

    public CtaTreeVector(String id, String tag, CtaTreeVector[] ctaSubTreeVectors){
          int n = ctaSubTreeVectors.length;

          // Get COSTA handles
          int[] ctaVectorHandles = new int[n];
          for (int i=0; i<n;i++){
             ctaVectorHandles[i]= ctaSubTreeVectors[i].gethandle();
          }
	      this.ctaHandle=this.ctaCreateFromSubtreevectors(id, tag,n, ctaVectorHandles);
    }




	public CtaTreeVector() {
	}


	public ITreeVector getSubTreeVector(String id) {

		int handle=ctaGetSubTreeVector(id);

		CtaTreeVector subtree=new CtaTreeVector();
		subtree.ctaHandle=handle;

		return subtree;
	}

    public int gethandle(){
		return this.ctaHandle;
	}

    public int export(int ctafilehandle) {
		return ctaExport(ctafilehandle);
    }

	public int TVimport(String netcdfname) {
	   int ctafilehandle = ctaNetcdfInit(netcdfname,"r");
	   int ierr = ctaImport(ctafilehandle);
	   if (ierr == 0){
          ierr = ctaNetcdfClose(ctafilehandle);
	   }
        return ierr;
    }
	 //import a treevector in flat format: TV contains only one vector
	 // this is handy if the tree-structure of the importing TV is not known
	 // AND not needed.
	public int Vimport(String netcdfname) {
	   int ctafilehandle = ctaNetcdfInit(netcdfname,"r");
	   int ierr = ctaVImport(ctafilehandle);
	   if (ierr == 0){
          ierr = ctaNetcdfClose(ctafilehandle);
	   }
        return ierr;
	}


    public int setReggrid(int nx, int ny, int nz, double x0, double y0, double z0, double dx, double dy, double dz){
		return ctaSetRegGrid(nx,ny,nz,x0,y0,z0,dx,dy,dz);

    }


    public IDimensionIndex[] getDimensionIndices() {
        throw new UnsupportedOperationException("org.costa.CtaTreeVector.getDimensionIndices(): Not implemented yet.");
    }

    public boolean excludeFromVector() {
        return false; // Todo: implement exclusion functionality
    }

    public native String getId();

    //public native Vector getChildVector(String id);

    private native int ctaCreateFromVector(String id, String tag, int somevector);
    private native int ctaCreateFromSubtreevectors(String id, String tag, int n, int[] handles);
    private native int ctaGetSubTreeVector(String id);
    private native int ctaExport(int ctafilehandle);
    private native int ctaSetRegGrid(int nx, int ny, int nz, double x0,
                                      double y0, double z0, double dx, double dy, double dz);
    private native int ctaImport(int ctafilehandle);
    private native int ctaVImport(int ctafilehandle);

	private native int ctaGetNumSubTreeVectors();
	private native String ctaGetSubTreeVectorId(int index );

	public native int ctaNetcdfInit(String netcdfname, String action);

    public native int ctaNetcdfClose(int ctafilehandle);



    public String getCaption(){
		return getId();
	}

    public String getDescription() {
        return null;
    }


    @SuppressWarnings({"CloneDoesntCallSuperClone", "CloneDoesntDeclareCloneNotSupportedException"})
    @Override
    public native ITreeVector clone();

    public ArrayList<String> getSubTreeVectorIds() {
		int n = ctaGetNumSubTreeVectors();
		ArrayList<String> childIds = new ArrayList<String>();
		for (int i=0; i< n ;i++){
		   String tag = ctaGetSubTreeVectorId(i);
			childIds.add(tag);

		}
   //     throw new UnsupportedOperationException("org.costa.CtaTreeVector.getSubTreeVectorIds(): Not implemented yet.");
	return childIds;
    }
}

