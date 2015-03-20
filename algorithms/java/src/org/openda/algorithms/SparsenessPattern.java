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


package org.openda.algorithms;

import org.openda.utils.Results;

/**
 * OpenDA Sparse Dud algorithm
 */
public class SparsenessPattern{

	// The sparseness pattern is stored in several formats, so that
	// the necessary information is always directly at hand. 
	
    //-----------------------------------------------------------------------
	// 1. Full matrix form of the sparseness pattern
    private int[][] nonZeros;
    //    Dimension of the matrix
    private int nrows;
    private int ncols;
    public int max_nnz_per_row;
    
    //-----------------------------------------------------------------------
    // 2. Row-oriented form of the sparseness pattern
    //    
    //    The nonzeros on row irow are found at
    //    for (int j=0; j<columnNumbers[irow].length; j++)
    //    {
    //        int icol = columnNumbers[irow][j];
    //        // Now: nonZeros[irow][icol] ==1
    //    }
    private int[][] columnNumbers;  
    //-----------------------------------------------------------------------
    // 3. Compressed row-oriented form of the sparseness pattern
    
    // Each of the arrays in the double array columnNumber is equal to exactly
    // one array in columnPatterns
    //  NB: array columnPaterns is usually much smaller than columnNumbers
    public int[][] columnPatterns;
    // row2Patterns can be used to find the (large) array columnNumbers from the
    // (small) array columnPatterns
    //    columnNumbers[irow] is equal to columnPatterns[rows2Patterns[irow]]
    private int[]   rows2Patterns;
    // Inverse table:
    //    The pattern ipat is the pattern of the rows patterns2Rows[ipat]:
    //    
    //    for (int j=0; j<patterns2Rows[ipat]; j++)
    //    {
    //          int icol = patterns2Rows[ipat][j];
    //          // Now: columnNumbers[icol].equals(columnPatterns[ipat]);
    //    }
    public int[][] patterns2Rows;
    //-----------------------------------------------------------------------
    
    private boolean issame(int[] a, int[] b)
    {
    	if (a.length != b.length) {return false;};
    	for (int j=0; j<a.length; j++)
    	{
    		if (a[j] != b[j]){return false;};
    	}
    	return true;
    }
  
    /**
     * Constructor function: calculate the 3 storage formats from the input (full 
     * matrix format)
     * 
     * @param nonZerosInput
     */
    public SparsenessPattern(int[][] nonZerosInput){
       
       // 1: copy the full matrix format
       nonZeros = nonZerosInput;
       nrows = nonZeros.length;
       ncols = nonZeros[0].length;
       
       // 2: calculate the row oriented storage 
       columnNumbers = new int[nrows][];
       for (int irow = 0; irow<nrows; irow++)
       {
    	   // Count the number of nonzeros in this row
    	   int ncolsUsed = 0;
    	   for (int icol = 0; icol<ncols; icol++)
    	   {
    		   ncolsUsed += nonZeros[irow][icol];
    	   }
    	   
    	   // Allocate and fill the array for this row
     	   columnNumbers[irow] = new int[ncolsUsed];
     	   ncolsUsed = 0;
   	       for (int icol = 0; icol<ncols; icol++)
   	       {
   	    	   if (nonZeros[irow][icol]==1)
   	    	   {
   	    	      columnNumbers[irow][ncolsUsed] = icol;
   		          ncolsUsed++;
   	    	   }
   	       }
       };
       
       // 3: Calculate the compressed row-oriented storage
       int ncolPatterns = 0;
       rows2Patterns = new int[nrows];
       
       final int UNKNOWN = -1;
       //    Initialize: all patterns still unknown
       for (int irow=0; irow<nrows; irow++ )
       {
    	   rows2Patterns[irow]= UNKNOWN; 
       }
       
       for (int irow=0; irow<nrows; irow++ )
       {
    	   if (rows2Patterns[irow]==UNKNOWN)
    	   {
    		   // New pattern found; administrate this pattern and all other occurrences
    		   rows2Patterns[irow] = ncolPatterns;
               for (int jrow=irow+1; jrow<nrows; jrow++)
               {
            	   if (issame(columnNumbers[irow],columnNumbers[jrow]))
            	   {
            		   rows2Patterns[jrow] = ncolPatterns;    
            	   }
               }
    		   ncolPatterns++;
    	   }
       }
        	  
       // Now that the number of patterns is known, allocate the other arrays
       columnPatterns = new int[ncolPatterns][];
       patterns2Rows  = new int[ncolPatterns][];
       for (int ipat=0; ipat<ncolPatterns; ipat++)
       {
    	  // Count the number of rows that have this pattern
    	  int nrowsForThisPattern=0;
          for (int irow=0; irow<nrows; irow++)
          {
        	  if (rows2Patterns[irow]==ipat) {nrowsForThisPattern++;}
          }
          
          // Allocate and fill the list of rows that have this pattern
          patterns2Rows[ipat] = new int [nrowsForThisPattern];
          nrowsForThisPattern=0;
          for (int irow=0; irow<nrows; irow++)
          {
        	  if (rows2Patterns[irow]==ipat) 
        	  {
        		  patterns2Rows[ipat][nrowsForThisPattern] = irow;
        		  nrowsForThisPattern++;
        	  }
          }
   
          // Store the first occurrence of the pattern in the pattern array
          int irow = patterns2Rows[ipat][0];
          columnPatterns[ipat] = columnNumbers[irow];
          
       }
       
       max_nnz_per_row = 0;
       for (int ipat=0; ipat<ncolPatterns; ipat++)
       {
    	  max_nnz_per_row = Math.max(columnPatterns[ipat].length, max_nnz_per_row); 
       }
       
       Results.putProgression("I have found the following patterns:");
    	 
       for (int ipat=0; ipat<ncolPatterns; ipat++)
       {
    	  String line = "   ";
    	  for (int j=0; j<columnPatterns[ipat].length; j++)
    	  {
    	      line += columnPatterns[ipat][j]+"  ";
    	  }
       	  Results.putProgression(line);
       }
    }
   

}
