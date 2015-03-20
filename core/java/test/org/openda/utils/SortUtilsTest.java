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

/**
 * TODO: class description
 * Main class for testing vector class
 */


import java.util.Arrays;

import junit.framework.TestCase;
import org.openda.utils.SortUtils;


/**
 * Test for simple vector
 */
public class SortUtilsTest extends TestCase {

    public static void testSorting_1() {
        System.out.println("==============================================================================");
        double values1[] = {1.0, 3.0, 2.0, 5.0, 4.0};
        int index1[] = SortUtils.sortedIndex(values1);
        String index1String = Arrays.toString(index1);
        System.out.println("index1 = "+index1String);
        System.out.println("Should be index1 = [0, 2, 1, 4, 3]");
        assertEquals("index1[1] ","[0, 2, 1, 4, 3]",index1String);
        
        // apply to values
        double sortedValues[] = SortUtils.applyIndexToDoubles(values1, index1, 999.0);
        String sortedValuesString = Arrays.toString(sortedValues);
        System.out.println("sortedValues = "+sortedValuesString);
        System.out.println("Should be sortedValues = [1.0, 2.0, 3.0, 4.0, 5.0]");
        assertEquals("sortedValues ","[1.0, 2.0, 3.0, 4.0, 5.0]",sortedValuesString);
        
    }

    public static void testMerging_1() {
        System.out.println("==============================================================================");
        double values1[] = {1.0, 2.0,      4.0, 5.0,        7.0};
        System.out.println("values1="+Arrays.toString(values1));
        double values2[] = {1.0,      3.0, 4.005,      6.0, 7.02};
        System.out.println("values2="+Arrays.toString(values2));
        double tolerance = 0.01;
        //
        // left tries to match one right value for each left value
        //
        //  {1.0, 2.0,      4.0,   5.0,      7.0      };
        //  {1.0,      3.0, 4.005,      6.0,      7.02};
        // left --->
        //  {1.0,2.0,4.0  ,5.0,7.0};
        //  {1,0,xxx,4.005,xxx,xxx}
        System.out.println("== left merge ==");
        int[][] doubleIndexLeft = SortUtils.mergeDoubleIndex(values1,values2,SortUtils.MergeType.left,tolerance);
        String rightIndexStringLeft = Arrays.toString(doubleIndexLeft[1]);
        System.out.println("rightIndexLeft = "+rightIndexStringLeft);
        System.out.println("Should be rightIndexLeft = [0, -1, 2, -1, -1]");
        assertEquals("rightIndexLeft ","[0, -1, 2, -1, -1]",rightIndexStringLeft);
        // apply this to right values
        double leftMergedValuesLeft[] = SortUtils.applyIndexToDoubles(values1, doubleIndexLeft[0], 999.0);
        double rightMergedValuesLeft[] = SortUtils.applyIndexToDoubles(values2, doubleIndexLeft[1], 999.0);
        String leftMergedValuesStringLeft = Arrays.toString(leftMergedValuesLeft);
        System.out.println("leftMergedValuesLeft = "+leftMergedValuesStringLeft);
        System.out.println("Should be leftMergedValuesLeft = [1.0, 2.0, 4.0, 5.0, 7.0]");
        assertEquals("leftMergedValuesLeft ","[1.0, 2.0, 4.0, 5.0, 7.0]",leftMergedValuesStringLeft);
        String rightMergedValuesStringLeft = Arrays.toString(rightMergedValuesLeft);
        System.out.println("rightMergedValuesLeft = "+rightMergedValuesStringLeft);
        System.out.println("Should be rightMergedValuesLeft = [1.0, 999.0, 4.005, 999.0, 999.0]");
        assertEquals("rightMergedValuesLeft ","[1.0, 999.0, 4.005, 999.0, 999.0]",rightMergedValuesStringLeft);

        //
        // intersection will return only matching pairs of left and right values 
        //
        //  {1.0, 2.0,      4.0,   5.0,      7.0      };
        //  {1.0,      3.0, 4.005,      6.0,      7.02};
        // intersection --->
        //  {1.0,4.0  };
        //  {1,0,4.005}
        System.out.println("== intersection merge ==");
        int[][] doubleIndexIntersection = SortUtils.mergeDoubleIndex(values1,values2,SortUtils.MergeType.intersection,tolerance);
        String rightIndexStringIntersection = Arrays.toString(doubleIndexIntersection[1]);
        System.out.println("rightIndexIntersection = "+rightIndexStringIntersection);
        System.out.println("Should be rightIndexIntersection = [0, 2]");
        assertEquals("rightIndexIntersection ","[0, 2]",rightIndexStringIntersection);
        // apply this to right values
        double leftMergedValuesIntersection[] = SortUtils.applyIndexToDoubles(values1, doubleIndexIntersection[0], 999.0);
        double rightMergedValuesIntersection[] = SortUtils.applyIndexToDoubles(values2, doubleIndexIntersection[1], 999.0);
        String leftMergedValuesStringIntersection = Arrays.toString(leftMergedValuesIntersection);
        System.out.println("leftMergedValuesIntersection = "+leftMergedValuesStringIntersection);
        System.out.println("Should be leftMergedValuesIntersection = [1.0, 4.0]");
        assertEquals("leftMergedValuesIntersection ","[1.0, 4.0]",leftMergedValuesStringIntersection);
        String rightMergedValuesStringIntersection = Arrays.toString(rightMergedValuesIntersection);
        System.out.println("rightMergedValuesIntersection = "+rightMergedValuesStringIntersection);
        System.out.println("Should be rightMergedValuesIntersection = [1.0, 4.005]");
        assertEquals("rightMergedValuesIntersection ","[1.0, 4.005]",rightMergedValuesStringIntersection);

        
        //
        // union will return both left and right values (matching values will be listed once) 
        //
        //  {1.0, 2.0,      4.0,   5.0,      7.0      };
        //  {1.0,      3.0, 4.005,      6.0,      7.02};
        // intersection --->
        //  {1.0, 2.0, xxx  4.0,   5.0, xxx  7.0  xxxx};
        //  {1.0, xxx  3.0, 4.005, xxx  6.0, xxx  7.02};
        System.out.println("== union merge ==");
        int[][] doubleIndexUnion = SortUtils.mergeDoubleIndex(values1,values2,SortUtils.MergeType.union,tolerance);
        String rightIndexStringUnion = Arrays.toString(doubleIndexUnion[1]);
        System.out.println("rightIndexUnion = "+rightIndexStringUnion);
        System.out.println("Should be rightIndexUnion = [0, -1, 1, 2, -1, 3, -1, 4]");
        assertEquals("rightIndexUnion ","[0, -1, 1, 2, -1, 3, -1, 4]",rightIndexStringUnion);
        // apply this to right values
        double leftMergedValuesUnion[] = SortUtils.applyIndexToDoubles(values1, doubleIndexUnion[0], 999.0);
        double rightMergedValuesUnion[] = SortUtils.applyIndexToDoubles(values2, doubleIndexUnion[1], 999.0);
        String leftMergedValuesStringUnion = Arrays.toString(leftMergedValuesUnion);
        System.out.println("leftMergedValuesUnion = "+leftMergedValuesStringUnion);
        System.out.println("Should be leftMergedValuesUnion = [1.0, 2.0, 999.0, 4.0, 5.0, 999.0, 7.0, 999.0]");
        assertEquals("leftMergedValuesUnion ","[1.0, 2.0, 999.0, 4.0, 5.0, 999.0, 7.0, 999.0]",leftMergedValuesStringUnion);
        String rightMergedValuesStringUnion = Arrays.toString(rightMergedValuesUnion);
        System.out.println("rightMergedValuesUnion = "+rightMergedValuesStringUnion);
        System.out.println("Should be rightMergedValuesUnion = [1.0, 999.0, 3.0, 4.005, 999.0, 6.0, 999.0, 7.02]");
        assertEquals("rightMergedValuesUnion ","[1.0, 999.0, 3.0, 4.005, 999.0, 6.0, 999.0, 7.02]",rightMergedValuesStringUnion);
    }
    
    public static void testSearch_1() {
        System.out.println("==============================================================================");
        System.out.println(" Test searching in ordered doubles[] array");
        System.out.println("==============================================================================");
        double values1[]={0.,1.,2.,3.,4.,5.};
        double tol=1e-4;
        double toFind=-0.4;
        int index;
        index=SortUtils.binarySearchIndex(values1,toFind);
        assertEquals(-1,index);
        toFind=1.4;
        index=SortUtils.binarySearchIndex(values1,toFind);
        assertEquals(1,index);
        toFind=6.4;
        index=SortUtils.binarySearchIndex(values1,toFind);
        assertEquals(5,index);
        toFind=-0.4;
        index=SortUtils.findMatchingIndex(values1, toFind, tol);
        assertEquals(-1,index);
        toFind=2.4;
        index=SortUtils.findMatchingIndex(values1, toFind, tol);
        assertEquals(-1,index);
        toFind=1000.;
        index=SortUtils.findMatchingIndex(values1, toFind, tol);
        assertEquals(-1,index);
        toFind=2.00001;
        index=SortUtils.findMatchingIndex(values1, toFind, tol);
        assertEquals(2,index);
        toFind=5.00001;
        index=SortUtils.findMatchingIndex(values1, toFind, tol);
        assertEquals(5,index);
        toFind=-0.00001;
        index=SortUtils.findMatchingIndex(values1, toFind, tol);
        assertEquals(0,index);
        
        double values2[]={0.,1.,2.,3.,4.,5.,6.,7.,8.};  //even number of elements
        toFind=2.0;
        index=SortUtils.findMatchingIndex(values2, toFind, tol);
        assertEquals(2,index);
        double values3[]={0.};  //only one element
        toFind=0.0;
        index=SortUtils.findMatchingIndex(values2, toFind, tol);
        assertEquals(0,index);
    }
}//end class



