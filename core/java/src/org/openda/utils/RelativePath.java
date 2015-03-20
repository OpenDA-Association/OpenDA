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

import java.io.*;
import java.util.*;

/**
 * Original URL: http://www.devx.com/tips/Tip/13737
 * <p/>
 * this class provides functions used to generate a relative path
 * from two absolute paths
 *
 * @author David M. Howard
 */

public class RelativePath {

    /**
     * break a path down into individual elements and add to a list.
     * example : if a path is /a/b/c/d.txt, the breakdown will be [d.txt,c,b,a]
     *
     * @param f input file
     * @return a List collection with the individual elements of the path in
     *         reverse order
     */
    private static List<String> getPathList(File f) {
        List<String> l = new ArrayList<String>();
        File r;
        try {
            r = f.getCanonicalFile();
            while (r != null) {
                l.add(r.getName());
                r = r.getParentFile();
            }
        }
        catch (IOException e) {
            e.printStackTrace();
            l = null;
        }
        return l;
    }

    /**
     * figure out a string representing the relative path of
     * 'f' with respect to 'r'
     *
     * @param r home path
     * @param f path of file
	 * @return The relative path as string
     */
    private static String matchPathLists(List<String> r, List<String> f) {
        int i;
        int j;
        String s;
        // start at the beginning of the lists
        // iterate while both lists are equal
        s = "";
        i = r.size() - 1;
        j = f.size() - 1;

        // first eliminate common root
        while ((i >= 0) && (j >= 0) && (r.get(i).equals(f.get(j)))) {
            i--;
            j--;
        }

        // for each remaining level in the home path, add a ..
        for (; i >= 0; i--) {
            s += ".." + File.separator;
        }

        // for each level in the file path, add the path
        for (; j >= 1; j--) {
            s += f.get(j) + File.separator;
        }

        // file name
        if(j>=0){
        	s += f.get(j);
        }
        return s;
    }

    /**
     * get relative path of File 'f' with respect to 'home' directory
     * example : home = /a/b/c
     * f    = /a/d/e/x.txt
     * s = getRelativePath(home,f) = ../../d/e/x.txt
     *
     * @param home base path, should be a directory, not a file, or it doesn't
     *             make sense
     * @param f    file to generate path for
     * @return path from home to f as a string
     */
    public static String getRelativePath(File home, File f) {
		List<String> homelist = getPathList(home);
		List<String> filelist = getPathList(f);
		return matchPathLists(homelist, filelist);
    }

    /**
    * Method to test the function
	 * @param args Arguments for testing (2 args: home file)
	 */
    public static void main(String args[]) {
        if (args.length != 2) {
            System.out.println("RelativePath <home> <file>");
            return;
        }
        System.out.println("home = " + args[0]);
        System.out.println("file = " + args[1]);
        System.out.println("path = " + getRelativePath(new File(args[0]), new
               File(args[1])));
    }
}
