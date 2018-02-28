/* OpenDA v2.4.3 
* Copyright (c) 2017 OpenDA Association 
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

package org.openda.model_delft3d;

/**
 * TODO: description
*/
class MnPoint {

    int m;
    int n;

    public MnPoint(String mString, String nString) {
        m = Integer.parseInt(mString);
        n = Integer.parseInt(nString);
    }

    public MnPoint(int m, int n) {
        this.m = m;
        this.n = n;
    }

    public int getM() {
        return m;
    }

    public int getN() {
        return n;
    }
}
