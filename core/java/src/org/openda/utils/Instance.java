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

import org.openda.interfaces.IInstance;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IStochObserver;
import org.openda.interfaces.IAlgorithm;

/**
 * Class to implement general methods for dealing with instances
 */
public class Instance implements IInstance {
    private IInstance parent = null;

    public Instance() {
        this.parent = null;
    }
    public Instance(IInstance parent) {
        this.parent = parent;
    }
    static public String identifySource(IInstance source) {
        String identity = "(other) ";
        String parentIdentity = "";

        if (source.getParent() != null) {
            parentIdentity = identifySource(source.getParent());
        }
        if (source instanceof IModelInstance) {
            identity = "Model ";
        }
        if (source instanceof IStochObserver) {
            identity = "Observer ";
        }
        if (source instanceof IAlgorithm) {
            identity = "Algorithm ";
        }

        identity = parentIdentity.concat(identity);

        return identity;
    }

    public IInstance getParent() {
        return parent;
    }

    public void setParent(IInstance parent) {
        this.parent = parent;
    }
}
