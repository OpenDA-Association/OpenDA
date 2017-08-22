/* OpenDA v2.4.1 
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

import java.util.List;
import java.util.ArrayList;
import java.util.Locale;

/**
 * Astro component, or a correction on an astro component
 */
public class D3dAstroStation {

    private String id;
    private List<D3dAstroComponent> astroComponents = new ArrayList<D3dAstroComponent>();
    private double a0;
    private boolean dataChanged;

    public D3dAstroStation(String id) {
        this.id = id;
        this.dataChanged = false;
    }

    public boolean getDataChanged() {
        if (!dataChanged) {
            for (D3dAstroComponent astroComponent : astroComponents) {
                if (astroComponent.getDataChanged()) {
                    return true;
                }
            }
            return false;
        }
        return true;
    }

    public String getId() {
        return id;
    }

    public List<D3dAstroComponent> getComponents() {
        return astroComponents;
    }

    public void addComponent(D3dAstroComponent astroComponent) {
        astroComponents.add(astroComponent);
    }

    public void setA0(String a0String) {
        this.a0 = Double.parseDouble(a0String);
    }

    public double getA0() {
        return a0;
    }

    public void setA0(double a0) {
        this.a0 = a0;
        this.dataChanged = true;
    }

    public String toString() {
        Locale locale = new Locale("EN");
        String stringFormat = "%-6s";
        String floatValueFormat = "%12.3f";
        return String.format(stringFormat, "A0")+ String.format(locale, floatValueFormat, a0);
    }
}
