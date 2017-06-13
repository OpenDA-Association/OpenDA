/* OpenDA v2.4 
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

import java.util.Locale;

/**
 * Astro component, or its correction
 */
public class D3dAstroComponent {

    private String id;
    private double phase;
    private double amplitude;
    private boolean dataChanged;
    private D3dAstroComponent correction;

    public D3dAstroComponent(String id, double amplitude, double phase) {
        this.id = id;
        this.amplitude = amplitude;
        this.phase = phase;
        this.correction = null;
        this.dataChanged = false;
     }

    public D3dAstroComponent(String id, String amplitudeString, String phaseString) {
        this(id, Double.parseDouble(amplitudeString), Double.parseDouble(phaseString));
    }

    public double getPhase() {
        if (correction != null) {
            return phase + correction.getPhase();
        } else {
            return phase;
        }
    }

    public double getAmplitude() {

        if (correction != null) {
            return amplitude * correction.getAmplitude();
        } else {
            return amplitude;
        }
    }

    public void setPhase(double phase) {
        if (correction != null) {
            correction.setPhase(phase - this.phase);
        } else {
            this.phase = phase;
        }
        this.dataChanged = true;
    }

    public void setAmplitude(double amplitude) {
        if (correction != null) {
            correction.setAmplitude(amplitude / this.amplitude);
        } else {
            this.amplitude = amplitude;
        }
        this.dataChanged = true;
    }

    public String getId() {
        return id;
    }

    public boolean getDataChanged() {
        return dataChanged;
    }

    public void setCorrection(D3dAstroComponent correction) {
        this.correction = correction;
    }

    public String toString() {
        Locale locale = new Locale("EN");
        String stringFormat = "%-6s";
        String floatValueFormat = "%12.3f";
        return String.format(stringFormat, id) + String.format(locale, floatValueFormat, amplitude)
                + String.format(locale, floatValueFormat, phase);
    }
}
