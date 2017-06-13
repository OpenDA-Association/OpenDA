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

import org.openda.interfaces.IPrevExchangeItem;

/**
 * Exchange Item representing a D3D astro component or correction
 */
public class D3dAstroExchangeItem implements IPrevExchangeItem {

    private String id;
    private D3dAstroComponent astroComponent;
    public final static String phaseString = "Phase";
    public final static String amplitudeString = "Amplitude";
    boolean isAmplitude;
    private D3dAstroStation astroStation;

    public D3dAstroExchangeItem(String stationId, String varString, D3dAstroComponent astroComponent) {
        isAmplitude = varString.equals(amplitudeString);
        this.astroComponent = astroComponent;
        this.id = stationId + "." + astroComponent.getId() + "." + varString;
    }

    public D3dAstroExchangeItem(D3dAstroStation astroStation) {
        this.astroStation = astroStation;
        this.id = astroStation.getId() + "." + "A0";
    }

    public String getId() {
        return id;
    }

    public String getDescription() {
        return null;  // no description
    }

    public Class getValueType() {
        return double.class;
    }

    public Role getRole() {
        return IPrevExchangeItem.Role.InOut;
    }

    public Object getValues() {
        if (astroComponent != null) {
            if (isAmplitude) {
                return astroComponent.getAmplitude();
            } else {
                return astroComponent.getPhase();
            }
        } else {
            return astroStation.getA0();
        }
    }

    public double[] getValuesAsDoubles() {
        if (astroComponent != null) {
            if (isAmplitude) {
                return new double[] {astroComponent.getAmplitude()};
            } else {
                return new double[] {astroComponent.getPhase()};
            }
        } else {
            return new double[] {astroStation.getA0()};
        }
    }

    public void axpyOnValues(double alpha, double[] axpyValues) {
        double[] values = getValuesAsDoubles();
        for (int i = 0; i < values.length; i++) {
            values[i] += alpha * axpyValues[i];
        }
        setValuesAsDoubles(values);
    }

	public void multiplyValues(double[] multiplicationFactors) {
		double[] values = getValuesAsDoubles();
		for (int i = 0; i < values.length; i++) {
			values[i] *= multiplicationFactors[i];
		}
		setValuesAsDoubles(values);
	}

	public void setValues(Object values) {
        if (!(values instanceof Double)) {
            throw new RuntimeException("SimpleExchangeItem.setValues(" + values.toString() +
                    "): values must be of type double, but is " + values.getClass().getName());
        }
        if (astroComponent != null) {
            if (isAmplitude) {
                astroComponent.setAmplitude((Double) values);
            } else {
                astroComponent.setPhase((Double) values);
            }
        } else {
            astroStation.setA0((Double) values);
        }
    }

    public void setValuesAsDoubles(double[] values) {
        if (values.length != 1) {
            throw new RuntimeException(this.getClass().getName() + "setValues(" + values.toString() +
                    "): values[] must have length 1, but is " + values.length);
        }
        if (astroComponent != null) {
            if (isAmplitude) {
                astroComponent.setAmplitude(values[0]);
            } else {
                astroComponent.setPhase(values[0]);
            }
        } else {
            astroStation.setA0(values[0]);
        }
    }

    public double[] getTimes() {
        return null;
    }

    public void setTimes(double[] times) {
        throw new RuntimeException(this.getClass().getName() + "setTimes(): time stamps can not be set");
    }

}

