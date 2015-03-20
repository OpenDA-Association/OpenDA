package org.openda.model_delft3d;

import org.openda.interfaces.IPrevExchangeItem;

/**
 * Exchange Item representing a 2D-field in a D3D file
 */
public class D3dField2DExchangeItem implements IPrevExchangeItem {

    private String id;
    private D3dField2D field2D;
    private boolean dataChanged;

    public D3dField2DExchangeItem(String id, D3dField2D field2D) {
        this.id = id;
        this.field2D = field2D;
        this.dataChanged = false;
    }

    public String getId() {
        return id;
    }

    public String getDescription() {
        return null;  // no description
    }

    public Class getValueType() {
        return D3dField2D.class;
    }

    public Role getRole() {
        return IPrevExchangeItem.Role.InOut;
    }

    public Object getValues() {
        return field2D;
    }

    public double[] getValuesAsDoubles() {
        return field2D.getValues();
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
        if (!(values instanceof D3dField2D)) {
            throw new RuntimeException("SetValues for" + this.getId() + ": unexpected object type: " + values.getClass().getName());
        }
        field2D = (D3dField2D) values;
        dataChanged = true;
    }

    public void setValuesAsDoubles(double[] values) {
        field2D.setValues(values);
        dataChanged = true;
    }

    public double[] getTimes() {
        return null;
    }

    public void setTimes(double[] times) {
        throw new RuntimeException(this.getClass().getName() + "setTimes(): time stamps can not be set");
    }

    public boolean getDataChanged() {
        return dataChanged;
    }
}
