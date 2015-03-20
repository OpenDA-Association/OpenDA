package org.openda.model_delft3d;

import org.openda.interfaces.IPrevExchangeItem;

import java.util.Iterator;
import java.util.List;

/**
 * Exchange Item representing a 2D-field in a D3D file
 */
public class D3dWindExchangeItem implements IPrevExchangeItem {

    private String id;
    private List<D3dField2D> field2Ds;
	private List<String> textContent;
	private int endOfHeader;
    private boolean dataChanged;
	private int[] dims= {0,0,0};

    public D3dWindExchangeItem(String id, List<D3dField2D> field2Ds, List<String> content, int endOfHeader) {
        this.id = id;
        this.field2Ds = field2Ds;
        this.dataChanged = false;
		this.textContent = content;
		this.endOfHeader = endOfHeader;
		this.dims[0] = field2Ds.size();
		this.dims[1] = field2Ds.get(0).getMmax();
		this.dims[2] = field2Ds.get(0).getNmax();
    }

    public String getId() {
        return id;
    }

    public String getDescription() {
        return null;  // no description
    }

    public Class getValueType() {
        return List.class;
    }

    public Role getRole() {
        return Role.InOut;
    }

    public Object getValues() {
        return field2Ds;
    }

	public int[] getDims() {
		return this.dims;
	}

    public double[] getValuesAsDoubles() {
		// first get size:
		int nLevels = field2Ds.size();
		double[] allValues = new double[field2Ds.get(0).getMmax() * field2Ds.get(0).getNmax() * nLevels];
        int index = 0;
		for (D3dField2D onefield2D : field2Ds) {
			double[] timeValues = onefield2D.getValues();
			for (int i = 0; i < timeValues.length; i++) {
				allValues[i+index] = timeValues[i];

			}
			index = index + timeValues.length; // which should be nmax*mmax!
		}
		return allValues;


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
        if (!(values instanceof List)) {
            throw new RuntimeException("SetValues for " + this.getId() + ": unexpected object type: " + values.getClass().getName());
        }
		for (int i = 0, field2DsSize = field2Ds.size(); i < field2DsSize; i++) {
			D3dField2D onefield2D = field2Ds.get(i);
			onefield2D = (D3dField2D) ((List) values).get(i);
		}

        dataChanged = true;
    }

    public void setValuesAsDoubles(double[] values) {
		int index = 0;
	    int nmsize = this.dims[1]*this.dims[2];
        for (D3dField2D onefield2D : field2Ds) {
			double[] timeValues = new double[nmsize];
			for (int i = 0; i < nmsize; i++) {
				timeValues[i] = values[i + index];
			}
			onefield2D.setValues(timeValues);
			index = index + nmsize;
		}
        dataChanged = true;
    }

    public double[] getTimes() {
        return null;
    }

	public List<String> getTextContent() {
		return this.textContent;
	}

	public int getEndOfHeader(){
		return this.endOfHeader;
	}

    public void setTimes(double[] times) {
        throw new RuntimeException(this.getClass().getName() + "setTimes(): time stamps can not be set");
    }

    public boolean getDataChanged() {
        return dataChanged;
    }
}
