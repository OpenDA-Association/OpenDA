package org.openda.model_dflowfm;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by prevel on 27-Nov-15.
 */
public class BcQuantity
{
	private BcProperty quantity;
	private BcProperty unit;
	private List<Double> values;

	public BcQuantity(BcProperty quantity)
	{
		this.quantity = quantity;
		values = new ArrayList<>();
	}

	public BcQuantity(BcProperty quantity, BcProperty unit)
	{
		new BcQuantity(quantity);
		this.unit = unit;
	}

	public void setUnit(BcProperty unit) { this.unit = unit; }
	public void setColumnData(List<Double> values) { this.values = values; }
	public void addColumnData(Double value) { values.add(value); }

	public BcProperty getQuantity() { return quantity; }
	public BcProperty getUnit() { return unit; }
	public List<Double> getValues() { return values; }
}
