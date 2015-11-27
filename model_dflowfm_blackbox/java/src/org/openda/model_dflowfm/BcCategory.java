package org.openda.model_dflowfm;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by prevel on 27-Nov-15.
 */
public class BcCategory
{
	private int lineNumber;
	private String name;
	private List<BcProperty> properties;
	private List<BcQuantity> table;

	public BcCategory(int lineNumber, String name)
	{
		this.lineNumber = lineNumber;
		this.name = name;
		properties = new ArrayList<>();
		table = new ArrayList<>();
	}

	public void addProperty(BcProperty property) { properties.add(property); }
	public void addTableColumn(BcQuantity column) { table.add(column); }

	public int getLineNumber() { return lineNumber; }
	public String getName() { return name; }
	public List<BcProperty> getProperties() { return properties; }
	public List<BcQuantity> getTable() { return table; }
}
