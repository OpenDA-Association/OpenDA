package org.openda.model_dflowfm;

/**
 * Created by prevel on 27-Nov-15.
 */
public class BcProperty
{
	private int lineNumber;
	private String name;
	private String value;
	private String comment;

	public BcProperty(int lineNumber, String name, String value)
	{
		this(lineNumber, name, value, "");
	}

	public BcProperty(int lineNumber, String name, String value, String comment)
	{
		this.lineNumber = lineNumber;
		this.name = name;
		this.value = value;
		this.comment = comment;
	}

	public int getLineNumber() { return lineNumber; }
	public String getName() { return name; }
	public String getValue() { return value; }
	public String getComment() { return comment; }
}
