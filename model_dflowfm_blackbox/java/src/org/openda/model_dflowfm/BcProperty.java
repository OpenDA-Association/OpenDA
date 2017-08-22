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
package org.openda.model_dflowfm;
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
