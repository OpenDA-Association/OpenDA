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
package org.openda.utils;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by prevel on 02-Dec-15.
 */
public class FileComparer
{
	public static Boolean CompareXmlFiles(File expected, File actual)
	{
		List<String> linesExpected = RemoveXmlComments(ReadLines(expected));
		List<String> linesActual = RemoveXmlComments(ReadLines(actual));

		if(linesExpected.size() != linesActual.size()) return false;
		return CompareLines(2, linesExpected, linesActual); // ignores first 2 lines of XML file
	}

	public static Boolean CompareIniFiles(File expected, File actual)
	{
		List<String> linesExpected = ReadLines(expected);
		List<String> linesActual = ReadLines(actual);

		if(linesExpected.size() != linesActual.size()) return false;
		return CompareLines(1, linesExpected, linesActual); // ignores first line of Ini file
	}

	private static Boolean CompareLines(int ignoreFirstLines, List<String> linesExpected, List<String> linesActual)
	{
		List<Integer> unmatchingLines = new ArrayList<>();
		for(int i = ignoreFirstLines; i < linesExpected.size(); i++)
		{
			// useful for debugging
			String expectedLine = linesExpected.get(i);
			String actualLine = linesActual.get(i);

			if(!expectedLine.equals(actualLine)) unmatchingLines.add(i);
		}

		return (unmatchingLines.size() == 0);
	}

	public static List<String> ReadLines(File target)
	{
		List<String> lines = new ArrayList<>();
		try
		{
			BufferedReader reader = new BufferedReader(new FileReader(target));
			String nextLine;
			while ((nextLine = reader.readLine()) != null)
			{
				nextLine = nextLine.replaceAll(" ", "");
				if(nextLine.length() > 0) lines.add(nextLine);
			}
			reader.close();
		}
		catch(Exception ex)	{ throw new RuntimeException(String.format("%s, Error reading file: %s", ex.getMessage(), target.getPath())); }
		return lines;
	}

	private static List<String> RemoveXmlComments(List<String> originalLines)
	{
		List<String> prunedLines = new ArrayList<>();
		Boolean isComment = false;
		for(String line : originalLines)
		{
			if(isComment && line.contains("-->"))
			{
				line = line.substring(line.indexOf("-->")+3, line.length());
				isComment = false;
			}
			if(line.contains("<!--"))
			{
				String comment = line.substring(line.indexOf("<!--"), line.length());
				if(comment.contains("-->"))
				{
					comment = comment.substring(0, comment.indexOf("-->")+3);
					isComment = false;
				}
				else
				{
					isComment = true;
				}
				line = line.replace(comment, "");
			}
			if(!isComment && line.length() > 0) prunedLines.add(line);
		}
		return prunedLines;
	}
}
