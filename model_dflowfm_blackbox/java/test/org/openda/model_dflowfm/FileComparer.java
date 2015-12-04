package org.openda.model_dflowfm;

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

	private static List<String> ReadLines(File target)
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
