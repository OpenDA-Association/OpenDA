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
	public static Boolean Compare(File expected, File actual)
	{
		List<String> linesExpected = new ArrayList<>();
		List<String> linesActual = new ArrayList<>();
		String nextLine;

		try
		{
			BufferedReader reader = new BufferedReader(new FileReader(expected));
			while ((nextLine = reader.readLine()) != null) if(nextLine.length() > 0) linesExpected.add(nextLine.replace(" ", ""));
			reader.close();

			reader = new BufferedReader(new FileReader(actual));
			while ((nextLine = reader.readLine()) != null) if(nextLine.length() > 0) linesActual.add(nextLine.replace(" ", ""));
			reader.close();
		}
		catch(Exception ex)
		{
			// swallow exception for tests
			return false;
		}

		if(linesExpected.size() != linesActual.size()) return false;

		List<Integer> unmatchingLines = new ArrayList<>();
		for(int i = 1; i < linesExpected.size(); i++) // ignore 'generated on' comment
		{
			// useful for debugging
			if(!linesExpected.get(i).replaceFirst("\\s+$", "").equals(
					linesActual.get(i).replaceFirst("\\s+$", "")))
				unmatchingLines.add(i);
		}

		return (unmatchingLines.size() == 0);
	}
}
