/* MOD_V2.0 
* Copyright (c) 2012 OpenDA Association 
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
import java.io.*;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by prevel on 27-Nov-15.
 */
public class BcFileReaderWriter
{
	private static final String KEY_VALUE_COMMENT_PATTERN = "^\\s*(?<key>[^=\\s]+)\\s*=\\s*(?<value>[^#=]*)(#(?<comment>.*))?$";

	public static List<BcCategory> readBcFile(File bcFile)
	{
		List<BcCategory> categories = new ArrayList<>();
		BcCategory lastCategory = null;
		int lineNumber = 0;
		String nextLine;

		try
		{
			BufferedReader reader = new BufferedReader(new FileReader(bcFile));
			boolean extraColumn = false;
			while ((nextLine = reader.readLine()) != null) {
				nextLine = nextLine.trim();
				lineNumber++;
				if (nextLine.isEmpty() || nextLine.startsWith("#")) continue;

				if (nextLine.startsWith("[")) {
					String header = nextLine.substring(1, nextLine.lastIndexOf("]"));
					lastCategory = new BcCategory(lineNumber, header);

					categories.add(lastCategory);
					extraColumn = false;
				} else if (lastCategory != null) {
					if (nextLine.contains("=")) {
						Pattern pattern = Pattern.compile(KEY_VALUE_COMMENT_PATTERN);
						Matcher matcher = pattern.matcher(nextLine);
						if (matcher.find()) {
							String name = matcher.group("key");
							String value = matcher.group("value");
							String comment = matcher.group("comment") == null ? "" : matcher.group("comment");

							BcProperty property = new BcProperty(lineNumber, name, value, comment);

							if (name.equalsIgnoreCase("quantity")) {
								lastCategory.getTable().add(new BcQuantity(property));
							} else if (name.equalsIgnoreCase("unit")) {
								List<BcQuantity> table = lastCategory.getTable();
								table.get(table.size() - 1).setUnit(property);
							} else if (name.equalsIgnoreCase("function") && ("astronomic".equalsIgnoreCase(value) || "harmonic".equalsIgnoreCase(value))) {
								extraColumn = true;
								lastCategory.addProperty(property);
							} else {
								lastCategory.addProperty(property);
							}
						}
					} else {
						List<BcQuantity> table = lastCategory.getTable();
						String[] values = nextLine.split("[ ]{1,}");
						if (table.size() != values.length)
							throw new IllegalArgumentException("Number of values does not match number of quantities");

						for (int i = 0; i < table.size(); i++) {
							BcQuantity bcQuantity = table.get(i);
							if (i == 0 && extraColumn) {
								bcQuantity.addColumnDataString(values[i]);
								continue;
							}
							// How to handle string column
							Double value = Double.valueOf(values[i]);
							bcQuantity.addColumnDataDouble(value);
						}
					}
				}
			}
			reader.close();
		} catch (Exception ex) {
			String errorMessage = ex.getMessage();
			if (lastCategory != null) errorMessage = String.format("%s, Category: %s", errorMessage, lastCategory.getName());
			errorMessage = String.format("%s, BcFile: %s, LineNumber: %s", errorMessage, bcFile.getPath(), lineNumber);

			throw new RuntimeException("Error parsing BcFile: " + errorMessage);
		}
		return categories;
	}

	public static void writeBcFile(File bcFile, List<BcCategory> categories)
	{
		try
		{
			BufferedWriter writer = new BufferedWriter(new FileWriter(bcFile));
			String currentDateTime = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss").format(Calendar.getInstance().getTime());
			writer.write(String.format("# Generated on %s%s%s", currentDateTime, System.lineSeparator(), System.lineSeparator()));

			for (BcCategory category : categories)
			{
				writer.write(String.format("[%s]%s", category.getName(), System.lineSeparator()));

				for (BcProperty property : category.getProperties())
					writer.write(String.format("%s%s", generatePropertyString(property), System.lineSeparator()));


				List<BcQuantity> table = category.getTable();
				if (table.size() > 0) // General category will have zero entries in table
				{
					BcQuantity bcQuantity = table.get(0);
					String[] tableRows = new String[bcQuantity.getStrings().size() + bcQuantity.getValues().size()];
					Arrays.fill(tableRows, "");

					for (BcQuantity column : table) {
						writer.write(String.format("%s%s", generatePropertyString(column.getQuantity()), System.lineSeparator()));
						writer.write(String.format("%s%s", generatePropertyString(column.getUnit()), System.lineSeparator()));

						List<String> strings = column.getStrings();
						for (int j = 0; j < strings.size(); j++) {
							tableRows[j] += strings.get(j) + ' ';
						}
						List<Double> values = column.getValues();
						for (int j = 0; j < values.size(); j++) {
							tableRows[j] += values.get(j).toString() + ' ';
						}
					}

					for (String row : tableRows)
						writer.write(String.format("    %s%s", row, System.lineSeparator()));
				}
				writer.newLine();
			}
			writer.close();
		} catch (Exception ex) {
			throw new RuntimeException("Error writing BcFile: " + ex.getMessage());
		}
	}

	private static String generatePropertyString(BcProperty property) {
		String comment = property.getComment();
		comment = comment.equals("") ? "" : String.format("# %s", comment);

		// formatted name value comment string with trailing spaces removed
		return String.format("    %-22s= %-34s%s", property.getName(), property.getValue(), comment).replaceFirst("\\s+$", "");
	}
}
