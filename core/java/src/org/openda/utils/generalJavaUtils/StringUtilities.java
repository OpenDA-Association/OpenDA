/* OpenDA v2.4 
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
package org.openda.utils.generalJavaUtils;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created with IntelliJ IDEA.
 * User: bos_en
 * Date: 15-3-13
 * Time: 12:15
 * To change this template use File | Settings | File Templates.
 */
public class StringUtilities {

	// Split a string on whitespace, ignoring whitespace between quotes.
	// Whitespace includes spaces, tabs, and ...
	public static String[] splitStringOnWhitespaceWhilePreservingQuotes(String line) {

		List<String> matchList = new ArrayList<String>();
		Pattern regex = Pattern.compile("[^\\s\"']+|\"[^\"]*\"|'[^']*'");
		Matcher regexMatcher = regex.matcher(line);
		while (regexMatcher.find()) {
			matchList.add(regexMatcher.group());
		}
		String[] lineParts =  new String[matchList.size()];
		lineParts = matchList.toArray(lineParts);
		return lineParts;
	}

	// Join a StringArray into a String, using a separator.
	public static String joinStringArrayUsingSeparator(String[] lineParts, String separator) {

		StringBuilder builder = new StringBuilder();
		for(int i=0; i<lineParts.length-1; i++) {
			builder.append(lineParts[i]);
			builder.append(separator);
		}
		builder.append(lineParts[lineParts.length-1]);
		return builder.toString();
	}

	public static String[] getKeyValuePair(String argument) {
		int index = argument.indexOf('=');
		if (index == -1 || index == argument.length() - 1) throw new IllegalArgumentException("Not a key value pair: " + argument);
		String[] split = argument.split("=");
		if (split.length > 2) throw new IllegalArgumentException("Not a key value pair: " + argument);
		return split;
	}
}
