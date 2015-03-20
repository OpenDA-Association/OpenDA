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
}
