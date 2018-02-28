/* OpenDA v2.4.3 
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
import java.util.*;
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
	public static final String[] EMPTY_ARRAY = new String[0];

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

	public static String[] split(String text, char separatorChar) {
		return split(text, separatorChar, separatorChar);
	}

	public static String[] split(String text, char separatorChar, char quoteChar) {
		return split(text, separatorChar, separatorChar, quoteChar, separatorChar == ' ');
	}

	public static String[] split(String text, char delimiter, char subDelimiter, char quoteChar, boolean ignoreEmptySections) {
		int size = countSections(text, delimiter, subDelimiter, quoteChar, ignoreEmptySections);
		if (size == 0) return EMPTY_ARRAY;
		String[] res = new String[size];
		int sectionCount = split(text, delimiter, subDelimiter, quoteChar, ignoreEmptySections, res);
		assert sectionCount == res.length;
		return res;
	}

	public static int split(String text, char separatorChar, char quoteChar, String[] buffer) {
		return split(text, separatorChar, separatorChar, quoteChar, separatorChar == ' ', buffer);
	}

	public static int split(String text, char separatorChar, String[] buffer) {
		return split(text, separatorChar, separatorChar == ' ', buffer);
	}

	public static int countSections(String text, char delimiter, char subDelimiter, char quoteChar,
									boolean ignoreEmptySections) {
		if (text.isEmpty()) return 1;

		int res = 0;
		for (int pos = 0, n = text.length(); pos < n; ) {
			if (ignoreEmptySections && text.charAt(pos) == delimiter) {
				pos++;
				continue;
			}

			pos += getMainSectionLength(text, delimiter, subDelimiter, quoteChar, pos);
			res++;
			pos++; // skip next separator
			if (pos == n && !ignoreEmptySections) res++; //last section is empty
		}
		return res;
	}

	/**
	 * @param text                The text to split
	 * @param delimiter           e.g. " ", "\t"
	 * @param subDelimiter        To split an excel clipboard into rows you have to know the cell separator (\t)
	 * @param quoteChar           When the content of a cell is surrounded with quotes, separators within the content of the cell are taken literal.
	 *                            A literal quote char is written with a quote char directly followed by an other quote char.
	 *                            Excel does not allow tabs in a cell. When no line separator exists between two quote chars, the quote chars should taken literal
	 * @param ignoreEmptySections Excel clipboard formatting.
	 *                            When a cell contains new lines the cell is quoted with "".
	 *                            Only when the first character is a quote character and a new line is find before the new tab the cell is quoted
	 *                            Quote characters in a quoted cell are given "".
	 */
	// todo add argument ignoreLastEmptySection
	// todo add argument allowRowSeparatorCharInsideCellContent
	@SuppressWarnings("StringConcatenationMissingWhitespace")
	public static int split(String text, char delimiter, char subDelimiter,
							char quoteChar, boolean ignoreEmptySections, String[] buffer) {
		if (text.isEmpty()) {
			Arrays.fill(buffer, 0, buffer.length, "");
			return 1;
		}

		int i = 0;
		for (int pos = 0, n = text.length(); pos < n; ) {
			if (ignoreEmptySections && text.charAt(pos) == delimiter) {
				pos++;
				continue;
			}

			int sectionLength = getMainSectionLength(text, delimiter, subDelimiter, quoteChar, pos);
			if (sectionLength > 1 && text.charAt(pos) == quoteChar
				&& text.charAt(pos + sectionLength - 1) == quoteChar) {
				String s = text.substring(pos + 1, pos + sectionLength - 1);
				if (s.indexOf(quoteChar) != -1) s = replaceAll(s, Character.toString(quoteChar) + Character.toString(quoteChar), Character.toString(quoteChar));
				buffer[i++] = s;
			} else {
				buffer[i++] = text.substring(pos, pos + sectionLength);
			}
			if (i >= buffer.length) return i;

			pos += sectionLength;
			pos++; // skip next separator
			if (pos == n && !ignoreEmptySections) {
				// last section is empty
				buffer[i++] = "";
			}

		}

		Arrays.fill(buffer, i, buffer.length, "");
		return i;
	}

	public static int split(String text, char delimiter, boolean ignoreEmptySections, String[] buffer) {
		if (text.isEmpty()) {
			buffer[0] = "";
			Arrays.fill(buffer, 1, buffer.length, "");
			return 0;
		}

		int i = 0;
		for (int pos = 0, n = text.length(); pos < n; ) {
			if (ignoreEmptySections && text.charAt(pos) == delimiter) {
				pos++;
				continue;
			}

			int sectionLength = getSectionLength(text, delimiter, pos);
			buffer[i++] = text.substring(pos, pos + sectionLength);
			if (i >= buffer.length) return i;

			pos += sectionLength;
			pos++; // skip next separator
			if (pos == n && !ignoreEmptySections) {
				// last section is empty
				buffer[i++] = "";
			}

		}

		Arrays.fill(buffer, i, buffer.length, "");
		return i;
	}

	public static int getSectionLength(String text, char delimiter, int startPos) {
		int pos = text.indexOf(delimiter, startPos);
		if (pos == -1) return text.length() - startPos;
		return pos - startPos;
	}

	public static int getMainSectionLength(String text, char delimiter, char subDelimiter, char quoteChar, int startPos) {
		if (delimiter == subDelimiter) {
			return getSubSectionLength(text, delimiter, subDelimiter, quoteChar, startPos);
		}


		for (int pos = startPos; ; ) {
			pos += getSubSectionLength(text, delimiter, subDelimiter, quoteChar, pos);
			if (pos == text.length() || text.charAt(pos) == delimiter) return pos - startPos;
			pos++; // skip sub delimiter
			if (pos == text.length()) return pos - startPos;
		}
	}


	public static int getSubSectionLength(String text, char delimiter, char subDelimiter, char quoteChar, int startPos) {
		char c = text.charAt(startPos);
		if (c == delimiter || c == subDelimiter) return 0;
		if (c == quoteChar) {
			if (startPos + 1 == text.length()) return 1;
			c = text.charAt(startPos + 1);
			if (c != quoteChar) {
				if (c == delimiter || c == subDelimiter) return 1;
				//            boolean previousQuoteChar = false;
				for (int pos = startPos + 1, n = text.length(); pos < n; pos++) {
					c = text.charAt(pos);
					if (c != quoteChar) continue;
					if (pos + 1 < text.length() && text.charAt(pos + 1) == quoteChar) {
						pos++;
						continue;
					}
					return pos - startPos + 1;
				}
				return text.length() - startPos;
			}
		}

		if (delimiter != subDelimiter) {
			for (int pos = startPos + 1, n = text.length(); pos < n; pos++) {
				c = text.charAt(pos);
				if (startPos + 1 == text.length()) return pos - startPos;
				if (c == delimiter || c == subDelimiter) return pos - startPos;
			}
		}

		int pos = text.indexOf(delimiter, startPos);
		if (pos == -1) return text.length() - startPos;
		return pos - startPos;
	}

	/**
	 * This method replaces all variables in <code>string</code> with their values as stored in <code>variables</code>.
	 *
	 * @param string    Original string
	 * @param variables A collection of variables and their actual values
	 * @return Resulting string
	 */
	public static String replaceAll(String string, Map<String, String> variables) {
		if (variables.isEmpty())
			return string;
		Iterator<Map.Entry<String, String>> iterator = variables.entrySet().iterator();
		String newString = string;
		while (iterator.hasNext()) {
			Map.Entry<String, String> entry = iterator.next();
			newString = replaceAll(newString, entry.getKey(), entry.getValue());
		}
		return newString;
	}

	public static String replaceFirst(String s, String oldString, String newString) {
		return replace(s, oldString, newString, 1);
	}

	public static String replaceAll(String s, String oldString, String newString) {
		return replace(s, oldString, newString, Integer.MAX_VALUE);
	}


	public static String replace(String s, String oldString, String inputNewString, int maxCount) {
		// todo remove implicit behaviour of illegal arguments, add null argument checks
		if (s == null) return null;
		if (oldString == null) return s;
		String newString;
		if (inputNewString == null) {
			newString = "";
		} else {
			newString = inputNewString;
		}

		if (oldString.equals(newString)) return s;
		if (oldString.length() == 1 && newString.length() == 1) return s.replace(oldString.charAt(0), newString.charAt(0));

		int count;
		int newLength;
		if (oldString.length() == newString.length()) {
			if (!s.contains(oldString)) return s;
			count = maxCount;
			newLength = s.length();
		} else {
			count = Math.min(maxCount, count(s, oldString));
			if (count == 0) return s;
			newLength = s.length() + count * (newString.length() - oldString.length());
		}

		char[] res = new char[newLength];

		int pos = 0;
		int k = 0;
		for (int i = 0; i < count; i++) {
			int newPos = s.indexOf(oldString, pos);
			if (newPos == -1) {
				//noinspection BreakStatement
				break;
			}
			s.getChars(pos, newPos, res, k);
			k += newPos - pos;
			newString.getChars(0, newString.length(), res, k);
			k += newString.length();
			pos = newPos + oldString.length();
		}
		s.getChars(pos, s.length(), res, k);
		return new String(res);
	}

	public static int count(String s, String subString) {
		int res = 0;
		for (int pos = 0; (pos = s.indexOf(subString, pos)) != -1; pos += subString.length()) {
			res++;
		}

		return res;
	}

	/**
	 * {@link Character#toLowerCase} is very slow because it call virtual methods, take a shortcut for ascii
	 */
	public static char toLowerCase(char ch) {
		if (ch > 127) return Character.toLowerCase(ch);
		if ('A' <= ch && ch <= 'Z') return (char) (ch + 32);
		return ch;
	}

	/**
	 * {@link Character#toUpperCase(char)} is very slow because it call virtual methods, take a shortcut for ascii
	 */
	public static char toUpperCase(char ch) {
		if (ch > 127) return Character.toUpperCase(ch);
		if ('a' <= ch && ch <= 'z') return (char) (ch - 32);
		return ch;
	}

	public static boolean equalsIgnoreCase(String s1, String s2) {
		if (s1 == s2) return true;
		if (s1 == null || s2 == null) return false;
		int length = s1.length();
		if (length != s2.length()) return false;
		return s1.equalsIgnoreCase(s2);
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
