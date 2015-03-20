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
package org.openda.utils.io;
import java.io.*;
import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: Juzer Dhondia
 * Date: 08-Mar-2010
 * Time: 13:47:09
 */
public class CsvReader extends BufferedReader {
    private char quoteChar = '"';
    private char columnSeparatorChar = ',';
    private String commentLinePrefix = "#";
    private boolean skipEmptyLines = true;
    private String currentLine = null;

    /**
     * The current line number
     */
    private int lineNumber = 0;
    private File file = null;


    public CsvReader(String aPath) throws FileNotFoundException {
        this(new File(aPath));
    }

    public CsvReader(File aFile) throws FileNotFoundException {
        this(new BufferedReader(new FileReader(aFile)));
        file = aFile;
    }


    public CsvReader(Reader in) {
        super(in);
    }

    public CsvReader(InputStream in) {
        super(new InputStreamReader(in));
    }


    public char getQuoteChar() {
        return quoteChar;
    }

    public void setQuoteChar(char aQuoteChar) {
        quoteChar = aQuoteChar;
    }

    public boolean isSkipEmptyLines() {
        return skipEmptyLines;
    }

    public void setSkipEmptyLines(boolean skipEmptyLines) {
        this.skipEmptyLines = skipEmptyLines;
    }

    public String getCommentLinePrefix() {
        return commentLinePrefix;
    }

    public void setCommentLinePrefix(String aCommentLinePrefix) {
        commentLinePrefix = aCommentLinePrefix;
    }

    public String[] readCSVLineTrimElements() throws IOException {
        String[] res = readCSVLine();
        trimElements(res);
        return res;
    }

    public String readLine() throws IOException {
        String res;
        for (; ;) {
            res = super.readLine();
            if (res == null) return null;
            lineNumber++;
            if (skipEmptyLines && res.length() == 0) continue;
            if (commentLinePrefix != null && res.trim().startsWith(commentLinePrefix)) continue;
            break;
        }

        currentLine = res;
        return res;
    }

    public String[] readCSVLine() throws IOException {
        String s = readLine();
        if (s == null) return null;
        return parseLine(s, columnSeparatorChar, quoteChar);
    }

    public String getCurrentLine() {
        return currentLine;
    }

    public char getColumnSeperatorChar() {
        return columnSeparatorChar;
    }

    public void setColumnSeparatorChar(char aColumnSeparatorChar) {
        columnSeparatorChar = aColumnSeparatorChar;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public void setLineNumber(int aLineNumber) {
        lineNumber = aLineNumber;
    }

    public File getFile() {
        return file;
    }

    public String getFileAndLineNumber() {
        if (file == null) {
            return "line: " + lineNumber;
        } else {
            return "file:" + file + " at line: " + lineNumber;
        }
    }

    public int skipLines(int count) throws IOException {
        for (int i = 0; i < count; i++) {
            String[] line = readCSVLine();
            if (line == null) return i;
        }

        return count;
    }

    private static void trimElements(String[] array) {
        if (array != null) {
            for (int i = 0; i < array.length; i++) {
                array[i] = (array[i] != null) ? array[i].trim() : null;
            }
        }
    }

    public static String[] parseLine(String line, char separator, char quoteChar) {

        if (line == null)
            throw new IllegalArgumentException("line == null");

        ArrayList<String> buffer = new ArrayList<String>();

        for (int pos = 0, n = line.length() ; pos < n;) {
            int sectionLength = getSectionLength(line, separator, quoteChar, pos);
            if (sectionLength > 1 && line.charAt(pos) == quoteChar
                    && line.charAt(pos + sectionLength - 1) == quoteChar)  {
                buffer.add(line.substring(pos + 1, pos + sectionLength - 1));
            } else {
                buffer.add(line.substring(pos, pos + sectionLength));
            }
            pos += sectionLength;
            while (pos < n && line.charAt(pos) == separator) {
                pos++; // skip next seperator
            }
        }
        return buffer.toArray(new String[buffer.size()]);
    }

    private static int getSectionLength(String text, char delimiter, char quoteChar, int startPos) {

        char c = text.charAt(startPos);
        if (c == delimiter) return 0;
        if (c == quoteChar) {
            if (startPos + 1 == text.length()) return 1;
            c = text.charAt(startPos + 1);
            if (c == delimiter) return 1;
            for (int pos = startPos + 1, n = text.length(); pos < n; pos++) {
                c = text.charAt(pos);
                if (c == quoteChar) return pos - startPos + 1;
            }
            return text.length() - startPos;
        }
        int pos = text.indexOf(delimiter, startPos);
        if (pos == -1) return text.length() - startPos;
        return pos - startPos;
    }
}
