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
package org.openda.model_delwaq;
import java.io.File;
import java.util.regex.Pattern;

/**
 * Created with IntelliJ IDEA.
 * User: bos_en
 * Date: 8/27/13
 */
public class delwaqInpConstants extends delwaqInpFile{

	public void initialize(File workingDir, String[] arguments) {
		name_line_pattern = Pattern.compile("\\s*?'[\\w ]+?'\\s*?.*?"); // e.g.: " 'NCRatGreen'"
		name_word_pattern = Pattern.compile("'[\\w ]+?'"); // e.g.: "'DetC'"
		name_word_start = 1;
		name_word_stop = -1;
		value_line_pattern = Pattern.compile("\\s*?[\\d+-]+?[.]\\d+?[eE][+-]\\d+\\s*?.*?"); // e.g.: " 1.60000e-001 ; NCRatGreen"
		value_word_pattern = Pattern.compile("\\d+?[.]\\d+?[eE][+-]\\d+"); // e.g.: "1.00000e+000"
		itemDescription = "A Delwaq constant.";
		format = "0.#####E0";
		super.initialize(workingDir, arguments);
	}
}
