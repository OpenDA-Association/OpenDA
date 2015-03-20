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
