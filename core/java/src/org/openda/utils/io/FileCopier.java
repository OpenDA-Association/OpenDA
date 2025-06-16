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

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.text.SimpleDateFormat;

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IConfigurable;
import org.openda.utils.Results;

/**
 * Utility class to copy a file. If the destination file
 * already exists, then it is overwritten.
 *
 * @author Arno Kockx
 */
public class FileCopier implements IConfigurable {

    /**
     * Utility class to copy a file. If the destination file
     * already exists, then it is overwritten.
     *
     * @param workingDir the working directory.
     * @param arguments should be: <sourceFilePath> <destinationFilePath>
     *        where <sourceFilePath> is the pathname of the source file to copy (relative to working directory)
     *        and <destinationFilePath> is the pathname of the destination file (relative to working directory).
     * @throws IllegalArgumentException when specified arguments are incorrect.
     * @throws RuntimeException when copying fails.
     */
    
    public void initialize(File workingDir, String[] arguments) {
        //get arguments.
        if (arguments == null || arguments.length < 2 || arguments[0] == null || arguments[1] == null) {
			throw new IllegalArgumentException("Wrong number of arguments supplied."
				+ " The command line arguments should be: <sourceFilePath> <destinationFilePath> and optional arguments currentTimeFormattingStringPrefix=<pattern> or currentTimeFormattingStringPostfix=<pattern>."
				+ " Where <sourceFilePath> is the pathname of the source file to copy (relative to working directory)"
				+ " and <destinationFilePath> is the pathname of the destination file (relative to working directory)."
				+ " Optional arguments can be used to add a prefix or postfix to the destination file name based on the current time, using a specified date format pattern.");
		}

        File sourceFile = new File(workingDir, arguments[0]);
        if (!sourceFile.exists()) {
            throw new IllegalArgumentException(this.getClass().getSimpleName() + ": source file '"
                    + sourceFile.getAbsolutePath() + "' does not exist.");
        }
        if (!sourceFile.isFile()) {
            throw new IllegalArgumentException(this.getClass().getSimpleName() + ": source file '"
                    + sourceFile.getAbsolutePath() + "' is not a file.");
        }

		String fileNameWithoutExtension = getDestFileName(arguments);

		File destinationFile = new File(workingDir, fileNameWithoutExtension);
        try {
			checkDestinationFile(destinationFile);

			Results.putMessage(this.getClass().getSimpleName() + ": copying file "
				+ sourceFile.getAbsolutePath() + " to " + destinationFile.getAbsolutePath());
			if (arguments.length > 2) {
				Files.copy(sourceFile.toPath(), destinationFile.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
				return;
			}
			// Alternatively, you can use BBUtils.copyFile if you want to use the OpenDA blackbox utility:
			BBUtils.copyFile(sourceFile, destinationFile);
        } catch (IOException e) {
            throw new RuntimeException(this.getClass().getSimpleName() + ": problem while copying file "
                    + sourceFile.getAbsolutePath() + ". Message was: " + e.getMessage(), e);
        }
    }

	private static void checkDestinationFile(File destinationFile) throws IOException {
		if (!destinationFile.exists()) {
			File parentFile = destinationFile.getParentFile();
			if (!parentFile.exists() && !destinationFile.mkdirs()) throw new RuntimeException("Could not create destination directory: " + destinationFile.getParentFile().getAbsolutePath());
			boolean newFile = destinationFile.createNewFile();
			if (!destinationFile.exists() && !newFile) throw new RuntimeException("Could not create destination file: " + destinationFile.getAbsolutePath());
		}
	}

	private static String getDestFileName(String[] arguments) {
		String prefix = "";
		String postfix = "";

		for (int i = 2; i < arguments.length; i++) {
			String[] split = arguments[i].split("=");
			if (split.length != 2)
				throw new RuntimeException(String.format("Argument %s not a valid key=value pair. Only supply arguments currentTimeFormattingStringPrefix=<pattern> or currentTimeFormattingStringPostfix=<pattern>", arguments[i]));
			if (split[0].equals("currentTimeFormattingStringPrefix")) {
				String currentTimeString = new SimpleDateFormat(split[1]).format(System.currentTimeMillis());
				prefix = currentTimeString + "_";
				continue;
			}
			if (split[0].equals("currentTimeFormattingStringPostfix")) {
				String currentTimeString = new SimpleDateFormat(split[1]).format(System.currentTimeMillis());
				postfix = "_" + currentTimeString;
				continue;
			}
			throw new RuntimeException(String.format("Argument %s not a valid key=value pair. Only supply arguments currentTimeFormattingStringPrefix=<pattern> or currentTimeFormattingStringPostfix=<pattern>", arguments[i]));
		}

		File file = new File(arguments[1]);
		String fileName = file.getName();
		String newFileName = prefix + BBUtils.getFileNameWithoutExtension(fileName) + postfix + BBUtils.getFileExtension(fileName);
		File newFile = new File(file.getParentFile(), newFileName);
		return newFile.getPath();
	}
}
