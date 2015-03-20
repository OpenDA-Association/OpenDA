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
    @Override
    public void initialize(File workingDir, String[] arguments) {
        //get arguments.
        if (arguments == null || arguments.length != 2 || arguments[0] == null || arguments[1] == null) {
            throw new IllegalArgumentException("Wrong number of arguments supplied."
                    + " The command line arguments should be: <sourceFilePath> <destinationFilePath>"
                    + " Where <sourceFilePath> is the pathname of the source file to copy (relative to working directory)"
                    + " and <destinationFilePath> is the pathname of the destination file (relative to working directory).");
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

        File destinationFile = new File(workingDir, arguments[1]);

        //copy file.
        Results.putMessage(this.getClass().getSimpleName() + ": copying file "
                + sourceFile.getAbsolutePath() + " to " + destinationFile.getAbsolutePath());
        try {
            BBUtils.copyFile(sourceFile, destinationFile);
        } catch (IOException e) {
            throw new RuntimeException(this.getClass().getSimpleName() + ": problem while copying file "
                    + sourceFile.getAbsolutePath() + ". Message was: " + e.getMessage(), e);
        }
    }
}
