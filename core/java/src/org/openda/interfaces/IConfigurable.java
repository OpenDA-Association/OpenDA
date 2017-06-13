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

package org.openda.interfaces;

import java.io.File;

/**
 * Item that can be configured as part of an OpenDA application.
 */
public interface IConfigurable {
    /**
     * Initialize the configurable. Specify what its "working directory" is (usually meaning: the directory
     * where its configuration file is), and provide its arguments.
     * @param workingDir The directory indicating the where the configurable is started (not as 'current
     *                   working directory', but as the root path for its configuration files etc).
     * @param arguments The arguments needed to initialize. Typically the first argument can be a configuration
     *                  file name string, specified relative to the working dir.
     */
    public void initialize(File workingDir, String[] arguments);
}
