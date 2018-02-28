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
package org.openda.utils;
import java.io.IOException;
import java.net.URL;
import java.util.jar.Attributes;
import java.util.jar.Manifest;

/**
 * Utilities for getting version strings, checking versions, etc.
 */
public class VersionUtils {

	public static String getVersionFromManifest(Class aClass) {
		String className = aClass.getSimpleName() + ".class";
		String classPath = aClass.getResource(className).toString();
		if (!classPath.startsWith("jar")) {
			// not from jar
			return "(Development)";
		}

		String manifestPath = classPath.substring(0, classPath.lastIndexOf("!") + 1) +
				"/META-INF/MANIFEST.MF";
		Manifest manifest;
		try {
			manifest = new Manifest(new URL(manifestPath).openStream());
		} catch (IOException e) {
			throw new RuntimeException("Error opening manifest " + manifestPath);
		}
		Attributes attr = manifest.getMainAttributes();
		if (attr != null) {
			String version = attr.getValue("Version");
			if (version != null) {
				return version;
			}
		}

		return null;
	}
}
