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