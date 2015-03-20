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

package org.openda.utils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.openda.blackbox.config.BBUtils;

public class ZipUtils {
    final static int BUFFER_SIZE = 8192;

    private ZipUtils() {
    }

    public static File[] unzipFiles(ZipInputStream zipInputStream, File outputDir) throws IOException {
    	byte[] buffer = new byte[BUFFER_SIZE];
    	List<File> res = new ArrayList<File>();
        for (ZipEntry entry; (entry = zipInputStream.getNextEntry()) != null;) {
            String entryName = entry.getName();
            String fileName;
            if (entry.isDirectory()) {
                fileName = entryName.substring(0, entryName.length() - 1);
            } else {
                fileName = entryName;
            }
            File file = new File(outputDir, fileName);
            res.add(file);
            if (entry.isDirectory()) {
                if (!file.exists()) {
                    boolean successful = file.mkdirs();
                    assert successful;
                }
                continue;
            }

            if (!file.getParentFile().exists()) {
                boolean successful = file.getParentFile().mkdirs();
                assert successful;
            }

            try {
                OutputStream outputStream = new FileOutputStream(file);
                try {
                    copy(zipInputStream, outputStream, buffer);
                } finally {
                    outputStream.close();
                }
            } catch (IOException e) {
                throw e;
            } catch (Exception e) {
                throw new IOException(e.getMessage(), e);
            }
            if (entry.getTime() != -1) {
                @SuppressWarnings({"UnusedDeclaration"})
                boolean successful = file.setLastModified(entry.getTime());
                // assert successful; file.setLastModified returns false when successful
            }

            if (BBUtils.RUNNING_ON_LINUX) {
                String ext = getFileExt(file.getPath());
                if (ext.isEmpty() || ext.equalsIgnoreCase("exe") || ext.equalsIgnoreCase("sh")) {
                    boolean successful = file.setExecutable(true);
                    assert successful;
                }
            }
        }
        return res.toArray(new File[res.size()]);
    }

    private static void copy(InputStream inputStream, OutputStream outputStream, byte[] buffer) throws IOException {
    	if (inputStream == null) {
    		throw new IllegalArgumentException("inputStream == null");
    	}

    	if (outputStream == null) {
    		throw new IllegalArgumentException("outputStream == null");
    	}

    	if (buffer == null) {
    		throw new IllegalArgumentException("buffer == null");
    	}

    	if (buffer.length == 0) {
    		throw new IllegalArgumentException("buffer.length = 0");
    	}

    	for (int bytesRead; (bytesRead = inputStream.read(buffer)) > 0;) {
    		outputStream.write(buffer, 0, bytesRead);
    	}
    }

    private static String getFileExt(String path) {
        for (int i = path.length() - 1; i >= 0; i--) {
            char ch = path.charAt(i);
            if (ch == '/') return "";
            if (ch == '\\') return "";
            if (ch == '.') return path.substring(i + 1);
        }

        return "";
    }
}
