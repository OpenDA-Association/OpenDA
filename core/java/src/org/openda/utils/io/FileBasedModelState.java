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
package org.openda.utils.io;
import org.openda.interfaces.IModelState;
import org.openda.utils.RelativePath;

import java.io.*;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

/**
 * Modelstate that consists out of a list of files and/or directories.
 */
public class FileBasedModelState implements IModelState{

	private final static int ZIP_BUFFER_SIZE = 4096;

	private File dirContainingModelstateFiles = null;
	private ArrayList<File> filesInModelState = new ArrayList<File>();
	private File zippedStateFile = null;

	/**
	 * Empty constructor.
	 */
	public FileBasedModelState() {
	}

	/**
	 * Convenience constructor for creating a FileBasedModelState containing one file
	 * @param dirContainingModelstateFiles The directory that contains the model state file
	 * @param stateFileName Name of the state file
	 */
	public FileBasedModelState(File dirContainingModelstateFiles, String stateFileName) {
		setDirContainingModelstateFiles(dirContainingModelstateFiles);
		addFile(new File(stateFileName));
	}

	/**
	 * Convenience constructor for creating a FileBasedModelState containing a list of files
	 *
	 * @param dirContainingModelstateFiles The directory that contains the model state file
	 * @param stateFileNames				Names of the state files
	 */
	public FileBasedModelState(File dirContainingModelstateFiles, String[] stateFileNames) {
		setDirContainingModelstateFiles(dirContainingModelstateFiles);
		for (String stateFileName : stateFileNames) {
			addFile(new File(stateFileName));
		}
	}

	/**
	 * Convenience constructor for creating a FileBasedModelState containing a directory and its files
	 *
	 * @param stateFilesDirectory The directory that contains the model state file
	 */
	public FileBasedModelState(File stateFilesDirectory) {
		setDirContainingModelstateFiles(stateFilesDirectory);
		addFilesInDirectory(stateFilesDirectory);
	}

	private void addFilesInDirectory(File stateFilesDirectory) {
		for (File fileInDirectory : stateFilesDirectory.listFiles()) {
			if (fileInDirectory.isDirectory()) {
				addFilesInDirectory(fileInDirectory);
			} else {
				addFile(fileInDirectory);
			}
		}
	}

	/***
	 * Set the directory that, in case of making a model state persistent, contains the model state file(s).
	 * @param dirContainingModelstateFiles The directory that does or will contain the model state file(s)
	 */
	public void setDirContainingModelstateFiles(File dirContainingModelstateFiles) {
		if (!dirContainingModelstateFiles.isAbsolute()) {
			throw new IllegalArgumentException(this.getClass().getName() +
					": The model state's working directory must be absolute: " + dirContainingModelstateFiles.getPath());
		}
		this.dirContainingModelstateFiles = dirContainingModelstateFiles;
	}

	/**
	 * Get the directory that, in case of making a model state persistent, contains the model state file(s).
	 * @return dirContainingModelstateFiles
	 */
	public File getDirContainingModelStateFiles(){
		return this.dirContainingModelstateFiles;
	}
	
	/**
	 * Add a file to the model state
	 * @param file The file to be added
	 */
	public void addFile(File file) {
		if (dirContainingModelstateFiles == null) {
			throw new IllegalStateException(this.getClass().getName() + ": dirContainingModelstateFiles not set yet");
		}
		if (file.isDirectory()) {
			// todo: recurse, or implement explicit addDirectory method()
			throw new IllegalArgumentException(this.getClass().getName() +
					": File to be added can not be a directory: " + file.getAbsolutePath());
		}
		filesInModelState.add(file);
	}


	/***
	 * Get files that were added to the model state. 
	 * @return The files that are part of the model state, expressed as absolute files.
	 */
	public ArrayList<File> getFilesInModelState() {
		ArrayList<File> absoluteFiles = new ArrayList<File>();
		for (File file : filesInModelState) {
			if (file.isAbsolute()) {
				absoluteFiles.add(file);
			} else {
				absoluteFiles.add(new File(dirContainingModelstateFiles, file.getPath()));
			}
		}
		return absoluteFiles;
	}

	/***
	 * Set zip file, in case of making a model state persistent, will contain the zipped model state file(s),
	 * or that, in case of restoring a model state, contains the model state file(s) to be unzipped.
	 * @param zippedStateFile The zip file that contains the model state file(s).
	 */
	public void setZippedStateFile(File zippedStateFile) {
		if (!zippedStateFile.getName().toLowerCase().endsWith(".zip")) {
			throw new UnsupportedOperationException(
					"org.openda.utils.io.FileBasedModelState.loadPersistentState(): Not implemented for non zip files.");
		}
		this.zippedStateFile = zippedStateFile;
	}

	/**
	 * Get the zip file, in case of making a model state persistent, will contain the zipped model state file(s),
	 * or that, in case of restoring a model state, contains the model state file(s) to be unzipped.
	 * @return zippedStateFile The zip file that contains the model state file(s).
	 */
	public File getZippedStateFile() {return this.zippedStateFile;}

	/**
	 * Save the model state, a collection of files, to zip file
	 */
	public void savePersistentState(File stateFile) {
		zippedStateFile = stateFile;
		zipFiles();
	}

	/**
	 * Read the model state, a collection of files, from zip file
	 * @param stateFile File containing state
	 */
	public void loadState(File stateFile) {
		if (!stateFile.exists()) {
			throw new IllegalStateException(this.getClass().getName() + ": State file does not exist: "
					+ stateFile.getAbsolutePath());
		}
		zippedStateFile = stateFile;
	}

	public static FileBasedModelState loadPersistenState(File algorithmStateFile, File directoryForRestartFiles) {
		FileBasedModelState modelState = new FileBasedModelState();
		modelState.setDirContainingModelstateFiles(directoryForRestartFiles);
		modelState.setZippedStateFile(algorithmStateFile);
		modelState.restoreState();
		return modelState;
	}

	/**
	 * Read the model state, a collection of files, from zip file
	 */
	public void restoreState() {
		unzipFiles();
	}

	public void releaseState(File directoryContainingModelStateFiles) {

		dirContainingModelstateFiles = directoryContainingModelStateFiles;

		if (dirContainingModelstateFiles == null) {
			throw new IllegalArgumentException(this.getClass().getName() +
					": Directory containing model state files not set yet");
		}
		if (!dirContainingModelstateFiles.exists()) {
			throw new IllegalArgumentException(this.getClass().getName() +
					": Directory containing model state files not found: " +
					directoryContainingModelStateFiles.getAbsolutePath());
		}

        // remove files, loop only through unique file names:
        Set<File> filesSet = new LinkedHashSet<File>(filesInModelState);
        for (File fileInModelState : filesSet) {
			File fileToBeDeleted;
			if (fileInModelState.isAbsolute()) {
				fileToBeDeleted = fileInModelState;
			} else {
				fileToBeDeleted = new File(dirContainingModelstateFiles, fileInModelState.getPath());
			}
			if (!fileToBeDeleted.exists()) {
				throw new IllegalArgumentException(this.getClass().getName() +
						": File to be deleted can not be found: " + fileToBeDeleted.getAbsolutePath());
			}
			fileToBeDeleted.delete();
		}
		deleteAllEmptySubdirsRecursively(directoryContainingModelStateFiles);
		directoryContainingModelStateFiles.delete();
	}

	private static void deleteAllEmptySubdirsRecursively(File directoryContainingModelStateFiles) {
		File[] files = directoryContainingModelStateFiles.listFiles();
		if (files == null) return;
		for (File file : files) {
			if (file.isDirectory()) {
				File[] filesInSubdir = file.listFiles();
				if (filesInSubdir != null && filesInSubdir.length > 0) deleteAllEmptySubdirsRecursively(file);
				file.delete();
			}
		}
	}

	private void zipFiles() {

		if (dirContainingModelstateFiles == null) {
			throw new IllegalStateException(this.getClass().getName() + ": dirContainingModelstateFiles not set yet");
		}
		if (zippedStateFile == null) {
			throw new IllegalStateException(this.getClass().getName() + ": zippedStateFile not set yet");
		}

		File[] filesToBeZipped = new File[filesInModelState.size()];

		for (int i = 0; i < filesInModelState.size(); i++) {
			File fileInModelState = filesInModelState.get(i);
			if (fileInModelState.isAbsolute()) {
				filesToBeZipped[i] = new File(RelativePath.getRelativePath(
						dirContainingModelstateFiles, fileInModelState))	;
			} else {
				filesToBeZipped[i] = fileInModelState;
			}
			if (!new File(dirContainingModelstateFiles, filesToBeZipped[i].toString()).exists()) {
				throw new IllegalArgumentException(this.getClass().getName() +
						": File to be added can not be found: " + filesToBeZipped[i].getAbsolutePath());
			}
		}
		Arrays.sort(filesToBeZipped);

		try {
			ZipOutputStream zipStream = new ZipOutputStream(new BufferedOutputStream(
					new FileOutputStream(zippedStateFile)));
			for (File sortedFile : filesToBeZipped) {
				ZipEntry entry = new ZipEntry(sortedFile.getPath().replace('\\', '/'));
				entry.setTime(sortedFile.lastModified());
				zipStream.putNextEntry(entry);
				InputStream fileStream = new FileInputStream(
						new File(dirContainingModelstateFiles, sortedFile.toString()));
				byte[] buffer = new byte[ZIP_BUFFER_SIZE];
				for (int bytesRead; (bytesRead = fileStream.read(buffer)) != -1;) {
					zipStream.write(buffer, 0, bytesRead);
				}
				fileStream.close();
			}
			zipStream.close();
		} catch (IOException e) {
			throw new RuntimeException(this.getClass().getName() + ": Could not zip files: " + e.getMessage());
		}
	}

	private void unzipFiles() {
		if (dirContainingModelstateFiles == null) {
			throw new IllegalStateException(this.getClass().getName() + ": dirContainingModelstateFiles not set yet");
		}
		if (zippedStateFile == null) {
			throw new IllegalStateException(this.getClass().getName() + ": zippedStateFile not set yet");
		}
		if (!dirContainingModelstateFiles.exists()) {
			dirContainingModelstateFiles.mkdir();
		}
		try {
			ZipInputStream zipStream = new ZipInputStream(new BufferedInputStream(
					new FileInputStream(zippedStateFile)));
			byte[] buffer = new byte[ZIP_BUFFER_SIZE];
			for (ZipEntry entry; (entry = zipStream.getNextEntry()) != null;) {
				String fileName;
				if (entry.isDirectory()) {
					fileName = entry.getName().substring(0, entry.getName().length() - 1);
					throw new IllegalStateException(this.getClass().getName() +
							": Restore directories from zip file not supported yet: " + fileName);
				} else {
					fileName = entry.getName();
				}
				File file = new File(dirContainingModelstateFiles, fileName);
				filesInModelState.add(file);
				if (!file.getParentFile().exists()) {
					if (!file.getParentFile().mkdirs()) {
						throw new RuntimeException(this.getClass().getName() +
								": Directory to load state to could not be created: " + file.getParentFile().getAbsolutePath());
					}
				}
				OutputStream fileStream = new FileOutputStream(file);
				try {
					for (int bytesRead; (bytesRead = zipStream.read(buffer)) != -1;) {
						fileStream.write(buffer, 0, bytesRead);
					}
				} finally {
					fileStream.close();
				}
				if (entry.getTime() != -1) file.setLastModified(entry.getTime());
			}
			zipStream.close();
		} catch (Exception e) {
			throw new RuntimeException(this.getClass().getName() + ": Could not unzip files: " + e.getMessage());
		}
	}
}
