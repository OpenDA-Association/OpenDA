package org.openda.model_dflowfm;

import org.openda.interfaces.IConfigurable;
import org.openda.utils.Results;
import org.openda.utils.generalJavaUtils.StringUtilities;
import org.openda.utils.io.FileSupport;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

public class DFlowFMRestartFilePostProcessor implements IConfigurable {

	public static final String NUMBER_OF_PARTITIONS = "numberOfPartitions";
	public static final String TARGET_RESTART_FILE_NAME_POSTFIX = "targetRestartFileNamePostFix";
	public static final String RUN_ID = "runId";
	public static final String SOURCE_RESTART_FILE_SUB_DIR = "sourceRestartFileSubDir";
	public static final String DELETE_OLDER_RST_FILES = "deleteOlderRstFiles";
	private int numberOfPartitions = 0;
	private String runId;
	private String targetRestartFileNamePostFix;

	@Override
	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length < 2) {
			throw new IllegalArgumentException("DFlowFMRestartFilePostProcessor expects at least two arguments:\n" +
				"<runId> and <targetRestartFileName>");
		}
		String sourceRestartFileSubDir = null;
		boolean deleteOlderRstFiles = false;
		for (int i = 0; i < arguments.length; i++) {
			String argument = arguments[i];
			String[] keyValue = StringUtilities.getKeyValuePair(argument);
			if (keyValue == null || keyValue.length != 2) throw new RuntimeException(String.format("Invalid key=value pair: %s", argument));
			String key = keyValue[0];
			String value = keyValue[1];
			switch (key) {
				case RUN_ID:
					runId = value;
					continue;
				case SOURCE_RESTART_FILE_SUB_DIR:
					sourceRestartFileSubDir = value;
					continue;
				case TARGET_RESTART_FILE_NAME_POSTFIX:
					targetRestartFileNamePostFix = value;
					continue;
				case DELETE_OLDER_RST_FILES:
					deleteOlderRstFiles = Boolean.parseBoolean(value);
					continue;
				case NUMBER_OF_PARTITIONS:
					this.numberOfPartitions = Integer.parseInt(value);
					continue;
				default:
					throw new RuntimeException(String.format("Unknown key %s. Please specify only [%s, %s, %s, %s, %s] as key=value pairs", key, RUN_ID, SOURCE_RESTART_FILE_SUB_DIR, TARGET_RESTART_FILE_NAME_POSTFIX, DELETE_OLDER_RST_FILES, NUMBER_OF_PARTITIONS));
			}
		}
		if (numberOfPartitions == 0) {
			String fileNamePattern = "'" + runId + "_'yyyyMMdd'_'HHmmss'_rst.nc'";
			processRestartFilesForPattern(workingDir, sourceRestartFileSubDir, deleteOlderRstFiles, fileNamePattern, null);
			return;
		}
		for (int i = 0; i < numberOfPartitions; i++) {
			String partition = StringUtilities.padLeft(String.valueOf(i), 4, '0');
			String fileNamePattern = String.format("'%s_%s_'yyyyMMdd'_'HHmmss'_rst.nc'", runId, partition);
			processRestartFilesForPattern(workingDir, sourceRestartFileSubDir, deleteOlderRstFiles, fileNamePattern, partition);
		}
	}

	private void processRestartFilesForPattern(File workingDir, String sourceRestartFileSubDir, boolean deleteOlderRstFiles, String fileNamePattern, String partition) {
		final SimpleDateFormat simpleDateFormat = new SimpleDateFormat(fileNamePattern);
		File sourcesDir = sourceRestartFileSubDir != null ? new File(workingDir, sourceRestartFileSubDir) : workingDir;
		ArrayList<Date> dates = FileSupport.getDatesFromFileNames(fileNamePattern, simpleDateFormat, sourcesDir);
		moveMostRecentRestartFile(workingDir, simpleDateFormat, sourcesDir, dates, partition);
		deleteOlderRestartFiles(deleteOlderRstFiles, simpleDateFormat, sourcesDir, dates);
	}

	private static void deleteOlderRestartFiles(boolean deleteOlderRstFiles, SimpleDateFormat simpleDateFormat, File sourcesDir, ArrayList<Date> dates) {
		if (!deleteOlderRstFiles) return;
		dates.remove(dates.size() - 1);
		for (Date date : dates) {
			String fileNameForDeletion = simpleDateFormat.format(date);
			File rstFileForDeletion = new File(sourcesDir, fileNameForDeletion);
			try {
				Files.deleteIfExists(rstFileForDeletion.toPath());
			} catch (IOException e) {
				Results.putMessage("Failed to delete " + rstFileForDeletion + " due to " + e.getMessage());
			}
		}
	}

	private void moveMostRecentRestartFile(File workingDir, SimpleDateFormat simpleDateFormat, File sourcesDir, ArrayList<Date> dates, String partition) {
		String formattedFileName = simpleDateFormat.format(dates.get(dates.size() - 1));
		File sourceRestartFile = new File(sourcesDir, formattedFileName);
		String targetRestartFileName = partition == null ? String.format("%s_%s", runId, this.targetRestartFileNamePostFix) : String.format("%s_%s_%s", runId, partition, this.targetRestartFileNamePostFix);
		File targetRestartFile = new File(workingDir, targetRestartFileName);
		if (!sourceRestartFile.exists()) throw new RuntimeException("DFlowFMRestartFileWrapper: Source restart file does not exist " + sourceRestartFile);
		try {
			Files.move(sourceRestartFile.toPath(), targetRestartFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
		} catch (IOException e) {
			throw new RuntimeException("DFlowFMRestartFileWrapper: failed to move " + sourceRestartFile + " to " + targetRestartFile + " due to " + e.getMessage(), e);
		}
	}

}
