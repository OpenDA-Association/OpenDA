package org.openda.model_dflowfm;

import org.openda.interfaces.IConfigurable;
import org.openda.utils.Results;
import org.openda.utils.generalJavaUtils.StringUtilities;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;

public class DFlowFMRestartFilePostProcessor implements IConfigurable {

	public static final String TARGET_RESTART_FILE_NAME = "targetRestartFileName";
	public static final String RUN_ID = "runId";
	public static final String SOURCE_RESTART_FILE_SUB_DIR = "sourceRestartFileSubDir";
	public static final String DELETE_OLDER_RST_FILES = "deleteOlderRstFiles";

	@Override
	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length < 2) {
			throw new IllegalArgumentException("DFlowFMRestartFilePostProcessor expects at least two arguments:\n" +
				"<runId> and <targetRestartFileName>");
		}
		String targetRestartFileName = null;
		String runId = null;
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
				case TARGET_RESTART_FILE_NAME:
					targetRestartFileName = value;
					continue;
				case DELETE_OLDER_RST_FILES:
					deleteOlderRstFiles = Boolean.parseBoolean(value);
					continue;
				default:
					throw new RuntimeException(String.format("Unknown key %s. Please specify only [%s, %s, %s, %s] as key=value pairs", key, RUN_ID, SOURCE_RESTART_FILE_SUB_DIR, TARGET_RESTART_FILE_NAME, DELETE_OLDER_RST_FILES));
			}
		}
		String fileNamePattern = "'" + runId + "_'yyyyMMdd'_'HHmmss'_rst.nc'";
		final SimpleDateFormat simpleDateFormat = new SimpleDateFormat(fileNamePattern);
		ArrayList<Date> dates = new ArrayList<>();
		File sourcesDir = sourceRestartFileSubDir != null ? new File(workingDir, sourceRestartFileSubDir) : workingDir;
		File[] files = sourcesDir.listFiles(pathname -> {
			try {
				dates.add(simpleDateFormat.parse(pathname.getName()));
				return true;
			} catch (ParseException e) {
				return false;
			}
		});
		if (files == null || dates.isEmpty()) throw new RuntimeException(String.format("DFlowFMRestartFileWrapper: no restart file found with pattern %s in %s", fileNamePattern, sourcesDir));
		Collections.sort(dates);
		String formattedFileName = simpleDateFormat.format(dates.get(dates.size() - 1));
		File sourceRestartFile = new File(sourcesDir, formattedFileName);
		File targetRestartFile = new File(workingDir, targetRestartFileName);
		if (!sourceRestartFile.exists()) throw new RuntimeException("DFlowFMRestartFileWrapper: Source restart file does not exist " + sourceRestartFile);
		try {
			Files.move(sourceRestartFile.toPath(), targetRestartFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
		} catch (IOException e) {
			throw new RuntimeException("DFlowFMRestartFileWrapper: failed to move " + sourceRestartFile + " to " + targetRestartFile + " due to " + e.getMessage(), e);
		}
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
}
