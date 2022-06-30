package org.openda.model_dflowfm;

import org.openda.interfaces.IConfigurable;
import org.openda.utils.generalJavaUtils.StringUtilities;

import java.io.File;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;

public class DFlowFMRestartFilePostProcessor implements IConfigurable {

	public static final String TARGET_RESTART_FILE_NAME = "targetRestartFileName";
	public static final String RUN_ID = "runId";

	@Override
	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length < 2) {
			throw new IllegalArgumentException("DFlowFMRestartFilePostProcessor expects at least two arguments:\n" +
				"<runId> and <targetRestartFileName>");
		}
		String targetRestartFileName = null;
		String runId = null;
		for (int i = 0; i < arguments.length; i++) {
			String argument = arguments[i];
			String[] keyValue = StringUtilities.getKeyValuePair(argument);
			String key = keyValue[0];
			String value = keyValue[1];
			switch (key) {
				case RUN_ID:
					runId = value;
					continue;
				case TARGET_RESTART_FILE_NAME:
					targetRestartFileName = value;
					continue;
				default:
					throw new RuntimeException("Unknown key " + key + ". Please specify only " + RUN_ID + " and " + TARGET_RESTART_FILE_NAME + " as key=value pair");
			}
		}
		String fileNamePattern = "'" + runId + "_'yyyyMMdd'_'HHmmss'_rst.nc'";
		final SimpleDateFormat simpleDateFormat = new SimpleDateFormat(fileNamePattern);
		ArrayList<Date> dates = new ArrayList<>();
		File[] files = workingDir.listFiles(pathname -> {
			try {
				dates.add(simpleDateFormat.parse(pathname.getName()));
				return true;
			} catch (ParseException e) {
				return false;
			}
		});
		if (files == null) throw new RuntimeException("DFlowFMRestartFileWrapper: no restart file found with pattern " + fileNamePattern);
		Collections.sort(dates);
		File sourceRestartFile = new File(workingDir, simpleDateFormat.format(dates.get(dates.size() - 1)));
		File targetRestartFile = new File(workingDir, targetRestartFileName);
		boolean succeeded = sourceRestartFile.renameTo(targetRestartFile);
		if (!succeeded) throw new RuntimeException("DFlowFMRestartFileWrapper: failed to rename " + sourceRestartFile + " to " + targetRestartFile);
	}
}
