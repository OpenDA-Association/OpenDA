package org.openda.model_wanda_seawat;

import org.openda.exchange.AbstractDataObject;
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;
import java.util.List;

public class WandaSeawatGridDataObject extends AbstractDataObject {
	@Override
	public void finish() {

	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		File file = new File(workingDir, arguments[0]);
		System.out.println(file.getAbsolutePath() + " exists: " + file.exists());
		List<String> lines = AsciiFileUtils.readLines(file);
		for (String line : lines) {
			System.out.println(line);
		}
	}
}
