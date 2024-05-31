package org.openda.model_wanda_seawat;

import org.ini4j.Ini;
import org.openda.exchange.AbstractDataObject;

import java.io.File;
import java.io.IOException;

public class WandaSeawatModelRunIniFileDataObject extends AbstractDataObject {
	@Override
	public void finish() {


	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		File file = new File(workingDir, arguments[0]);
		Ini ini = new Ini();
		try {
			ini.load(file);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}

		System.out.println(ini.getFile().getAbsolutePath() + " exists: " + ini.getFile().exists());
	}
}
