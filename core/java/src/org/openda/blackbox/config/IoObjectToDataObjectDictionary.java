package org.openda.blackbox.config;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class IoObjectToDataObjectDictionary {

	private static Map<String, String> map;

	static {
		HashMap<String, String> dictionary = new HashMap<>();

		dictionary.put("org.openda.blackbox.io.TreeVectorIoObject", "org.openda.blackbox.io.TreeVectorDataObject");
		dictionary.put("org.openda.model_efdc.EfdcTimeSeriesIoObject", "org.openda.model_efdc.EfdcTimeSeriesDataObject");
		dictionary.put("org.openda.model_efdc.EfdcRestartFileIoObject", "org.openda.model_efdc.EfdcRestartFileDataObject");
		dictionary.put("org.openda.model_efdc.EfdcInpIoObject", "org.openda.model_efdc.EfdcInpDataObject");
		dictionary.put("org.openda.model_efdc.EfdcGridTimeSeriesIoObject", "org.openda.model_efdc.EfdcGridTimeSeriesDataObject");
		dictionary.put("org.openda.model_efdc.EfdcEventTox2InpIoObject", "org.openda.model_efdc.EfdcEventTox2InpDataObject");
		dictionary.put("org.openda.blackbox.wrapper.DummyTimeInfoIoObject", "org.openda.blackbox.wrapper.DummyTimeInfoDataObject");
		dictionary.put("org.openda.blackbox.wrapper.DummyParametersIoObject", "org.openda.blackbox.wrapper.DummyParametersDataObject");
		dictionary.put("org.openda.blackbox.wrapper.DummyAstroIoObject", "org.openda.blackbox.wrapper.DummyAstroDataObject");

		map = Collections.unmodifiableMap(dictionary);
	}

	static String getNewDataObjectClassName(String oldIoObjectClassName) {
		return map.get(oldIoObjectClassName);
	}
}
