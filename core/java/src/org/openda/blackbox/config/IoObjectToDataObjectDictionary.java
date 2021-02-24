/*
* Copyright (c) 2021 OpenDA Association 
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
