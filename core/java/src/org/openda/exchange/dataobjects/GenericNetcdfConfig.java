package org.openda.exchange.dataobjects;

import java.util.ArrayList;
import java.util.HashMap;

public class GenericNetcdfConfig {

//	ArrayList<GenericNetcdfTSConfig> tsConfigs = new ArrayList<>();
//	ArrayList<GenericNetcdfArrayConfig> arrayConfigs = new ArrayList<>();

	HashMap<String, GenericNetcdfTSConfig> tsConfigs = new HashMap<>();
	HashMap<String, GenericNetcdfArrayConfig> arrayConfigs = new HashMap<>();
}
