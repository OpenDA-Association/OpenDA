package org.openda.geolab;

class CalLibAlgorithmSetting {

	private String name;
	private double defaultValue;
	private double value;

	CalLibAlgorithmSetting(String name, double defaultValue) {
		this.name = name;
		this.defaultValue = defaultValue;
		this.value = defaultValue;
	}

	String getName() {
		return name;
	}

	double getDefaultValue() {
		return defaultValue;
	}

	void setValue(double value) {
		this.value = value;
	}

	double getValue() {
		return value;
	}
}
