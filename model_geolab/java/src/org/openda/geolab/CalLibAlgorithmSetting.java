package org.openda.geolab;

class CalLibAlgorithmSetting {

	private String name;
	private double defaultValue;
	private double value;
	private int defaultValueInt = Integer.MIN_VALUE;
	private int valueInt = Integer.MIN_VALUE;
	private boolean isInt = false;

	CalLibAlgorithmSetting(String name, String defaultValue) {
		this.name = name;
		parseValue(defaultValue);
	}

	private void parseValue(String defaultValue) {
		if (defaultValue.contains(".")) {
			this.defaultValue = Double.parseDouble(defaultValue);
			this.value = this.defaultValue;
		}
		else {
			this.defaultValueInt = Integer.parseInt(defaultValue);
			this.valueInt = this.defaultValueInt;
			this.isInt = true;
		}
	}

	String getName() {
		return name;
	}

	String getDefaultValue() {
		return isInt?String.valueOf(defaultValueInt):String.valueOf(defaultValue);
	}

	void setValue(String value) {
		if (isInt && value.contains(".")) {
			throw new RuntimeException("CalLibAlgorithmSetting.setValue(): setting double value for integer settings-attribute");
		}
		parseValue(value);
	}

	void setValue(double value) {
		if (isInt) {
			throw new RuntimeException("CalLibAlgorithmSetting.setValue(): setting double value for integer settings-attribute");
		}
		this.value = value;
	}

	void setValue(int value) {
		if (!isInt) {
			throw new RuntimeException("CalLibAlgorithmSetting.setValue(): setting integer value for double settings-attribute");
		}
		this.valueInt = value;
	}

	String getValue() {
		return isInt?String.valueOf(valueInt):String.valueOf(value);
	}
}
