package org.openda.geolab;

import java.util.LinkedHashMap;

class CalLibAlgorithmSettings {

	private LinkedHashMap<String, CalLibAlgorithmSetting> settings = new LinkedHashMap<>();
	private String parsedConfigString = "";
	private final String attrStart = "ATTR_START";
	private final String attrEnd = "ATTR_END";

	CalLibAlgorithmSettings(String configStringTemplate) {
		parseConfigStringTemplate(configStringTemplate);
	}

	void setAttributeValue(String key, String value) {
		getCalLibAlgorithmSetting(key).setValue(value);
	}

	void setAttributeValue(String key, double value) {
		getCalLibAlgorithmSetting(key).setValue(value);
	}

	void setAttributeValue(String key, int value) {
		getCalLibAlgorithmSetting(key).setValue(value);
	}

	String getConfigString() {
		return replaceValuesInConfigString();
	}

	LinkedHashMap<String, CalLibAlgorithmSetting> getSettings() {
		return settings;
	}

	private void parseConfigStringTemplate(String configStringTemplate) {
		String remainingConfigString = configStringTemplate;
		StringBuilder parsedString = new StringBuilder();
		while (remainingConfigString != null) {
			int atPos = remainingConfigString.indexOf('@');
			if (atPos > 0) {
				int startQuotePos = remainingConfigString.substring(0, atPos).lastIndexOf( '\"');
				int endQuotePos = remainingConfigString.substring(atPos).indexOf( '\"');
				if (startQuotePos < 0 || endQuotePos < 0) {
					throw new RuntimeException("Error in config string template near " +
						configStringTemplate.substring(Math.max(0, atPos-20), Math.min(atPos+20, configStringTemplate.length()-1)));
				}
				int replaceStartPos=startQuotePos+1;
				int replaceEndPos=atPos+endQuotePos;
				String[] attribute = remainingConfigString.substring(replaceStartPos, replaceEndPos).split(":");
				String settingName = attribute[0];
				String settingValue = attribute[1];
				settings.put(settingName, new CalLibAlgorithmSetting(settingName, settingValue));

				String attrReplaceString = attrStart + settingName + attrEnd;
				parsedString.append(remainingConfigString, 0, replaceStartPos);
				parsedString.append(attrReplaceString);
				remainingConfigString = remainingConfigString.substring(replaceEndPos);
			}
			else {
				parsedString.append(remainingConfigString);
				remainingConfigString = null;
			}
		}
		parsedConfigString = parsedString.toString();
	}

	private String replaceValuesInConfigString() {
		StringBuilder configString = new StringBuilder();
		String remainingParsedConfigString = parsedConfigString;

		int startPos = remainingParsedConfigString.indexOf(attrStart);
		int endPos = -1;
		if (startPos > 0) {
			endPos = remainingParsedConfigString.indexOf(attrEnd);
		}

		while (startPos > 0) {

			String attributeKey = remainingParsedConfigString.substring(
				startPos+attrStart.length(), endPos);
			endPos += attrEnd.length();

			configString.append(remainingParsedConfigString, 0, startPos);
			configString.append(settings.get(attributeKey).getValue());
			remainingParsedConfigString = remainingParsedConfigString.substring(endPos);

			startPos = remainingParsedConfigString.indexOf(attrStart);
			if (startPos > 0) {
				endPos = remainingParsedConfigString.indexOf(attrEnd);
			} else {
				configString.append(remainingParsedConfigString);
			}
		}
		return configString.toString();
	}

	private CalLibAlgorithmSetting getCalLibAlgorithmSetting(String key) {
		if (!settings.containsKey(key)) {
			throw new RuntimeException("Unknown attribute name: " + key);
		}
		return settings.get(key);
	}
}
