package org.openda.geolab;

import junit.framework.TestCase;

import java.util.LinkedHashMap;

public class CalLibAlgorithmSettingsTest extends TestCase {

	public void testGetConfigString() {
		CalLibAlgorithmSettings calLibAlgorithmSettings = new CalLibAlgorithmSettings(DudXmlTestString);
		String configString = calLibAlgorithmSettings.getConfigString();
		assertTrue(configString.contains("outerLoop maxIterations=\"50.0\" absTolerance=\"1.0E-4\" ")&&
			configString.contains("<lineSearch maxIterations=\"50.0\" maxRelStepSize=\"3.0\">")&&
			configString.contains("backTracking shorteningFactor=\"0.5\" startIterationNegativeLook=\"3.0\""));
	}

	public void testSetAttributeValue() {
		String configTemplate = (new CalibrationLibraryDudAlgorithm()).getConfigStringTemplate();
		CalLibAlgorithmSettings calLibAlgorithmSettings = new CalLibAlgorithmSettings(configTemplate);
		LinkedHashMap<String, CalLibAlgorithmSetting> settings = calLibAlgorithmSettings.getSettings();
		assertEquals(8, settings.size());
		CalLibAlgorithmSetting setting = settings.get("lineSearch@backTracking@shorteningFactor");
		assertEquals("lineSearch@backTracking@shorteningFactor", setting.getName());
		assertEquals(0.5, setting.getDefaultValue());
		assertEquals(0.5, setting.getValue());
		calLibAlgorithmSettings.setAttributeValue("lineSearch@backTracking@shorteningFactor", .85);
		assertEquals(0.5, setting.getDefaultValue());
		assertEquals(0.85, setting.getValue());
		calLibAlgorithmSettings.setAttributeValue("lineSearch@maxIterations", 70);
		calLibAlgorithmSettings.setAttributeValue("lineSearch@backTracking@startIterationNegativeLook", 4);
		String configString = calLibAlgorithmSettings.getConfigString();
		assertTrue(configString.contains("<lineSearch maxIterations=\"70.0\" maxRelStepSize=\"3.0\">")&&
			configString.contains("backTracking shorteningFactor=\"0.85\" startIterationNegativeLook=\"4.0\""));
	}

	private static final String DudXmlTestString = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
		"<DudConfig>\n" +
		"\t<outerLoop maxIterations=\"outerLoop@maxIterations:50\" absTolerance=\"outerLoop@absTolerance:0.0001\" relTolerance=\"outerLoop@relTolerance:0.001\" relToleranceLinearCost=\"outerLoop@relToleranceLinearCost:0.0001\"/>\n" +
		"\t<lineSearch maxIterations=\"lineSearch@maxIterations:50\" maxRelStepSize=\"lineSearch@maxRelStepSize:3\">\n" +
		"\t\t<backTracking shorteningFactor=\"lineSearch@backTracking@shorteningFactor:0.5\" startIterationNegativeLook=\"lineSearch@backTracking@startIterationNegativeLook:3\"/>\n" +		"\t</lineSearch>\n" +
		"</DudConfig>\n";
}
