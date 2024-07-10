package org.openda.model_wanda_seawat;

import org.ini4j.Ini;
import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.IntExchangeItem;
import org.openda.exchange.StringExchangeItem;
import org.openda.interfaces.IExchangeItem;

import java.io.File;
import java.io.IOException;

public class WandaSeawatModelRunIniFileDataObject extends AbstractDataObject {
	private static final String SECTION_RUN = "run";
	private static final String OPTION_START_DATE_TIME = "startDateTime";
	private static final String OPTION_END_DATE_TIME = "endDateTime";
	private static final String SECTION_PARAMETERS = "parameters";
	private static final String OPTION_BULK_DENSITY = "HTGEOFIL H1.Geoformations.Bulk density formation.kleilaag 1";
	private static final String OPTION_POROSITY = "HTGEOFIL H1.Geoformations.Porosity formation.kleilaag 1";
	private static final String OPTION_MAX_YEARLY_WATER_VOLUME = "HTGEOFIL H1.Max yearly water volume";

	@Override
	public void finish() {
		// Do nothing
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
		dateTimeExchangeItem(ini, OPTION_START_DATE_TIME, "startDateTime");
		dateTimeExchangeItem(ini, OPTION_END_DATE_TIME, "endDateTime");
		doubleExchangeItem(ini, OPTION_BULK_DENSITY, "h1GeoformationsBulkDensity");
		doubleExchangeItem(ini, OPTION_POROSITY, "h1GeoformationsPorosity");
		intExchangeItem(ini, OPTION_MAX_YEARLY_WATER_VOLUME, "h1MaxYearlyWaterVolume");
	}

	private void dateTimeExchangeItem(Ini ini, String optionName, String id) {
		String valueString = ini.get(SECTION_RUN, optionName);
		StringExchangeItem stringExchangeItem = new StringExchangeItem(id, IExchangeItem.Role.InOut, valueString);
		exchangeItems.putIfAbsent(id, stringExchangeItem);
	}
	
	private void doubleExchangeItem(Ini ini, String optionName, String id) {
		String valueString = ini.get(SECTION_PARAMETERS, optionName);
		DoubleExchangeItem doubleExchangeItem = new DoubleExchangeItem(id, IExchangeItem.Role.InOut, Double.parseDouble(valueString));
		exchangeItems.putIfAbsent(id, doubleExchangeItem);
	}

	private void intExchangeItem(Ini ini, String optionName, String id) {
		String valueString = ini.get(SECTION_PARAMETERS, optionName);
		IntExchangeItem intExchangeItem = new IntExchangeItem(id, IExchangeItem.Role.InOut, Integer.parseInt(valueString));
		exchangeItems.putIfAbsent(id, intExchangeItem);
	}
}
