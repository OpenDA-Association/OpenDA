package org.openda.blackbox.config;

import javafx.util.Pair;

import java.util.ArrayList;

/**
 * Created by hummel on 07-Aug-15.
 */
public class BBBoundaryMappingConfig {
	private int operationType;
	private ArrayList<Pair<String, String>> mappingExchangeItems;

	public BBBoundaryMappingConfig(int operationType, ArrayList<Pair<String, String>> mappingExchangeItems) {
		this.operationType = operationType;
		this.mappingExchangeItems = mappingExchangeItems;
	}

	public int getOperationType() {
		return operationType;
	}

	public ArrayList<Pair<String, String>> getMappingExchangeItems() {
		return mappingExchangeItems;
	}
}
