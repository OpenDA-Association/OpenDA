package org.openda.blackbox.config;

import java.util.Map;

/**
 * Created by hummel on 07-Aug-15.
 */
public class BBBoundaryMappingConfig {
	private int operationType;
	private Map<String, String> mappingExchangeItems;

	public BBBoundaryMappingConfig(int operationType, Map<String, String> mappingExchangeItems) {
		this.operationType = operationType;
		this.mappingExchangeItems = mappingExchangeItems;
	}

	public int getOperationType() {
		return this.operationType;
	}

	public Map<String, String> getMappingExchangeItems() {
		return this.mappingExchangeItems;
	}
}
