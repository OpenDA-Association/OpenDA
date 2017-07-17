package org.openda.model_dflowfm;

import org.openda.exchange.DoubleExchangeItem;

/**
 * Created by pelgrim on 21-Jun-17.
 */
public class DFlowFMCalibrationFactorExchangeItem extends DoubleExchangeItem {
	private int lineNumber;

	public DFlowFMCalibrationFactorExchangeItem(String id, double value, int lineNumber) {
		super(id, value);
		this.lineNumber = lineNumber;
	}

	public int getLineNumber() {
		return lineNumber;
	}
}
