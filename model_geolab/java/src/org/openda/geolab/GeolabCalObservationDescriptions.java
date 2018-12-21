package org.openda.geolab;

import org.openda.exchange.DoublesExchangeItem;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;

import java.util.ArrayList;
import java.util.List;

public class GeolabCalObservationDescriptions implements IObservationDescriptions {

	private List<IPrevExchangeItem> exchangeItems = new ArrayList<>();

	GeolabCalObservationDescriptions(double[] observations) {
		DoublesExchangeItem exchangeItem = new DoublesExchangeItem("lab-observations",
			IPrevExchangeItem.Role.Output, observations);
		this.exchangeItems.add(exchangeItem);
	}

	@Override
	public List<IPrevExchangeItem> getExchangeItems() {
		return exchangeItems;
	}

	@Override
	public IVector getValueProperties(String Key) {
		throw new RuntimeException("org.openda.geolab.GeolabCalObservationDescriptions.getValueProperties() not implemented yet");
	}

	@Override
	public String[] getStringProperties(String Key) {
		throw new RuntimeException("org.openda.geolab.GeolabCalObservationDescriptions.getStringProperties() not implemented yet");
	}

	@Override
	public String[] getPropertyKeys() {
		throw new RuntimeException("org.openda.geolab.GeolabCalObservationDescriptions.getPropertyKeys() not implemented yet");
	}

	@Override
	public int getPropertyCount() {
		throw new RuntimeException("org.openda.geolab.GeolabCalObservationDescriptions.getPropertyCount() not implemented yet");
	}

	@Override
	public int getObservationCount() {
		throw new RuntimeException("org.openda.geolab.GeolabCalObservationDescriptions.getObservationCount() not implemented yet");
	}

	@Override
	public ITime[] getTimes() {
		return null;
	}
}
