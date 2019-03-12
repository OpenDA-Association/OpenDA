package org.openda.model_bmi;

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.ILocalizationDomains;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IPrevExchangeItem;

import java.util.*;

public class BmiModelLocalizationDomains implements ILocalizationDomains {

	private BmiModelInstance modelInstance;
	private LinkedHashMap<String, IExchangeItem> modelStateExchangeItems;
	private Map<String, String> observationIdSourceVectorIdMap;

	public BmiModelLocalizationDomains(LinkedHashMap<String, IExchangeItem> modelStateExchangeItems) {
		this.modelStateExchangeItems = modelStateExchangeItems;
	}


	@Override
	public void setStateDomainObservations(int iDomain, IObservationDescriptions observationDescriptions) {
		throw new RuntimeException("org.openda.model_bmi.BmiModelLocalizationDomains.setStateDomainObservations() not implemented yet");

	}

	@Override
	public int getStateDomainCount() {
		return modelStateExchangeItems.size();
	}

	@Override
	public int[] getObservationSelector(IObservationDescriptions observationDescriptions, int iDomain) {
		IExchangeItem[] values = modelStateExchangeItems.values().toArray(new IExchangeItem[0]);
		BmiStateExchangeItem bmiStateExchangeItem = (BmiStateExchangeItem) values[iDomain];
		List<String> ids = Arrays.asList(bmiStateExchangeItem.getIds());
		List<IPrevExchangeItem> exchangeItems = observationDescriptions.getExchangeItems();
		List<Integer> indices = new ArrayList<>();
		// Select index of of observation exchange item if the id is present in the bmi state exchange item
		for (int i = 0; i < exchangeItems.size(); i++) {
			IPrevExchangeItem exchangeItem = exchangeItems.get(i);
			String id = exchangeItem.getId();
			String mappedId = observationIdSourceVectorIdMap.get(id);
			if (mappedId == null || !ids.contains(mappedId)) continue;
			indices.add(i);
		}
		int[] ints = new int[indices.size()];
		for (int i = 0; i < indices.size(); i++) {
			ints[i] = indices.get(i);
		}
		return ints;
	}

	@Override
	public void setObservationSourceMap(Map<String, String> observationIdSourceVectorIdMap) {
		this.observationIdSourceVectorIdMap = observationIdSourceVectorIdMap;
	}
}
