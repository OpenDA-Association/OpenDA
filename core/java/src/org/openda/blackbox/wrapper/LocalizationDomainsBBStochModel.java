package org.openda.blackbox.wrapper;

import org.openda.blackbox.config.BBNoiseModelConfig;
import org.openda.blackbox.config.BBStochModelVectorConfig;
import org.openda.interfaces.ILocalizationDomains;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IStochModelInstance;
import org.openda.utils.Array;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

public class LocalizationDomainsBBStochModel implements ILocalizationDomains {

	private ArrayList<String> domainIds;
	private ArrayList<Collection<BBStochModelVectorConfig>> predictorItems;
	// TODO (GvdO): Should these unused members be removed?
	private ArrayList<Collection<BBNoiseModelConfig>> noiseModels;
	private ArrayList<Collection<IPrevExchangeItem>> exchangeItems;


	public LocalizationDomainsBBStochModel(ArrayList<String> stateIds,
		                                   ArrayList<Collection<BBNoiseModelConfig>> noiseModels,
										   ArrayList<Collection<IPrevExchangeItem>> exchangeItems,
										   ArrayList<Collection<BBStochModelVectorConfig>> predictorItems) {

		this.domainIds = stateIds;
		this.noiseModels = noiseModels;
		this.exchangeItems = exchangeItems;
		this.predictorItems = predictorItems;
	}

	@Override
	public void setStateDomainObservations(int iDomain, IObservationDescriptions observationDescriptions) {
		// Nothing to be done here...
	}

	@Override
	public int getStateDomainCount() {
		return this.domainIds.size();
	}

	@Override
	public int[] getObservationSelector(IObservationDescriptions observationDescriptions, int iDomain) {

		int count = 0;

		ArrayList<Integer> selection = new ArrayList<>();

		for (IPrevExchangeItem obs_item : observationDescriptions.getExchangeItems()) {
			for ( BBStochModelVectorConfig predictorVectorConfig : predictorItems.get(iDomain)) {
				if (obs_item.getId().equals(predictorVectorConfig.getId())) {
					selection.add(count);
				}
			}
			count++;
		}
		int[] result = new int[selection.size()];
		for (int i =0 ; i < selection.size(); i++){
			result[i] = selection.get(i);
		}
		return result;
	}
}