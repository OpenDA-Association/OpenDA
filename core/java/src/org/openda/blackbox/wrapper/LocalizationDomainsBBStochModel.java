package org.openda.blackbox.wrapper;

import org.openda.blackbox.config.BBStochModelVectorConfig;
import org.openda.interfaces.ILocalizationDomains;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IPrevExchangeItem;

import java.util.ArrayList;
import java.util.Collection;

public class LocalizationDomainsBBStochModel implements ILocalizationDomains {

	private ArrayList<String> domainIds;
	private ArrayList<Collection<BBStochModelVectorConfig>> predictorItems;


	public LocalizationDomainsBBStochModel(ArrayList<String> stateIds, ArrayList<Collection<BBStochModelVectorConfig>> predictorItems) {

		this.domainIds = stateIds;
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
