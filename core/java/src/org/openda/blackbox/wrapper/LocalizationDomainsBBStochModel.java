package org.openda.blackbox.wrapper;

import org.openda.interfaces.ILocalizationDomains;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IStochModelInstance;
import org.openda.utils.Array;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

public class LocalizationDomainsBBStochModel implements ILocalizationDomains {

	// What to do with this one?
	private Collection<Collection<String>> stochModels;
	// What to do with this one?
	private Collection<Collection<String>> exchangeItems;
	private ArrayList<Collection<String>> observationSourceIds;


	public LocalizationDomainsBBStochModel(Collection<String> stateIds,
		                                   Map<String, Collection<IStochModelInstance>> stochModelMap,
										   Map<String, Collection<IPrevExchangeItem>> exchangeItemMap,
										   Map<String, Collection<String>> observationIdSourceMap) {

		this.observationSourceIds = new ArrayList<>();
		for(String stateId : stateIds){
			if(observationIdSourceMap.containsKey(stateId))
			{
				observationSourceIds.add(observationIdSourceMap.get(stateId));
			}
			else
			{
				observationSourceIds.add(new ArrayList<String>());
			}
		}
	}

	@Override
	public void setStateDomainObservations(int iDomain, IObservationDescriptions observationDescriptions) {
		// Doing to be done here...
	}

	@Override
	public int getStateDomainCount() {
		return observationSourceIds.size();
	}

	@Override
	public int[] getObservationSelector(IObservationDescriptions observationDescriptions, int iDomain) {
		ArrayList<Integer> result = new ArrayList<>();
		int j = 0;
		for (String observationId : observationDescriptions.getStringProperties("id")) {
			if (this.observationSourceIds.get(iDomain).contains(observationId)) {
				result.add(j);
			}
			j++;
		}
		int[] returnArray = new int[result.size()];
		for (j = 0; j < result.size(); j++)
		{
			returnArray[j] = result.get(j);
		}
		return returnArray;
	}

}
