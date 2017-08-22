/* OpenDA v2.4.1 
* Copyright (c) 2017 OpenDA Association 
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/
package org.openda.observers;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IStochObserver;

public class ObserverUtils {
	private int numberOfObsValues=0;
	private Vector<String> obsIds = new Vector<String>();
	private Vector<Integer> indexFirst = new Vector<Integer>();
	private Vector<Integer> indexLast = new Vector<Integer>();
	private double[] obsTimes = null;


	public ObserverUtils(IObservationDescriptions observationDescriptions){
		this.numberOfObsValues = observationDescriptions.getObservationCount();
		this.obsTimes = new double[numberOfObsValues];
		List<IPrevExchangeItem> items = null;
        boolean gelukt=false;
		boolean whithExchangeItems=true;
		try{
			items = observationDescriptions.getExchangeItems();
		}catch (Exception e) {
			items = new ArrayList<IPrevExchangeItem>(); //Empty list
			whithExchangeItems=false;
		}
		if(whithExchangeItems && items!=null){
			int indFirst = 0;
			int indLast = 0;
			for(IPrevExchangeItem item : items){ // assume the exchangeItems are in the
				String id = item.getId();
				int n = 0;
				double times[] = item.getTimes();
				if (times != null) {
					n = times.length;
				}
				indLast = indFirst + n -1;
				this.indexFirst.add(indFirst);
				this.indexLast.add(indLast);
				this.obsIds.add(id);
				if(n>0){
					System.arraycopy(times, 0, obsTimes, indFirst, n);
				}
				indFirst = indLast+1;
			}
			gelukt=true;
		}else{
		 	try {
				String[] keys = observationDescriptions.getPropertyKeys();
				String[] ids = null;
				boolean notDone = true;
				for (int i = 0; i < keys.length && notDone; i++) {
					if (keys[i].toLowerCase().equals("id") ||
							keys[i].toLowerCase().equals("name")) {
						notDone = false;
						ids = observationDescriptions.getStringProperties(keys[i]);
					}

				}
				if (!notDone) {
					for (int i = 0; i < numberOfObsValues; i++) {
						this.indexFirst.add(i);
						this.indexLast.add(i);
						this.obsIds.add(ids[i]);
						this.obsTimes[i] = 0.0;
					}
				}
				gelukt=true;
			}
			catch(Exception e){
				System.out.println("Cannot get observation ID using name/id property. I'll just make up an index_<num> ID");
			}
			if (!gelukt){
		 		//if there is no metadata then use the index
				for(int i=0;i<numberOfObsValues;i++) {
					this.indexFirst.add(i);
					this.indexLast.add(i);
					this.obsIds.add("index_" + i);
					this.obsTimes[i] = 0.0;
				}
			}
		}
	}

	public ObserverUtils(IStochObserver observer){
		this(observer.getObservationDescriptions());
	}

	public String[] getObsIds(){
		String[] results = new String[this.numberOfObsValues];
		for(int i=0;i<this.obsIds.size();i++){
			for(int j=this.indexFirst.get(i);j<=indexLast.get(i);j++){
				results[j] = this.obsIds.get(i);
			}
		}
		return results;
	}

	public double[] getObsTimeOffsets(double refTime){
		double[] result = new double[this.numberOfObsValues];
		for(int i=0;i<this.numberOfObsValues;i++){
			result[i] = this.obsTimes[i] - refTime;
		}
		return result;
	}
}
