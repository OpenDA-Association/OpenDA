/* V2.2
* Copyright (c) 2015 OpenDA Association
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
package org.openda.model_RainfallRunoffZhang;

import org.openda.model_RainfallRunoffZhang.ASCIITimeSeriesFormatter;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesFormatter;
import org.openda.interfaces.ISelector;
import org.openda.interfaces.IStochObserver;
import org.openda.observers.TimeSeriesStochObserver;
import org.openda.utils.ConfigTree;
import org.openda.utils.IndicesSelector;
import org.openda.utils.Results;

import java.io.File;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

/**
 * Implementation of IStochObserver based on timeSeries in a generic ASCII format.
 * <p>
 * A generic mechanism based on the classes TimeSeries, TimeSeriesFormatter, TimeSeriesStochObserver
 * is used. The actual reading and writing for the input files is done by the class
 * ASCIITimeSeriesFormatter. This class Extends the IStochObserver for TimeSeries-data with configuration
 * facilities and a link to the ASCIITimeSeriesFormatter.
 * <p>
 * The input file for this class has the following format
 * 	<pre>
 *   &lt;?xml version="1.0" encoding="UTF-8"?&gt;<br>
 *   &lt;ASCIIObserver xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"&gt;<br>
 *   	&lt;timeSeries status="use" standardDeviation="0.05" &gt; &lt;!-- status can be "use", "validate" or "ignore" --&gt;<br>
 *   		den_helder_waterlevel_astro.noos<br>
 *   	&lt;/timeSeries&gt;<br>
 *   	&lt;timeSeries status="validate"<br> 
 *                  location="aberdeen" <br>
 *                  position="(-2.045543,57.361939)"<br> 
 *                  source="observed" <br>
 *                  quantity="waterlevel" <br>
 *                  unit="m" <br>
 *                  timezone="GMT"<br> 
 *                  height="0.0" <br>
 *                  standardDeviation="0.05"<br>
 *                  id="waterlevel@aberdeen"<br>
 *                  minDateTime="200901011259"<br>
 *                  maxDateTime="200901021259"<br>
 *                  minValue="-3.0"<br>
 *                  maxValue="3.0" &gt;<br>
 *   		aberdeen_waterlevel_astro.noos<br>
 *   	&lt;/timeSeries&gt;<br>
 *   &lt;/noosObserver&gt;</pre><p>
 *   The properties, like location="aberdeen", can be used to overrule the settings within the data-files.
 *   Valid properties are:<br>
 *   - location<br>
 *   - position<br>
 *   - source<br>
 *   - quantity<br>
 *   - unit<br>
 *   - timezone (not used at the moment; just passed as metadata)<br>
 *   - status (not used at the moment; just passed as metadata)<br>
 *   - height<br>
 *   - standardDeviation : additive errors independent of value<br>
 *   - relativeStandardDeviation : errors proportional to the value<br>
 *   - id (overrules location.quantity)<p>
 *   Selections can be made with<br>
 *   - maxValue<br>
 *   - minValue<br>
 *   - minDateTime<br>
 *   - maxDateTime
 *   
 * @author verlaanm
 *
 */
public class ASCIITimeSeriesObserver extends ASCIITimeSeriesStochObserver {

    /**
	 * Construct ASCIITimeSeriesStochObserver from input file.
	 * @param workingDir Working dir. for stoch observer.
	 * @param arguments Arguments for initialization
	 */
	public void initialize(File workingDir, String[] arguments){
		String fileName = arguments[0];
		File file = new File(workingDir,fileName);
		TimeSeries seriesArray[] = null;
		// read config file
		if(file.exists()){
			ConfigTree config = new ConfigTree(workingDir,fileName);
			ConfigTree seriesTrees[] = config.getSubTrees("timeSeries");
			TimeSeriesFormatter noosFormatter = new ASCIITimeSeriesFormatter();
			seriesArray = new TimeSeries[seriesTrees.length];
			for(int i=0;i<seriesTrees.length;i++){
				// read content
				String seriesFileName = seriesTrees[i].getContentString("");
				//deblank string
				seriesFileName=seriesFileName.trim();
				TimeSeries tempSeries = null;
				File seriesFile = new File(workingDir,seriesFileName);
				if(seriesFile.exists()){
					tempSeries = noosFormatter.readFile(seriesFile.getAbsolutePath());
					// optionally overrule settings
					// priorities are 1) overule 2) file 3) default
					System.out.println("seriesTrees["+i+"]"+seriesTrees[i].toString());
					// <timeSeries status="validate" location="aberdeen" position="(-2.045543,57.361939)" 
					//      source="observed" quantity="waterlevel" unit="m" timezone="GMT" 
					//      height="0.0" standardDeviation="0.05">
					//      aberdeen_waterlevel_astro.noos
					// </timeSeries>
					// status
					String keyword = "status";
					String stringValue = "use";
					stringValue = tempSeries.getStringProperty(keyword,stringValue);
					stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
					stringValue=stringValue.trim();
                    if (!(stringValue.toLowerCase().contentEquals("use")
                            | stringValue.toLowerCase().contentEquals("assimilation")
                            | stringValue.toLowerCase().contentEquals("assimilate")
                            | stringValue.toLowerCase().contentEquals("validation")
                            | stringValue.toLowerCase().contentEquals("validate")
                            | stringValue.toLowerCase().contentEquals("ignore"))){
                        throw new RuntimeException("Field 'status' in not valid. Possible values are use (assimilate), validate, or ignore");
                    }
					System.out.println(""+keyword+"="+stringValue);
                    if (stringValue.toLowerCase().contentEquals("use") | stringValue.toLowerCase().contentEquals("assimilate")) {
                        stringValue = "assimilation";
                        tempSeries.setProperty(keyword,stringValue);
                    } else if ((stringValue.toLowerCase().contentEquals("validate"))) {
                        stringValue = "validation";
                        tempSeries.setProperty(keyword,stringValue);
                    } else {
					    tempSeries.setProperty(keyword,stringValue);
                    }
                    tempSeries.setProperty(keyword,stringValue);
					// relativeStandardDeviation
					keyword = "relativeStandardDeviation";
					double doubleValue = 0.0;
					doubleValue=tempSeries.getDoubleProperty(keyword,doubleValue);
					doubleValue = seriesTrees[i].getAsDouble("@"+keyword, doubleValue);
					//System.out.println(""+keyword+"="+doubleValue);
					tempSeries.setProperty(keyword,""+doubleValue);
					// standardDeviation
					keyword = "standardDeviation";
					doubleValue = 0.05;
					if(tempSeries.getDoubleProperty("relativeStandardDeviation",-1.0)>0.0){
						doubleValue=0.0; //set absolute errors to 0 when relative errors are given
					}
					doubleValue=tempSeries.getDoubleProperty(keyword,doubleValue);
					doubleValue = seriesTrees[i].getAsDouble("@"+keyword, doubleValue);
					//System.out.println(""+keyword+"="+doubleValue);
					tempSeries.setProperty(keyword,""+doubleValue);
					// quantity
					keyword = "quantity";
					stringValue = "";
					stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
					if(stringValue.equals("")){
						stringValue = tempSeries.getQuantityId();
					}
					stringValue=stringValue.trim();
					//System.out.println(""+keyword+"="+stringValue);
					tempSeries.setQuantity(stringValue);
					// unit
					keyword = "unit";
					stringValue = "";
					stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
					if(stringValue.equals("")){
						stringValue = tempSeries.getUnitId();
					}
					stringValue=stringValue.trim();
					//System.out.println(""+keyword+"="+stringValue);
					tempSeries.setUnit(stringValue);
					// source
					keyword = "source";
					stringValue = "";
					stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
					if(stringValue.equals("")){
						stringValue = tempSeries.getSource();
					}
					stringValue=stringValue.trim();
					//System.out.println(""+keyword+"="+stringValue);
					tempSeries.setSource(stringValue);
					// timezone
					keyword = "timezone";
					stringValue = "";
					stringValue = tempSeries.getStringProperty(keyword,stringValue);
					stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
					stringValue=stringValue.trim();
					//System.out.println(""+keyword+"="+stringValue);
					tempSeries.setProperty(keyword,stringValue);
					// height
					keyword = "height";
					doubleValue = Double.NaN;
					doubleValue = seriesTrees[i].getAsDouble("@"+keyword, doubleValue);
					if(Double.isNaN(doubleValue)){
						doubleValue=tempSeries.getHeight();
					}
					//System.out.println(""+keyword+"="+doubleValue);
					tempSeries.setHeight(doubleValue);
					// location
					keyword = "location";
					stringValue ="";
					stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
					if(stringValue.equals("")){
						stringValue = tempSeries.getLocation();
					}
					stringValue=stringValue.trim();
					//System.out.println(""+keyword+"="+stringValue);
					tempSeries.setLocation(stringValue);
					// position
					keyword = "position";
					stringValue = "";
					stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
					double x=0.0;
					double y=0.0;
					if(stringValue.equals("")){
						double[] pos = tempSeries.getPosition();
						x=pos[0];
						y=pos[1];
					}else{
						try {
							stringValue=stringValue.trim();
							String partString = stringValue.substring(1,stringValue.length()-2);
							String parts[] = partString.split(",");
							x = Double.parseDouble(parts[0]);
							y = Double.parseDouble(parts[1]);
						} catch (Exception e) {
							throw new RuntimeException("Problem parsing position '"+stringValue+"' "+e.getMessage());
						}
					}
					//System.out.println(""+keyword+"=("+x+","+y+")");
					tempSeries.setPosition(x, y);
					// id
					keyword = "id";
					stringValue ="";
					stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
					if(stringValue.equals("")){
						stringValue = tempSeries.getLocation()+"."+tempSeries.getQuantityId();
					}
					stringValue=stringValue.trim();
					//System.out.println(""+keyword+"="+stringValue);
					tempSeries.setId(stringValue);
					// minValue
					keyword = "minValue";
					double minValue = Double.NaN;
					minValue = seriesTrees[i].getAsDouble("@"+keyword, minValue);
					//System.out.println(""+keyword+"="+minValue);
					// maxValue
					keyword = "maxValue";
					double maxValue = Double.NaN;
					maxValue = seriesTrees[i].getAsDouble("@"+keyword, maxValue);
					//System.out.println(""+keyword+"="+maxValue);
					// minDateTime
					keyword = "minDateTime";
					double minDateTime=Double.NaN;
					stringValue ="";
					stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
					stringValue = stringValue.trim();
					//System.out.println(""+keyword+"="+stringValue);
					if(!stringValue.equals("")){
						try {
							minDateTime = org.openda.exchange.timeseries.TimeUtils.date2Mjd(stringValue);
						} catch (ParseException e) {
							e.printStackTrace();
							throw new RuntimeException("Illegal date format for minDateTime "
									+"(expected something like 201012312359) found "+stringValue );
						}
					}
					// maxDateTime
					keyword = "maxDateTime";
					double maxDateTime=Double.NaN;
					stringValue ="";
					stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
					stringValue = stringValue.trim();
					//System.out.println(""+keyword+"="+stringValue);
					if(!stringValue.equals("")){
						try {
							maxDateTime = org.openda.exchange.timeseries.TimeUtils.date2Mjd(stringValue);
						} catch (ParseException e) {
							e.printStackTrace();
							throw new RuntimeException("Illegal date format for maxDateTime "
									+"(expected something like 201012312359) found "+stringValue );
						}
					}
					if(!(Double.isNaN(minDateTime)&&Double.isNaN(maxDateTime))){
						tempSeries = tempSeries.selectTimeSubset(minDateTime, maxDateTime);
					}
					if(!(Double.isNaN(minValue)&&Double.isNaN(maxValue))){
						tempSeries = tempSeries.selectValueSubset(minValue, maxValue);
					}
					Results.putMessage("NoosStochObserver[i]="+tempSeries.toString());
					if(tempSeries.getTimesRef()==null){
						Results.putMessage("WARNING : Empty selection for this series.");
					}
				}else{
					throw new RuntimeException("Could not find file "+seriesFile.getAbsolutePath());
				}
				seriesArray[i] = tempSeries; 
			}
		}else{
            throw new RuntimeException("Could not find file "+file.getAbsolutePath());
		}
		// everything is there, now fill the fields
		setSeries(seriesArray);
	}

	public IStochObserver createSelection(Type observationType) {
        List<TimeSeries> selectedSeries = new ArrayList<TimeSeries>();
        for (TimeSeries serie : series) {
            String status = serie.getProperty("status");
            if (status != null) {
                if (observationType.toString().toLowerCase().contentEquals(status.toLowerCase())) {
                    selectedSeries.add(serie);
                }
            }
        }

        TimeSeries arrSelectedSeries[] = selectedSeries.toArray(new TimeSeries[selectedSeries.size()]);
        return new TimeSeriesStochObserver(arrSelectedSeries);
	}

    public ISelector createSelector(Type observationType) {
        IndicesSelector indSelector = new IndicesSelector();
        int indLast=0;
        int indFirst;
        for (TimeSeries serie : series) {
            String status = serie.getProperty("status");
            indFirst = indLast;
            indLast += serie.getSize();
            if (status != null) {
                if (observationType.toString().toLowerCase().contentEquals(status.toLowerCase())) {
                    indSelector.addIndexRange(indFirst, indLast);
                }
            }
        }
        return indSelector;
    }

//    public IStochObserver createSelection(double minVal, double maxVal) {
//        HashMap<String,Integer> obsStatusLocation = new HashMap<String,Integer>();
//        TimeSeries[] selectedSeries = series.clone();
//        int nSeries = selectedSeries.length;
//        List<Integer> assimLoopStep = new ArrayList<Integer>();
//        int j=0;
//        for (int i=0; i<nSeries; i++){
//            TimeSeries serie = selectedSeries[i];
//            String obsStatus = serie.getProperty("status");
//            String obsLocation = serie.getLocation();
//            obsStatusLocation.put(obsStatus+obsLocation,i);
//            if (obsStatus.toLowerCase().contains(Type.Validation.toString().toLowerCase())){
//                selectedSeries[i] = serie.selectValueSubset(minVal,maxVal);
//            } else {
//                assimLoopStep.add(j,i);
//                j++;
//            }
//        }
//        String obsStatus = Type.Validation.toString().toLowerCase();
//        for (int i=0; i<j; i++){
//            TimeSeries serie = selectedSeries[assimLoopStep.get(i)];
//            String obsLocation = serie.getLocation();
//            int iOrder = obsStatusLocation.get(obsStatus+obsLocation);
//            TimeSeries serieValidation = selectedSeries[iOrder];
//            double[] timeThisObs = serieValidation.getTimes();
//            selectedSeries[assimLoopStep.get(i)] = serie.selectTimeSubset(timeThisObs);
//        }
//        return new TimeSeriesStochObserver(selectedSeries);
//    }
//
//    private boolean[] getMaskSelectedIndices(TimeSeries serie, double minVal, double maxVal) {
//        boolean[] selectedIndices = new boolean[serie.getSize()];
//        double[] values = serie.getValuesAsDoubles();
//        int nValues = values.length;
//        for (int i=0; i<nValues; i++){
//            selectedIndices[i] = values[i] >= minVal && values[i] <= maxVal;
//        }
//        return selectedIndices;
//    }
//
//    public ISelector createSelector(double minVal, double maxVal) {
//        DischargeDependentSelector indSelector = new DischargeDependentSelector();
//        TimeSeries[] selectedSeries = series.clone();
//        HashMap<String,boolean[]> obsSelectedElements = new HashMap<String,boolean[]>();
//        int nSeries = selectedSeries.length;
//        for (int i=0; i<nSeries; i++){
//            TimeSeries serie = selectedSeries[i];
//            String obsStatus = serie.getProperty("status");
//            String obsLocation = serie.getLocation();
//            if (obsStatus.toLowerCase().contains(Type.Validation.toString().toLowerCase())){
//                boolean[] maskSelectedIndices = getMaskSelectedIndices(serie,minVal,maxVal);
//                obsSelectedElements.put(obsStatus+obsLocation,maskSelectedIndices);
//                obsSelectedElements.put(Type.Assimilation.toString().toLowerCase()+obsLocation,maskSelectedIndices);
//            }
//        }
//
//        boolean[] outMask = new boolean[this.getCount()];
//        int nextindex = 0;
//        for (TimeSeries serie : series) {
//            String obsStatus = serie.getProperty("status");
//            String obsLocation = serie.getLocation();
//            boolean[] thisSerieMask = obsSelectedElements.get(obsStatus+obsLocation);
//            int seriesLength = serie.getSize();
//            seriesLength = serie.getSize();
//            System.arraycopy(thisSerieMask, 0, outMask, nextindex, seriesLength);
//            nextindex += seriesLength;
//        }
//        indSelector.setMask(outMask);
//        return indSelector;
//    }
}
