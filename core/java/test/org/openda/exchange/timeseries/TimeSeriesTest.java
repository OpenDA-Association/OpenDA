/* MOD_V2.0
 * Copyright (c) 2012 OpenDA Association
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
package org.openda.exchange.timeseries;

import junit.framework.TestCase;

import org.openda.interfaces.IArray;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Mask;

import java.lang.reflect.Type;
import java.util.Arrays;

public class TimeSeriesTest extends TestCase {

   public static void testTimeSeries() {
      System.out.println("==============================================================================");
      System.out.println("test TimeSeries");
      System.out.println("==============================================================================");
      double delta = 0.00001;

      // public TimeSeries(double times[], double values[]){
      double times[] = { 0.0, 1.0, 2.0, 3.0 };
      double values[] = { 0.0, 0.1, 0.2, 0.3 };
      TimeSeries ts1 = new TimeSeries(times, values);
      // public double[] getTimes(){
      double times2[] = ts1.getTimes();
      System.out.println("times2[1] =" + times2[1]);
      System.out.println("Should be times2[1] = 1.0");
      assertEquals(1.0, times2[1], delta);
      // public double[] getValues(){
      double values2[] = ts1.getValuesAsDoubles();
      System.out.println("values2[1] =" + values2[1]);
      System.out.println("Should be values2[1] = 0.1");
      assertEquals(0.1, values2[1], delta);

      // public TimeSeries(double times[], double values[],double x, double y,
      double x = 1.0;
      double y = 2.0;
      String source = "source";
      String quantity = "quantity";
      String unit = "unit";
      String location = "location";
      TimeSeries ts2 = new TimeSeries(times, values, x, y, source, quantity, unit, location);
      // public TimeSeries(double times[], double values[]){
      // public double[] getTimes(){
      double times3[] = ts2.getTimes();
      System.out.println("times3[1] =" + times3[1]);
      System.out.println("Should be times3[1] = 1.0");
      assertEquals(1.0, times3[1], delta);
      // public double[] getValues(){
      double values3[] = ts2.getValuesAsDoubles();
      System.out.println("values3[1] =" + values3[1]);
      System.out.println("Should be values3[1] = 0.1");
      assertEquals(0.1, values3[1], delta);
      // public String getLocation(){
      String location2 = ts2.getLocation();
      System.out.println("location2 =" + location2);
      System.out.println("Should be location2 = location");
      assertEquals("location", location2);
      // public void setLocation(String location){
      ts2.setLocation("changed");
      String location3 = ts2.getLocation();
      System.out.println("location3 =" + location3);
      System.out.println("Should be location3 = changed");
      assertEquals("changed", location3);

      // public String getSource(){
      String source2 = ts2.getSource();
      System.out.println("source2 =" + source2);
      System.out.println("Should be source2 = source");
      assertEquals("source", source2);
      // public void setSource(String source){
      ts2.setSource("changed");
      String source3 = ts2.getSource();
      System.out.println("source3 =" + source3);
      System.out.println("Should be source3 = changed");
      assertEquals("changed", source3);

      // public String getQuantity(){
      String quantity2 = ts2.getQuantityId();
      System.out.println("quantity2 =" + quantity2);
      System.out.println("Should be quantity2 = quantity");
      assertEquals("quantity", quantity2);
      // public void setQuantity(String quantity){
      ts2.setQuantity("changed");
      String quantity3 = ts2.getQuantityId();
      System.out.println("quantity3 =" + quantity3);
      System.out.println("Should be quantity3 = changed");
      assertEquals("changed", quantity3);

      // public String getUnit(){
      String unit2 = ts2.getUnitId();
      System.out.println("unit2 =" + unit2);
      System.out.println("Should be unit2 = unit");
      assertEquals("unit", unit2);
      // public void setUnit(String unit){
      ts2.setUnit("changed");
      String unit3 = ts2.getUnitId();
      System.out.println("unit3 =" + unit3);
      System.out.println("Should be unit3 = changed");
      assertEquals("changed", unit3);

      // public double[] getPosition(){
      double[] pos = ts2.getPosition();
      System.out.println("pos =(" + pos[0] + "," + pos[1] + ")");
      System.out.println("Should be pos = (1.0,2.0)");
      assertEquals(1.0, pos[0], delta);
      assertEquals(2.0, pos[1], delta);
      // public void setPosition(double x, double y){
      ts2.setPosition(1.1, 2.2);
      double[] pos2 = ts2.getPosition();
      System.out.println("pos2 =(" + pos2[0] + "," + pos2[1] + ")");
      System.out.println("Should be pos2 = (1.1,2.2)");
      assertEquals(1.1, pos2[0], delta);
      assertEquals(2.2, pos2[1], delta);

      // public double getHeight(){
      double height = ts2.getHeight();
      System.out.println("height =" + height);
      System.out.println("Should be height = 0.0");
      assertEquals(0.0, height, delta);
      // public void setHeight(double height){
      ts2.setHeight(0.1);
      double height2 = ts2.getHeight();
      System.out.println("height2 =" + height2);
      System.out.println("Should be height2 = 0.1");
      assertEquals(0.1, height2, delta);

      // public void setData(double[] times,double[] values){
      double times4[] = { 10.0, 11.0, 12.0, 13.0 };
      double values4[] = { 10.0, 10.1, 10.2, 10.3 };
      ts2.setData(times4, values4);
      // public double[] getTimesRef(){
      double times5[] = ts2.getTimesRef();
      // public double[] getValuesRef(){
      double values5[] = ts2.getValuesRef();
      System.out.println("times5[1] =" + times5[1]);
      System.out.println("Should be times5[1] = 11.0");
      assertEquals(11.0, times5[1], delta);
      System.out.println("values5[1] =" + values5[1]);
      System.out.println("Should be values5[1] = 10.1");
      assertEquals(10.1, values5[1], delta);

      // public String getProperty(String propertyName){
      // public void setProperty(String propertyName, String propertyValue){
      ts2.setProperty("referencePlane", "WGS84");
      String property = ts2.getProperty("referencePlane");
      System.out.println("property =" + property);
      System.out.println("Should be property = WGS84");
      assertEquals("WGS84", property);

      // get non existing prop
      String property2 = ts2.getProperty("doesNotExist");
      System.out.println("property2 =" + property2);
      System.out.println("Should be property2 = null");
      assertEquals(null, property2);

      // check for a property
      boolean propIsThere = ts2.hasProperty("referencePlane");
      boolean propIsNotThere = ts2.hasProperty("doesNotExist");
      assertEquals(true, propIsThere);
      assertEquals(false, propIsNotThere);

      // get property with a default if it is not there
      String refPlane = ts2.getStringProperty("referencePlane", "Rijksdriehoek");
      assertEquals("WGS84", refPlane);
      String refPlane2 = ts2.getStringProperty("NoReferencePlane", "Rijksdriehoek");
      assertEquals("Rijksdriehoek", refPlane2);

      ts2.setProperty("timestep", "" + 0.005);
      double timestep = ts2.getDoubleProperty("timestep", 0.1);
      assertEquals(0.005, timestep, 0.000005);
      double timestep2 = ts2.getDoubleProperty("NoTimestep", 0.1);
      assertEquals(0.1, timestep2, 0.000005);

   }

   public static void testTimeSeriesSelection() {
      System.out.println("==============================================================================");
      System.out.println("test selection on TimeSeries");
      System.out.println("==============================================================================");
      double delta = 0.00001;

      // public TimeSeries(double times[], double values[]){
      double times[] = { 0.0, 1.0, 2.0, 3.0 };
      double values[] = { 0.0, 0.1, 0.2, 0.3 };

      TimeSeries ts1 = new TimeSeries(times, values);
      ts1.setLocation("location");
      // public double[] getTimes(){
      TimeSeries ts2 = ts1.selectTimeSubset(0.5, 2.5);
      double times2[] = ts2.getTimesRef();
      double values2[] = ts2.getValuesRef();
      System.out.println("times2.length =" + times2.length);
      System.out.println("Should be times2.length = 2");
      assertEquals(2, times2.length);
      System.out.println("times2[0] =" + times2[0]);
      System.out.println("Should be times2[0] = 1.0");
      assertEquals(1.0, times2[0], delta);
      String loc2 = ts2.getLocation();
      assertEquals("location", loc2);
      System.out.println("values2[0] =" + values2[0]);
      System.out.println("Should be values2[0] = 0.1");
      assertEquals(0.1, values2[0], delta);

      assertEquals(0.0, ts1.minimum());
      assertEquals(0.3, ts1.maximum());
      assertEquals(0.15, ts1.average(), 0.01);
      assertEquals(0.15, ts1.averageAbs(), 0.01);
      assertEquals(Math.sqrt(7. / 200.), ts1.rms());
      assertEquals(Math.sqrt(1. / 60.), ts1.stdDev(0.15));

      TimeSeries tserr = ts1.errorSeries(ts2);
      assertEquals(2, tserr.getSize());
      assertEquals(0.0, tserr.getValue(1.0));
      assertEquals(0.0, tserr.getValue(2.0));

      assertEquals(0.15, ts1.average(), 0.01);
      assertEquals(0.15, ts1.averageAbs(), 0.01);
      assertEquals(Math.sqrt(0.035), ts1.rms(), 0.01);
      assertEquals(Math.sqrt(1. / 60.), ts1.stdDev(0.15), 0.01);

      assertEquals(0., tserr.average());
      assertEquals(0., tserr.averageAbs());
      assertEquals(0., tserr.rms());
      assertEquals(0., tserr.stdDev(0.0));

      // times[] = {0.0, 1.0, 2.0, 3.0};
      TimeSeries ts3 = ts1.selectTimeSubset(0.5, 4.0);
      double times3[] = ts3.getTimesRef();
      double values3[] = ts3.getValuesRef();
      System.out.println("times3.length =" + times3.length);
      System.out.println("Should be times3.length = 3");
      assertEquals(3, times3.length);
      System.out.println("times3[0] =" + times3[0]);
      System.out.println("Should be times3[0] = 1.0");
      assertEquals(1.0, times3[0], delta);
      String loc3 = ts3.getLocation();
      assertEquals("location", loc3);
      System.out.println("values3[0] =" + values3[0]);
      System.out.println("Should be values3[0] = 0.1");
      assertEquals(0.1, values3[0], delta);

      // times[] = {0.0, 1.0, 2.0, 3.0};
      TimeSeries ts4 = ts1.selectTimeSubset(-0.5, 2.5);
      double times4[] = ts4.getTimesRef();
      double values4[] = ts4.getValuesRef();
      System.out.println("times4.length =" + times4.length);
      System.out.println("Should be times4.length = 4");
      assertEquals(3, times4.length);
      System.out.println("times4[0] =" + times4[0]);
      System.out.println("Should be times4[0] = 0.0");
      assertEquals(0.0, times4[0], delta);
      String loc4 = ts4.getLocation();
      assertEquals("location", loc4);
      System.out.println("values4[0] =" + values4[0]);
      System.out.println("Should be values4[0] = 0.0");
      assertEquals(0.0, values4[0], delta);

      // times[] = {0.0, 1.0, 2.0, 3.0}; Empty selection
      TimeSeries ts5 = ts1.selectTimeSubset(3.5, 5.5);
      double times5[] = ts5.getTimesRef();
      double values5[] = ts5.getValuesRef();
      assertTrue(times5 == null);
      assertTrue(values5 == null);
   }

   public static void testTimeSeriesMaskSelection() {
      System.out.println("==============================================================================");
      System.out.println("test mask selection on TimeSeries");
      System.out.println("==============================================================================");
      double delta = 0.00001;

      // public TimeSeries(double times[], double values[]){
      double times[] = { 0.0, 1.0, 2.0, 3.0 };
      double values[] = { 0.0, 0.1, 0.2, 0.3 };
      TimeSeries ts1 = new TimeSeries(times, values);
      ts1.setLocation("location");
      // public double[] getTimes(){

      TimeSeries ts2 = ts1.selectMaskSubset(Mask.elementBetween(ts1.getTimesRef(), 0.5, 2.5));
      TimeSeries ts2a = ts1.selectTimeSubset(0.5, 2.5);
      TimeSeries ts2b = ts1.selectMaskSubset(Mask.elementGE(ts1.getTimesRef(), 0.5).and(Mask.elementLE(ts1.getTimesRef(), 2.5)));
      TimeSeries ts2c = ts1.selectMaskSubset(Mask.elementBetween(ts1.getValuesRef(), 0.1, 0.2));
      TimeSeries ts2d = ts1.selectMaskSubset(Mask.elementGE(ts1.getValuesRef(), 0.001).and(Mask.elementLE(ts1.getValuesRef(), 0.2999)));
      TimeSeries ts2e = ts1.selectMaskSubset(Mask.elementLT(ts1.getValuesRef(), 0.001).not().and(Mask.elementGT(ts1.getValuesRef(), 0.2999).not()));
      assertTrue(ts2.equals(ts2a));
      assertTrue(ts2.equals(ts2b));
      assertTrue(ts2.equals(ts2c));
      assertTrue(ts2.equals(ts2d));
      assertTrue(ts2.equals(ts2e));

      double times2[] = ts2.getTimesRef();
      double values2[] = ts2.getValuesRef();
      System.out.println("times2.length =" + times2.length);
      System.out.println("Should be times2.length = 2");
      assertEquals(2, times2.length);
      System.out.println("times2[0] =" + times2[0]);
      System.out.println("Should be times2[0] = 1.0");
      assertEquals(1.0, times2[0], delta);
      String loc2 = ts2.getLocation();
      assertEquals("location", loc2);
      System.out.println("values2[0] =" + values2[0]);
      System.out.println("Should be values2[0] = 0.1");
      assertEquals(0.1, values2[0], delta);

      assertEquals(0.0, ts1.minimum());
      assertEquals(0.3, ts1.maximum());
      assertEquals(0.15, ts1.average(), 0.01);
      assertEquals(0.15, ts1.averageAbs(), 0.01);
      assertEquals(Math.sqrt(7. / 200.), ts1.rms());
      assertEquals(Math.sqrt(1. / 60.), ts1.stdDev(0.15));

      TimeSeries tserr = ts1.errorSeries(ts2);
      assertEquals(2, tserr.getSize());
      assertEquals(0.0, tserr.getValue(1.0));
      assertEquals(0.0, tserr.getValue(2.0));

      assertEquals(0.15, ts1.average(), 0.01);
      assertEquals(0.15, ts1.averageAbs(), 0.01);
      assertEquals(Math.sqrt(0.035), ts1.rms(), 0.01);
      assertEquals(Math.sqrt(1. / 60.), ts1.stdDev(0.15), 0.01);

      assertEquals(0., tserr.average());
      assertEquals(0., tserr.averageAbs());
      assertEquals(0., tserr.rms());
      assertEquals(0., tserr.stdDev(0.0));

      // times[] = {0.0, 1.0, 2.0, 3.0};
      TimeSeries ts3 = ts1.selectMaskSubset(Mask.elementBetween(ts1.getTimesRef(), 0.5, 4.0));
      TimeSeries ts3a = ts1.selectTimeSubset(0.5, 4.0);
      TimeSeries ts3b = ts1.selectMaskSubset(Mask.elementBetween(ts1.getTimesRef(), 1.5, 4.0).or(Mask.elementBetween(ts1.getValuesRef(), 0.05, 0.2)));
      assertTrue(ts3.equals(ts3a));
      assertTrue(ts3.equals(ts3b));

      double times3[] = ts3.getTimesRef();
      double values3[] = ts3.getValuesRef();
      System.out.println("times3.length =" + times3.length);
      System.out.println("Should be times3.length = 3");
      assertEquals(3, times3.length);
      System.out.println("times3[0] =" + times3[0]);
      System.out.println("Should be times3[0] = 1.0");
      assertEquals(1.0, times3[0], delta);
      String loc3 = ts3.getLocation();
      assertEquals("location", loc3);
      System.out.println("values3[0] =" + values3[0]);
      System.out.println("Should be values3[0] = 0.1");
      assertEquals(0.1, values3[0], delta);

      // times[] = {0.0, 1.0, 2.0, 3.0};
      TimeSeries ts4 = ts1.selectMaskSubset(Mask.elementBetween(ts1.getTimesRef(), -0.5, 2.5));
      TimeSeries ts4a = ts1.selectTimeSubset(-0.5, 2.5);
      assertTrue(ts4.equals(ts4a));

      double times4[] = ts4.getTimesRef();
      double values4[] = ts4.getValuesRef();
      System.out.println("times4.length =" + times4.length);
      System.out.println("Should be times4.length = 4");
      assertEquals(3, times4.length);
      System.out.println("times4[0] =" + times4[0]);
      System.out.println("Should be times4[0] = 0.0");
      assertEquals(0.0, times4[0], delta);
      String loc4 = ts4.getLocation();
      assertEquals("location", loc4);
      System.out.println("values4[0] =" + values4[0]);
      System.out.println("Should be values4[0] = 0.0");
      assertEquals(0.0, values4[0], delta);

      // times[] = {0.0, 1.0, 2.0, 3.0}; Empty selection
      TimeSeries ts5 = ts1.selectMaskSubset(Mask.elementBetween(ts1.getTimesRef(), 3.5, 5.5));
      TimeSeries ts5a = ts1.selectTimeSubset(3.5, 5.5);
      assertTrue(ts5.equals(ts5a));

      double times5[] = ts5.getTimesRef();
      double values5[] = ts5.getValuesRef();
      assertTrue(times5 == null);
      assertTrue(values5 == null);
   }

   public static void testTimeSeries2() {
      System.out.println("==============================================================================");
      System.out.println("test TimeSeries as an implementation of IExchange");
      System.out.println("==============================================================================");
      // public TimeSeries(double times[], double values[]){
      double times[] = { 0.0, 1.0, 2.0, 3.0 };
      double values[] = { 0.0, 0.1, 0.2, 0.3 };
      double x = 1.0;
      double y = 2.0;
      String source = "source";
      String quantity = "quantity";
      String unit = "unit";
      String location = "location";
      IExchangeItem ex = new TimeSeries(times, values, x, y, source, quantity, unit, location);

      // String getId();
      String id = ex.getId();
      System.out.println("ex.getId() =" + id);
      System.out.println("Should be ex.getId() = location.quantity");
      assertEquals("ex.getId()", "location.quantity", id);

      // String getQuantityId();
      String quantityId = ((TimeSeries) ex).getQuantityId();
      System.out.println("ex.getQuantityId() =" + quantityId);
      System.out.println("Should be ex.getQuantityId() = quantity");
      assertEquals("ex.getQuantityId()", "quantity", quantityId);

      // String getUnitId();
      String unitId = ((TimeSeries) ex).getUnitId();
      System.out.println("ex.getUnitId() =" + unitId);
      System.out.println("Should be ex.getUnitId() = unit");
      assertEquals("ex.getUnitId()", "unit", unitId);

      // public Type getObjectType(); //
      Type valueType = ex.getValueType();
      assertTrue(valueType == org.openda.utils.Array.class);

      // public Object times;
      TimeSeries seriesRef = (TimeSeries) ex;
      double[] timesRef = seriesRef.getTimesRef();
      System.out.println("times[1] =" + timesRef[1]);
      System.out.println("Should be times[1] = 1.0");
      assertEquals("times[1]", 1.0, timesRef[1], 0.0001);

      // public double[] getValuesAsDoubles();
      double[] valuesCopy = ex.getValuesAsDoubles();
      System.out.println("values[1] =" + valuesCopy[1]);
      System.out.println("Should be values[1] = 0.1");
      assertEquals("values[1]", 0.1, valuesCopy[1], 0.0001);

      // public void setValues(Object values);
      double newValues[] = { 2.0, 2.1, 2.2, 2.3 };
      ex.setValuesAsDoubles(newValues);
      double[] valuesCopy2 = ex.getValuesAsDoubles();
      System.out.println("values[1] =" + valuesCopy2[1]);
      System.out.println("Should be values[1] = 2.1");
      assertEquals("values[1]", 2.1, valuesCopy2[1], 0.0001);
      
      // public Object getValues();
      IArray valArray = (IArray) ex.getValues();
      System.out.println("values =" + valArray);
      double[] valuesFromArray = valArray.getValuesAsDoubles();
      System.out.println("values.length =" + valuesFromArray.length);
      System.out.println("Should be values.length = 4");
      assertEquals("values.length", 4, valuesFromArray.length);
      System.out.println("values[0] =" + valuesFromArray[0]);
      System.out.println("Should be values[0] = 2.0");
      assertEquals("values[0]", 2.0, valuesFromArray[0], 0.0001);
      System.out.println("values[1] =" + valuesFromArray[1]);
      System.out.println("Should be values[1] = 2.1");
      assertEquals("values[1]", 2.1, valuesFromArray[1], 0.0001);

   }

   public static void testTimeSeriesSelection_2() {
      System.out.println("==============================================================================");
      System.out.println("test time selection on TimeSeries");
      System.out.println("==============================================================================");
      double delta = 0.01;

      // public TimeSeries(double times[], double values[]){
      double times[] = { 0.0, 1.0, 2.0, 3.0, 4.0 };
      double values[] = { 0.0, 0.1, 0.2, 0.3, 0.4 };
      TimeSeries ts1 = new TimeSeries(times, values);
      ts1.setLocation("location");
      double times2[] = { 1.0, 1.999, 2.001, 3.0, 7.0 };
      // 1.999,2.001 are within tolerance and 7.0 does not exist
      // now get selection
      double[] values2 = ts1.getValuesAsDoubles(times2, delta, 999.00);
      System.out.println("values2 =" + Arrays.toString(values2));
      System.out.println("Should be values2 = [0.1, 0.2, 0.2, 0.3, 999.0]");
      assertEquals("values2", "[0.1, 0.2, 0.2, 0.3, 999.0]", Arrays.toString(values2));
   }

   public static void testTimeSeriesSelection_3() {
      double times[] = { 0.0, 1.0, 2.0, 3.0, 4.0 };
      double values[] = { 0.0, 0.1, 0.2, 0.3, 0.4 };
      TimeSeries ts1 = new TimeSeries(times, values);
      double times2[] = { 1.0, 1.999, 2.001, 3.0, 7.0 };
      TimeSeries ts2 = ts1.selectTimeSubset(times2);
      assertEquals(2, ts2.getSize());
      assertEquals(1.0, ts2.getTimesRef()[0], 0.01);
      assertEquals(3.0, ts2.getTimesRef()[1], 0.01);
      assertEquals(0.1, ts2.getValuesRef()[0], 0.01);
      assertEquals(0.3, ts2.getValuesRef()[1], 0.01);
      double times3[] = { -1.0, 1.999, 2.001, 7.0, 7.0 };
      TimeSeries ts3 = ts1.selectTimeSubset(times3);
      assertEquals(0, ts3.getSize());
   }

   public static void testTimeSeriesId() {
      System.out.println("==============================================================================");
      System.out.println("test default and overrule for id");
      System.out.println("==============================================================================");

      // public TimeSeries(double times[], double values[]){
      double times[] = { 0.0, 1.0, 2.0, 3.0, 4.0 };
      double values[] = { 0.0, 0.1, 0.2, 0.3, 0.4 };
      TimeSeries ts1 = new TimeSeries(times, values);
      ts1.setLocation("valve1");
      ts1.setQuantity("pressure");
      String id = ts1.getId();
      assertEquals("default id", "valve1.pressure", id);
      // now overrule
      ts1.setId("pressure@valve1");
      id = ts1.getId();
      assertEquals("default id", "pressure@valve1", id);
   }

   public static void testTimeSeriesValueSelection() {
      System.out.println("==============================================================================");
      System.out.println("test excluding values outside the range");
      System.out.println("==============================================================================");

      // public TimeSeries(double times[], double values[]){
      double times[] = { 0.0, 1.0, 2.0, 3.0, 4.0 };
      double values[] = { 0.0, 0.1, 0.2, 0.3, 0.4 };
      TimeSeries ts1 = new TimeSeries(times, values);
      ts1.setLocation("valve1");
      ts1.setQuantity("pressure");

      TimeSeries ts2 = ts1.selectValueSubset(0.05, 0.35);
      double[] values2 = ts2.getValuesAsDoubles();
      System.out.println("values2 =" + Arrays.toString(values2));
      System.out.println("Should be values2 = [0.1, 0.2, 0.3]");
      assertEquals("values2", "[0.1, 0.2, 0.3]", Arrays.toString(values2));

      TimeSeries ts3 = ts1.selectValueSubset(Double.NaN, 0.35);
      double[] values3 = ts3.getValuesAsDoubles();
      System.out.println("values3 =" + Arrays.toString(values3));
      System.out.println("Should be values3 = [0.0, 0.1, 0.2, 0.3]");
      assertEquals("values3", "[0.0, 0.1, 0.2, 0.3]", Arrays.toString(values3));

      TimeSeries ts4 = ts1.selectValueSubset(0.05, Double.NaN);
      double[] values4 = ts4.getValuesAsDoubles();
      System.out.println("values4 =" + Arrays.toString(values4));
      System.out.println("Should be values4 = [0.1, 0.2, 0.3, 0.4]");
      assertEquals("values4", "[0.1, 0.2, 0.3, 0.4]", Arrays.toString(values4));
   }

   public static void testTimeSeriesToString() {
      System.out.println("==============================================================================");
      System.out.println("test TimeSeries.toString");
      System.out.println("==============================================================================");

      // public TimeSeries(double times[], double values[]){
      double times1[] = { 0.0, 1.0, 2.0, 3.0 };
      double values1[] = { 0.0, 0.1, 0.2, 0.3 };
      TimeSeries ts1 = new TimeSeries(times1, values1);
      String ts1String = ts1.toString();
      assertEquals(-297300475, ts1String.hashCode());

      // public TimeSeries(double times[], double values[]){
      double times2[] = { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0 };
      double values2[] = { 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7 };
      TimeSeries ts2 = new TimeSeries(times2, values2);
      String ts2String = ts2.toString();
      System.out.println("ts2=" + ts2String);
      assertEquals(-925868425, ts2String.hashCode());

      // public TimeSeries(double times[], double values[]){
      double times3[] = { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0 };
      double values3[] = { 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3 };
      TimeSeries ts3 = new TimeSeries(times3, values3);
      String ts3String = ts3.toString();
      System.out.println("ts3=" + ts3String);
      assertEquals(-591158452, ts3String.hashCode());

   }

}
