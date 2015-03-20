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

import java.util.Set;

import junit.framework.TestCase;

/**
 * @author johan
 *
 */
public class TimeSeriesSelectionTest extends TestCase {
   private TimeSeriesSelection tss;

   /**
    * @see junit.framework.TestCase#setUp()
    */
   @Override
   protected void setUp() throws Exception {
      super.setUp();
      this.tss = new TimeSeriesSelection();
   }

   /**
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#addLocation(java.lang.String)}.
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#getLocations()}.
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#isSelectedLocation(java.lang.String)}.
    */
   public final void testLocation() {
      this.tss.addLocation("wick");
      this.tss.addLocation("hoekvanholland");
      this.tss.addLocation("ijmuiden");

      final Set<String> locations = this.tss.getLocations();
      assertEquals(3, locations.size());
      assertTrue(locations.contains("ijmuiden"));
      assertTrue(locations.contains("wick"));
      assertTrue(locations.contains("hoekvanholland"));
      assertFalse(locations.contains("madrid"));

      assertTrue(this.tss.isSelectedLocation("ijmuiden"));
      assertTrue(this.tss.isSelectedLocation("wick"));
      assertTrue(this.tss.isSelectedLocation("hoekvanholland"));
      assertFalse(this.tss.isSelectedLocation("mira"));
   }

   /**
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#addSource(java.lang.String)}.
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#getSources()}.
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#isSelectedSource(java.lang.String)}.
    */
   public final void testSource() {
      this.tss.addSource("hmcn_csm8");

      final Set<String> sources = this.tss.getSources();
      assertEquals(1, sources.size());
      assertTrue(sources.contains("hmcn_csm8"));
      assertFalse(sources.contains("hmcn_oper"));

      assertTrue(this.tss.isSelectedSource("hmcn_csm8"));
      assertFalse(this.tss.isSelectedSource("hmcn_oper"));
   }

   /**
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#addQuantity(java.lang.String)}.
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#getQuantities()}.
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#isSelectedQuantity(java.lang.String)}.
    */
   public final void testQuantity() {
      this.tss.addQuantity("waterlevel");

      final Set<String> quantities = this.tss.getQuantities();
      assertEquals(1, quantities.size());
      assertTrue(quantities.contains("waterlevel"));
      assertFalse(quantities.contains("wind_speed"));

      assertTrue(this.tss.isSelectedQuantity("waterlevel"));
      assertFalse(this.tss.isSelectedQuantity("wind_speed"));
}

   /**
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#setStartTime(double)}.
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#setStartTime(java.lang.String)}.
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#getStartTime()}.
    */
   public final void testStartTime() {
      this.tss.setStartTime("200911200000");

      final double tstart = this.tss.getStartTime();
      assertEquals(55155., tstart, 0.0001);

      this.tss.setStartTime(tstart);
      assertEquals(tstart, this.tss.getStartTime(), 0.0001);
      assertEquals(55155., this.tss.getStartTime(), 0.0001);
   }

   /**
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#setStopTime(double)}.
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#setStopTime(java.lang.String)}.
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#getStopTime()}.
    */
   public final void testStopTime() {
      this.tss.setStopTime("200911300000");

      final double tstop = this.tss.getStopTime();
      assertEquals(55165., tstop, 0.0001);

      this.tss.setStopTime(tstop);
      assertEquals(tstop, this.tss.getStopTime(), 0.0001);
      assertEquals(55165., this.tss.getStopTime(), 0.0001);
   }

   /**
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#setStartTime(double)}.
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#setStopTime(double)}.
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#intersectsWithTimeInterval(double, double)}.
    */
   public final void testIntersectsWithTimeInterval() {
      this.tss.setStartTime(55155.);
      this.tss.setStopTime(55165.);

      assertTrue(this.tss.intersectsWithTimeInterval(55150, 55170));
      assertTrue(this.tss.intersectsWithTimeInterval(55150, 55160));
      assertTrue(this.tss.intersectsWithTimeInterval(55160, 55170));
      assertTrue(this.tss.intersectsWithTimeInterval(55160, 55160));
      assertFalse(this.tss.intersectsWithTimeInterval(55140, 55150));
      assertFalse(this.tss.intersectsWithTimeInterval(55170, 55170));
   }

   /**
    * Test method for {@link org.openda.exchange.timeseries.TimeSeriesSelection#isSelected(org.openda.exchange.timeseries.TimeSeries)}.
    */
   public final void testIsSelected() {
      final double[] times1 = {55155., 55165.};
      final double[] times2 = {55555., 55565.};
      final double[] values = {-1., 1.};
      final TimeSeries ts = new TimeSeries(times1, values, "source", "quantity", "unit", "location");
      this.tss.addLocation("location");
      this.tss.addSource("source");
      this.tss.addQuantity("quantity");
      this.tss.setStartTime("200911200000");
      this.tss.setStopTime("200911300000");
      assertTrue(this.tss.isSelected(ts));

      ts.setData(times2, values);
      assertFalse(this.tss.isSelected(ts));
   }

}
