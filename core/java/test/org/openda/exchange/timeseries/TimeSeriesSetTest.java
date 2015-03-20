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

public class TimeSeriesSetTest extends TestCase {
   private final double times1[]  = { 0.5, 1.0, 2.0, 3.0 };
   private final double values1[] = { 0.0, 0.1, 0.2, 0.3 };

   private final double times2[]  = { 2.0, 2.5, 3.0, 3.5 };
   private final double values2[] = { 1.0, 2.1, 3.2, 4.3 };

   private final double times3[]  = { 0.1, 2.0, 3.3, 3.4 };
   private final double values3[] = { 0.5, 0.6, 0.8, 0.4 };

   private TimeSeries   ts1, ts2, ts3;

   @Override
   protected void setUp() {
      this.ts1 = new TimeSeries(this.times1, this.values1);
      this.ts1.setLocation("hoekvanholland");
      this.ts1.setQuantity("waterlevel");
      this.ts1.setProperty("test", "aap");
      this.ts2 = new TimeSeries(this.times2, this.values2);
      this.ts2.setLocation("hoekvanholland");
      this.ts2.setQuantity("wind_speed");
      this.ts2.setProperty("test", "noot");
      this.ts3 = new TimeSeries(this.times3, this.values3);
      this.ts3.setLocation("ijmuiden");
      this.ts3.setQuantity("waterlevel");
      this.ts3.setProperty("test", "aap");
   }

   public void testConstruct1() {
      TimeSeriesSet set = new TimeSeriesSet();
      set.add(this.ts1);

      assertEquals(1, set.size());
      assertTrue(set.contains(this.ts1));
   }

   public void testConstruct2() {
      TimeSeriesSet set = new TimeSeriesSet();
      set.add(this.ts1);
      set.add(this.ts2);

      assertEquals(2, set.size());
      assertTrue(set.contains(this.ts1));
      assertTrue(set.contains(this.ts2));
   }

   public void testConstruct3() {
      TimeSeriesSet set = new TimeSeriesSet();
      set.add(this.ts1);
      set.add(this.ts1);
      set.add(this.ts2);
      set.add(this.ts2);
      set.add(this.ts1);

      assertEquals(2, set.size());
      assertTrue(set.contains(this.ts1));
      assertTrue(set.contains(this.ts2));

   }

   public void testConstruct4() {
      TimeSeriesSet set1 = new TimeSeriesSet();
      set1.add(this.ts1);
      set1.add(this.ts2);

      TimeSeriesSet set = new TimeSeriesSet(set1);

      assertEquals(2, set.size());
      assertTrue(set.contains(this.ts1));
   }

   public void testConstruct5() {
      TimeSeriesSet set1 = new TimeSeriesSet();
      set1.add(this.ts1);

      TimeSeriesSet set2 = new TimeSeriesSet();
      set2.add(this.ts2);

      TimeSeriesSet set = new TimeSeriesSet(set1);
      set.add(set2);

      assertEquals(2, set.size());
      assertTrue(set.contains(this.ts1));
      assertTrue(set.contains(this.ts2));
   }

   @SuppressWarnings("boxing")
   public void testRemove() {
      TimeSeriesSet set = new TimeSeriesSet();
      set.add(this.ts1);
      set.add(this.ts2);
      set.add(this.ts3);
      set.remove(this.ts2);

      assertEquals(2, set.size());
      assertTrue(set.contains(this.ts1));
      assertFalse(set.contains(this.ts2));
      assertTrue(set.contains(this.ts3));
      assertEquals(3.4, set.getStopTime());
      assertEquals(1, set.getOnLocation("hoekvanholland").size());
      assertEquals(1, set.getOnLocation("ijmuiden").size());
      assertEquals(2, set.getOnProperty("test", "aap").size());
      assertEquals(0, set.getOnProperty("test", "noot").size());

      set.remove(this.ts3);
      assertEquals(1, set.size());
      assertTrue(set.contains(this.ts1));
      assertFalse(set.contains(this.ts2));
      assertFalse(set.contains(this.ts3));
      assertEquals(this.ts1.getStartTime(), set.getStartTime());
      assertEquals(this.ts1.getStopTime(), set.getStopTime());
      assertEquals(1, set.getOnLocation("hoekvanholland").size());
      assertEquals(0, set.getOnLocation("ijmuiden").size());
      assertEquals(1, set.getOnProperty("test", "aap").size());
      assertEquals(0, set.getOnProperty("test", "noot").size());

      set.remove(this.ts1);
      assertEquals(0, set.size());
      assertFalse(set.contains(this.ts1));
      assertFalse(set.contains(this.ts2));
      assertFalse(set.contains(this.ts3));
      assertEquals(0, set.getOnLocation("hoekvanholland").size());
      assertEquals(0, set.getOnLocation("ijmuiden").size());
      assertEquals(0, set.getOnProperty("test", "aap").size());
      assertEquals(0, set.getOnProperty("test", "noot").size());
   }

   public void testGetLocations() {
      TimeSeriesSet set = new TimeSeriesSet();
      set.add(this.ts1);
      set.add(this.ts2);
      set.add(this.ts3);

      Set<String> locations = set.getLocations();
      assertEquals(2, locations.size());
      assertTrue(locations.contains("ijmuiden"));
      assertFalse(locations.contains("lowestoft"));
      assertTrue(locations.contains("hoekvanholland"));

      set.moveLocation("ijmuiden", "lowestoft");

      locations = set.getLocations();
      assertEquals(2, locations.size());
      assertFalse(locations.contains("ijmuiden"));
      assertTrue(locations.contains("lowestoft"));
      assertTrue(locations.contains("hoekvanholland"));
   }

   public void testGetOnLocation() {
      TimeSeriesSet set = new TimeSeriesSet();
      set.add(this.ts1);
      set.add(this.ts2);
      set.add(this.ts3);

      TimeSeriesSet hvh = set.getOnLocation("hoekvanholland");
      assertEquals(2, hvh.size());
      assertTrue(hvh.contains(this.ts1));
      assertTrue(hvh.contains(this.ts2));
      assertFalse(hvh.contains(this.ts3));

      TimeSeriesSelection sel1 = new TimeSeriesSelection();
      sel1.addLocation("hoekvanholland");
      TimeSeriesSet hvh2 = set.getOnSelection(sel1);
      assertEquals(hvh, hvh2);

      TimeSeriesSet lst = set.getOnLocation("lowestoft");
      assertEquals(0, lst.size());
      assertFalse(lst.contains(this.ts1));
      assertFalse(lst.contains(this.ts2));
      assertFalse(lst.contains(this.ts3));

      TimeSeriesSelection sel2 = new TimeSeriesSelection();
      sel2.addLocation("lowestoft");
      TimeSeriesSet lst2 = set.getOnSelection(sel2);
      assertEquals(lst, lst2);

      TimeSeriesSet ijm = set.getOnLocation("ijmuiden");
      assertEquals(1, ijm.size());
      assertFalse(ijm.contains(this.ts1));
      assertFalse(ijm.contains(this.ts2));
      assertTrue(ijm.contains(this.ts3));

      TimeSeriesSelection sel3 = new TimeSeriesSelection();
      sel3.addLocation("ijmuiden");
      TimeSeriesSet ijm2 = set.getOnSelection(sel3);
      assertEquals(ijm, ijm2);

      sel3.addLocation("hoekvanholland");
      TimeSeriesSet s = set.getOnSelection(sel3);
      hvh.add(ijm);
      assertEquals(hvh, s);
   }

   public void testGetOnSelection() {
      TimeSeriesSet set = new TimeSeriesSet();
      set.add(this.ts1);
      set.add(this.ts2);
      set.add(this.ts3);

      TimeSeriesSelection sel1 = new TimeSeriesSelection();
      sel1.addLocation("hoekvanholland");
      sel1.addQuantity("waterlevel");
      TimeSeriesSet set1 = set.getOnSelection(sel1);
      assertEquals(1, set1.size());
      assertTrue(set1.contains(this.ts1));
      assertFalse(set1.contains(this.ts2));
      assertFalse(set1.contains(this.ts3));

      TimeSeriesSelection sel2 = new TimeSeriesSelection();
      sel2.addLocation("hoekvanholland");
      sel2.addLocation("ijmuiden");
      sel2.addQuantity("waterlevel");
      TimeSeriesSet set2 = set.getOnSelection(sel2);
      assertEquals(2, set2.size());
      assertTrue(set2.contains(this.ts1));
      assertFalse(set2.contains(this.ts2));
      assertTrue(set2.contains(this.ts3));

      TimeSeriesSelection sel3 = new TimeSeriesSelection();
      sel3.addLocation("hoekvanholland");
      sel3.addLocation("ijmuiden");
      sel3.addQuantity("waterlevel");
      sel3.addQuantity("wind_speed");
      TimeSeriesSet set3 = set.getOnSelection(sel3);
      assertEquals(3, set3.size());
      assertTrue(set3.contains(this.ts1));
      assertTrue(set3.contains(this.ts2));
      assertTrue(set3.contains(this.ts3));
   }

   @SuppressWarnings("boxing")
   public void testGetTimes1() {
      TimeSeriesSet set = new TimeSeriesSet();
      set.add(this.ts1);

      double tstart = set.getStartTime();
      double tstop = set.getStopTime();
      assertEquals(0.5, tstart);
      assertEquals(3.0, tstop);
   }

   @SuppressWarnings("boxing")
   public void testGetTimes2() {
      TimeSeriesSet set = new TimeSeriesSet();
      set.add(this.ts1);
      set.add(this.ts2);
      set.add(this.ts3);

      double tstart = set.getStartTime();
      double tstop = set.getStopTime();
      assertEquals(0.1, tstart);
      assertEquals(3.5, tstop);
   }

   public void testInterval1() {
      TimeSeriesSet set = new TimeSeriesSet();
      set.add(this.ts1);

      assertEquals(0, set.intersectsWithTimeInterval(0.0, 0.4).size());
      assertEquals(1, set.intersectsWithTimeInterval(0.0, 0.5).size());
      assertEquals(1, set.intersectsWithTimeInterval(0.5, 3.0).size());
      assertEquals(1, set.intersectsWithTimeInterval(0.0, 9.9).size());
      assertEquals(1, set.intersectsWithTimeInterval(0.7, 0.7).size());
      assertEquals(0, set.intersectsWithTimeInterval(3.6, 3.7).size());
   }

   public void testInterval2() {
      TimeSeriesSet set = new TimeSeriesSet();
      set.add(this.ts1);
      set.add(this.ts2);
      set.add(this.ts3);

      assertEquals(1, set.intersectsWithTimeInterval(0.0, 0.4).size());
      assertEquals(2, set.intersectsWithTimeInterval(0.0, 0.7).size());
      assertEquals(2, set.intersectsWithTimeInterval(1.7, 1.7).size());
      assertEquals(3, set.intersectsWithTimeInterval(2.2, 2.2).size());
      assertEquals(3, set.intersectsWithTimeInterval(0.0, 9.9).size());
      assertEquals(3, set.intersectsWithTimeInterval(3.0, 9.9).size());
      assertEquals(2, set.intersectsWithTimeInterval(3.1, 3.1).size());
      assertEquals(1, set.intersectsWithTimeInterval(3.5, 9.9).size());
      assertEquals(0, set.intersectsWithTimeInterval(3.6, 9.9).size());
   }

   public void testSerial() {
      TimeSeriesSet set = new TimeSeriesSet();
      set.add(this.ts1);
      set.add(this.ts2);
      set.add(this.ts3);

      TimeSeriesSet subset1 = set.intersectsWithTimeInterval(3.1, 3.3);
      TimeSeriesSet subset2 = subset1.getOnLocation("hoekvanholland");

      assertEquals(3, set.size());
      assertEquals(2, subset1.size());
      assertEquals(1, subset2.size());

      assertFalse(subset2.contains(this.ts1));
      assertTrue(subset2.contains(this.ts2));
      assertFalse(subset2.contains(this.ts3));
   }

   public void testGetOnProperty() {
      TimeSeriesSet set = new TimeSeriesSet();
      set.add(this.ts1);
      set.add(this.ts2);
      set.add(this.ts3);

      assertEquals(1, set.getProperties().size());
      assertTrue(set.getProperties().contains("test"));
      assertEquals(2, set.getPropertyValues("test").size());
      assertTrue(set.getPropertyValues("test").contains("aap"));
      assertTrue(set.getPropertyValues("test").contains("noot"));
      assertEquals(0, set.getPropertyValues("ietsAnders").size());

      TimeSeriesSet aap = set.getOnProperty("test", "aap");
      assertEquals(2, aap.size());
      assertTrue(aap.contains(this.ts1));
      assertFalse(aap.contains(this.ts2));
      assertTrue(aap.contains(this.ts3));

      TimeSeriesSet noot = set.getOnProperty("test", "noot");
      assertEquals(1, noot.size());
      assertFalse(noot.contains(this.ts1));
      assertTrue(noot.contains(this.ts2));
      assertFalse(noot.contains(this.ts3));

      TimeSeriesSet mies = set.getOnProperty("test", "mies");
      assertEquals(0, mies.size());

      TimeSeriesSet tralala = set.getOnProperty("tralala", "aap");
      assertEquals(0, tralala.size());
   }

}
