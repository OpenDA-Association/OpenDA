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

/**
 * @author johan
 *
 */
public class TimeSeriesSetAnalyzerTest extends TestCase {
   private final double  times1[]  = { 0.0, 1.0, 2.0, 3.0 };
   private final double  values1[] = { -5.0, -2.0, 1.0, 4.0 };

   private final double  times2[]  = { 1.0, 1.5, 2.0, 2.5 };
   private final double  values2[] = { -1.0, 0.5, 2.0, 3.5 };

   private final double  times3[]  = { 1.8, 1.9, 2.0, 2.1 };
   private final double  values3[] = { 1.8, 2.4, 3.0, 3.6 };

   private TimeSeries    ts1, ts2, ts3;
   private TimeSeriesSet set;

   protected void setUp() {
      ts1 = new TimeSeries(times1, values1);
      ts2 = new TimeSeries(times2, values2);
      ts3 = new TimeSeries(times3, values3);

      set = new TimeSeriesSet();
      set.add(ts1);
      set.add(ts2);
      set.add(ts3);
   }

   public void testCalcAverage1() {
      TimeSeriesSetAnalyzer analyzer = new TimeSeriesSetAnalyzer(set);
      assertEquals(0.0, analyzer.getStartTime());
      assertEquals(3.0, analyzer.getStopTime());

      analyzer.setInterpolate(false);
      TimeSeries rms = analyzer.calcAverageTimeSeries(0.9, 2.4);
      assertEquals(1.0, rms.getStartTime());
      assertEquals(2.1, rms.getStopTime());
      assertTrue(rms.getExtraValuesKeySet().contains("nseries"));

      assertEquals(-1.5, rms.getValue(1.0));
      assertEquals(2., rms.getExtraValueAsDouble("nseries", 1.0));

      assertEquals(0.5, rms.getValue(1.5));
      assertEquals(1., rms.getExtraValueAsDouble("nseries", 1.5));

      assertEquals(2.0, rms.getValue(2.0));
      assertEquals(3., rms.getExtraValueAsDouble("nseries", 2.0));

      assertEquals(2.4, rms.getValue(1.9));
      assertEquals(1., rms.getExtraValueAsDouble("nseries", 1.9));
   }

   public void testCalcAverage2() {
      TimeSeriesSetAnalyzer analyzer = new TimeSeriesSetAnalyzer(set);
      assertEquals(0.0, analyzer.getStartTime());
      assertEquals(3.0, analyzer.getStopTime());

      analyzer.setInterpolate(true);
      TimeSeries avg2 = analyzer.calcAverageTimeSeries(-10., 10.);
      assertEquals(0.0, avg2.getStartTime());
      assertEquals(3.0, avg2.getStopTime());
      assertTrue(avg2.getExtraValuesKeySet().contains("nseries"));

      assertEquals(-1.5, avg2.getValue(1.0));
      assertEquals(2., avg2.getExtraValueAsDouble("nseries", 1.0));

      assertEquals(0., avg2.getValue(1.5));
      assertEquals(2., avg2.getExtraValueAsDouble("nseries", 1.5));

      assertEquals(1.6, avg2.getValue(1.9), 0.01);
      assertEquals(3., avg2.getExtraValueAsDouble("nseries", 1.9));

      assertTrue(Double.isNaN(avg2.getValue(1.95)));
      assertEquals(1.8, avg2.getInterpolatedValue(1.95), 0.01);
      assertTrue(Double.isNaN(avg2.getExtraValueAsDouble("nseries", 1.95)));

      assertEquals(2.0, avg2.getValue(2.0));
      assertEquals(3., avg2.getExtraValueAsDouble("nseries", 2.0));

      assertEquals(3.0, avg2.getValue(2.5));
      assertEquals(2., avg2.getExtraValueAsDouble("nseries", 2.5));
   }

   public void testCalcStdDev1() {
      TimeSeriesSetAnalyzer analyzer = new TimeSeriesSetAnalyzer(set);
      assertEquals(0.0, analyzer.getStartTime());
      assertEquals(3.0, analyzer.getStopTime());

      analyzer.setInterpolate(false);
      TimeSeries avg = analyzer.calcAverageTimeSeries(0.9, 2.4);
      TimeSeries stddev = analyzer.calcStdDevTimeSeries(avg);
      assertEquals(1.0, stddev.getStartTime());
      assertEquals(2.1, stddev.getStopTime());
      assertTrue(stddev.getExtraValuesKeySet().contains("nseries"));

      assertEquals(Math.sqrt(.5 * .5 + .5 * .5), stddev.getValue(1.0));
      assertEquals(2., stddev.getExtraValueAsDouble("nseries", 1.0));

      assertEquals(0., stddev.getValue(1.5));
      assertEquals(1., stddev.getExtraValueAsDouble("nseries", 1.5));

      assertEquals(1., stddev.getValue(2.0));
      assertEquals(3., stddev.getExtraValueAsDouble("nseries", 2.0));

      assertEquals(0., stddev.getValue(1.9));
      assertEquals(1., stddev.getExtraValueAsDouble("nseries", 1.9));
   }

   public void testCalcStdDev2() {
      TimeSeriesSetAnalyzer analyzer = new TimeSeriesSetAnalyzer(set);
      assertEquals(0.0, analyzer.getStartTime());
      assertEquals(3.0, analyzer.getStopTime());

      analyzer.setInterpolate(true);
      TimeSeries avg = analyzer.calcAverageTimeSeries(-10., 10);
      TimeSeries stddev = analyzer.calcStdDevTimeSeries(avg);
      assertEquals(0.0, stddev.getStartTime());
      assertEquals(3.0, stddev.getStopTime());
      assertTrue(stddev.getExtraValuesKeySet().contains("nseries"));

      assertEquals(Math.sqrt(.5 * .5 + .5 * .5), stddev.getValue(1.0));
      assertEquals(2., stddev.getExtraValueAsDouble("nseries", 1.0));

      assertEquals(Math.sqrt(.5 * .5 + .5 * .5), stddev.getValue(1.5));
      assertEquals(2., stddev.getExtraValueAsDouble("nseries", 1.5));

      assertEquals(Math.sqrt((.9 * .9 + .1 * .1 + .8 * .8) / 2.), stddev.getValue(1.9), 0.01);
      assertEquals(3., stddev.getExtraValueAsDouble("nseries", 1.9));

      assertEquals(Math.sqrt((.95 * .95 + .05 * .05 + .9 * .9) / 2.), stddev.getInterpolatedValue(1.95), 0.01);

      assertEquals(1., stddev.getValue(2.0));
      assertEquals(3., stddev.getExtraValueAsDouble("nseries", 2.0));

      assertEquals(Math.sqrt(.5 * .5 + .5 * .5), stddev.getValue(2.5));
      assertEquals(2., stddev.getExtraValueAsDouble("nseries", 2.5));
   }

   public void testCalcRMS1() {
      TimeSeriesSetAnalyzer analyzer = new TimeSeriesSetAnalyzer(set);
      assertEquals(0.0, analyzer.getStartTime());
      assertEquals(3.0, analyzer.getStopTime());

      analyzer.setInterpolate(false);
      TimeSeries rms = analyzer.calcRMSTimeSeries(0.9, 2.4);
      assertEquals(1.0, rms.getStartTime());
      assertEquals(2.1, rms.getStopTime());
      assertTrue(rms.getExtraValuesKeySet().contains("nseries"));

      assertEquals(Math.sqrt(2.5), rms.getValue(1.0));
      assertEquals(2., rms.getExtraValueAsDouble("nseries", 1.0));

      assertEquals(.5, rms.getValue(1.5));
      assertEquals(1., rms.getExtraValueAsDouble("nseries", 1.5));

      assertEquals(Math.sqrt(14. / 3.), rms.getValue(2.0));
      assertEquals(3., rms.getExtraValueAsDouble("nseries", 2.0));

      assertEquals(2.4, rms.getValue(1.9));
      assertEquals(1., rms.getExtraValueAsDouble("nseries", 1.9));
   }

   public void testCalcRMS2() {
      TimeSeriesSetAnalyzer analyzer = new TimeSeriesSetAnalyzer(set);
      assertEquals(0.0, analyzer.getStartTime());
      assertEquals(3.0, analyzer.getStopTime());

      analyzer.setInterpolate(true);
      TimeSeries rms = analyzer.calcRMSTimeSeries(-10., 10.);
      assertEquals(0.0, rms.getStartTime());
      assertEquals(3.0, rms.getStopTime());
      assertTrue(rms.getExtraValuesKeySet().contains("nseries"));

      assertEquals(Math.sqrt(2.5), rms.getValue(1.0));
      assertEquals(2., rms.getExtraValueAsDouble("nseries", 1.0));

      assertEquals(0.5, rms.getValue(1.5));
      assertEquals(2., rms.getExtraValueAsDouble("nseries", 1.5));

      assertEquals(Math.sqrt((.7 * .7 + 1.7 * 1.7 + 2.4 * 2.4) / 3.), rms.getValue(1.9));
      assertEquals(3., rms.getExtraValueAsDouble("nseries", 1.9));

      assertEquals(Math.sqrt((.85 * .85 + 1.85 * 1.85 + 2.7 * 2.7) / 3.), rms.getInterpolatedValue(1.95), 0.01);

      assertEquals(Math.sqrt(14. / 3.), rms.getValue(2.0));
      assertEquals(3., rms.getExtraValueAsDouble("nseries", 2.0));

      assertEquals(Math.sqrt((2.5 * 2.5 + 3.5 * 3.5) / 2.), rms.getValue(2.5));
      assertEquals(2., rms.getExtraValueAsDouble("nseries", 2.5));
   }

   public void testCalcAvgDiff1() {
      TimeSeriesSetAnalyzer analyzer = new TimeSeriesSetAnalyzer(set);
      assertEquals(0.0, analyzer.getStartTime());
      assertEquals(3.0, analyzer.getStopTime());

      analyzer.setInterpolate(true);
      analyzer.setAbsoluteDiffs(false);
      TimeSeries avg = analyzer.calcAverageTimeSeries();
      TimeSeries diff = analyzer.calcAverageDiffTimeSeries(avg);
      assertEquals(0.0, diff.getStartTime());
      assertEquals(3.0, diff.getStopTime());
      assertTrue(diff.getExtraValuesKeySet().contains("nseries"));

      assertEquals(0., diff.getValue(1.0));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 1.0));

      assertEquals(0., diff.getValue(1.5));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 1.5));

      assertEquals(0., diff.getValue(2.0));
      assertEquals(3., diff.getExtraValueAsDouble("nseries", 2.0));

      assertEquals(0., diff.getValue(1.9), 0.01);
      assertEquals(3., diff.getExtraValueAsDouble("nseries", 1.9));
   }

   public void testCalcAvgDiff2() {
      TimeSeriesSetAnalyzer analyzer = new TimeSeriesSetAnalyzer(set);
      assertEquals(0.0, analyzer.getStartTime());
      assertEquals(3.0, analyzer.getStopTime());

      analyzer.setInterpolate(true);
      analyzer.setAbsoluteDiffs(true);
      TimeSeries avg = analyzer.calcAverageTimeSeries();
      TimeSeries diff = analyzer.calcAverageDiffTimeSeries(avg);
      assertEquals(0.0, diff.getStartTime());
      assertEquals(3.0, diff.getStopTime());
      assertTrue(diff.getExtraValuesKeySet().contains("nseries"));

      assertEquals(0.5, diff.getValue(1.0));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 1.0));

      assertEquals(0.5, diff.getValue(1.5));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 1.5));

      assertEquals(2./3., diff.getValue(2.0));
      assertEquals(3., diff.getExtraValueAsDouble("nseries", 2.0));

      assertEquals(0.6, diff.getValue(1.9), 0.01);
      assertEquals(3., diff.getExtraValueAsDouble("nseries", 1.9));
   }

   public void testCalcRMSDiff() {
      TimeSeriesSetAnalyzer analyzer = new TimeSeriesSetAnalyzer(set);
      assertEquals(0.0, analyzer.getStartTime());
      assertEquals(3.0, analyzer.getStopTime());

      analyzer.setInterpolate(true);
      TimeSeries avg = analyzer.calcAverageTimeSeries();
      assertEquals(0.0, avg.getStartTime());
      assertEquals(3.0, avg.getStopTime());
      TimeSeries diff = analyzer.calcRMSDiffTimeSeries(avg);
      assertEquals(0.0, diff.getStartTime());
      assertEquals(3.0, diff.getStopTime());
      assertTrue(diff.getExtraValuesKeySet().contains("nseries"));

      assertEquals(0.5, diff.getValue(1.0));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 1.0));

      assertEquals(0.5, diff.getValue(1.5));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 1.5));

      assertEquals(Math.sqrt(2./3.), diff.getValue(2.0));
      assertEquals(3., diff.getExtraValueAsDouble("nseries", 2.0));

      assertEquals(Math.sqrt((.9*.9+.1*.1+.8*.8)/3.), diff.getValue(1.9), 0.01);
      assertEquals(3., diff.getExtraValueAsDouble("nseries", 1.9));
   }

   public void testCalcStdDevDiff() {
      TimeSeriesSetAnalyzer analyzer = new TimeSeriesSetAnalyzer(set);
      assertEquals(0.0, analyzer.getStartTime());
      assertEquals(3.0, analyzer.getStopTime());

      // Reference time series
      final double[] times  = {0.0, 5.0};
      final double[] values = {0.0, 0.0};
      TimeSeries noughts = new TimeSeries(times, values);

      analyzer.setInterpolate(true);
      TimeSeries avg = analyzer.calcAverageTimeSeries();
      TimeSeries stddev = analyzer.calcStdDevDiffTimeSeries(avg, noughts);
      assertEquals(0.0, stddev.getStartTime());
      assertEquals(3.0, stddev.getStopTime());
      assertTrue(stddev.getExtraValuesKeySet().contains("nseries"));

      assertEquals(Math.sqrt(.5 * .5 + .5 * .5), stddev.getValue(1.0));
      assertEquals(2., stddev.getExtraValueAsDouble("nseries", 1.0));

      assertEquals(Math.sqrt(.5 * .5 + .5 * .5), stddev.getValue(1.5));
      assertEquals(2., stddev.getExtraValueAsDouble("nseries", 1.5));

      assertEquals(Math.sqrt((.9 * .9 + .1 * .1 + .8 * .8) / 2.), stddev.getValue(1.9), 0.01);
      assertEquals(3., stddev.getExtraValueAsDouble("nseries", 1.9));

      assertEquals(Math.sqrt((.95 * .95 + .05 * .05 + .9 * .9) / 2.), stddev.getInterpolatedValue(1.95), 0.01);

      assertEquals(1., stddev.getValue(2.0));
      assertEquals(3., stddev.getExtraValueAsDouble("nseries", 2.0));

      assertEquals(Math.sqrt(.5 * .5 + .5 * .5), stddev.getValue(2.5));
      assertEquals(2., stddev.getExtraValueAsDouble("nseries", 2.5));
   }

   public void testCalcMaxDiff1() {
      TimeSeriesSetAnalyzer analyzer = new TimeSeriesSetAnalyzer(set);
      assertEquals(0.0, analyzer.getStartTime());
      assertEquals(3.0, analyzer.getStopTime());

      analyzer.setInterpolate(false);
      analyzer.setAbsoluteDiffs(false);
      TimeSeries avg = analyzer.calcAverageTimeSeries(0.9, 2.4);
      TimeSeries diff = analyzer.calcMaximumDiffTimeSeries(avg);
      assertEquals(1.0, diff.getStartTime());
      assertEquals(2.1, diff.getStopTime());
      assertTrue(diff.getExtraValuesKeySet().contains("nseries"));

      assertEquals(.5, diff.getValue(1.0));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 1.0));

      assertEquals(0., diff.getValue(1.5));
      assertEquals(1., diff.getExtraValueAsDouble("nseries", 1.5));

      assertEquals(1., diff.getValue(2.0));
      assertEquals(3., diff.getExtraValueAsDouble("nseries", 2.0));

      assertEquals(0., diff.getValue(1.9));
      assertEquals(1., diff.getExtraValueAsDouble("nseries", 1.9));
   }

   public void testCalcMaxDiff2() {
      TimeSeriesSetAnalyzer analyzer = new TimeSeriesSetAnalyzer(set);
      assertEquals(0.0, analyzer.getStartTime());
      assertEquals(3.0, analyzer.getStopTime());

      analyzer.setInterpolate(true);
      analyzer.setAbsoluteDiffs(false);
      TimeSeries avg = analyzer.calcAverageTimeSeries(-10., 10);
      TimeSeries diff = analyzer.calcMaximumDiffTimeSeries(avg);
      assertEquals(0.0, diff.getStartTime());
      assertEquals(3.0, diff.getStopTime());
      assertTrue(diff.getExtraValuesKeySet().contains("nseries"));

      assertEquals(.5, diff.getValue(1.0));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 1.0));

      assertEquals(.5, diff.getValue(1.5));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 1.5));

      assertEquals(.8, diff.getValue(1.9), 0.01);
      assertEquals(3., diff.getExtraValueAsDouble("nseries", 1.9));

      assertEquals(.9, diff.getInterpolatedValue(1.95), 0.01);

      assertEquals(1., diff.getValue(2.0));
      assertEquals(3., diff.getExtraValueAsDouble("nseries", 2.0));

      assertEquals(.5, diff.getValue(2.5));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 2.5));
   }

   public void testCalcMaxDiff3() {
      TimeSeriesSetAnalyzer analyzer = new TimeSeriesSetAnalyzer(set);
      assertEquals(0.0, analyzer.getStartTime());
      assertEquals(3.0, analyzer.getStopTime());

      analyzer.setInterpolate(true);
      analyzer.setAbsoluteDiffs(true);
      TimeSeries avg = analyzer.calcAverageTimeSeries(-10., 10);
      TimeSeries diff = analyzer.calcMaximumDiffTimeSeries(avg);
      assertEquals(0.0, diff.getStartTime());
      assertEquals(3.0, diff.getStopTime());
      assertTrue(diff.getExtraValuesKeySet().contains("nseries"));

      assertEquals(.5, diff.getValue(1.0));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 1.0));

      assertEquals(.5, diff.getValue(1.5));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 1.5));

      assertEquals(.9, diff.getValue(1.9), 0.01);
      assertEquals(3., diff.getExtraValueAsDouble("nseries", 1.9));

      assertEquals(.95, diff.getInterpolatedValue(1.95));

      assertEquals(1., diff.getValue(2.0));
      assertEquals(3., diff.getExtraValueAsDouble("nseries", 2.0));

      assertEquals(.5, diff.getValue(2.5));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 2.5));
   }

   public void testCalcMinDiff() {
      TimeSeriesSetAnalyzer analyzer = new TimeSeriesSetAnalyzer(set);
      assertEquals(0.0, analyzer.getStartTime());
      assertEquals(3.0, analyzer.getStopTime());

      analyzer.setInterpolate(true);
      analyzer.setAbsoluteDiffs(false);
      TimeSeries avg = analyzer.calcAverageTimeSeries();
      TimeSeries diff = analyzer.calcMinimumDiffTimeSeries(avg);
      assertEquals(0.0, diff.getStartTime());
      assertEquals(3.0, diff.getStopTime());
      assertTrue(diff.getExtraValuesKeySet().contains("nseries"));

      assertEquals(-0.5, diff.getValue(1.0));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 1.0));

      assertEquals(-0.5, diff.getValue(1.5));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 1.5));

      assertEquals(-0.9, diff.getValue(1.9), 0.01);
      assertEquals(3., diff.getExtraValueAsDouble("nseries", 1.9));

      assertEquals(-0.95, diff.getInterpolatedValue(1.95), 0.01);

      assertEquals(-1., diff.getValue(2.0));
      assertEquals(3., diff.getExtraValueAsDouble("nseries", 2.0));

      assertEquals(-0.5, diff.getValue(2.5));
      assertEquals(2., diff.getExtraValueAsDouble("nseries", 2.5));
   }


}
