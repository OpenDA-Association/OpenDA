/* OpenDA v2.4
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
package org.openda;

/**
 * Main class for testing LorenzModel class
 */
import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.utils.CsvStochObserver;
import org.openda.utils.Time;

/**
 * Test for simple vector
 */
public class AdvectionModelTest extends TestCase {

    public static void testAdvectionModel() {
        System.out.println("=========================================================");
        System.out.println("Run advection");
        System.out.println("=========================================================");
        IStochModelFactory factory = new AdvectionStochModelFactory();
        factory.initialize(null, new String[]{""});
        IStochModelInstance model = factory.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        String content = "time,index,value,std\n"
                + "0.0,6.0,8.1,0.1\n"
                + "0.1,6.0,8.2,0.1\n"
                + "0.2,6.0,8.3,0.1\n"
                + "0.3,6.0,8.4,0.1\n";
        IStochObserver observations = new CsvStochObserver(content);
        IObservationDescriptions obsDescriptions = observations.getObservationDescriptions();
        model.announceObservedValues(obsDescriptions);
        model.compute(new Time(1.0));
        System.out.println("model.compute(0.3)");
        //   public Vector getObservedValues(ObservationDescriptions observationDescriptions) {
        IVector prd = model.getObservedValues(obsDescriptions);
        System.out.println("model.getObservedValues()=" + prd.toString());
        // x(0) is taken at t=0.5 for all 3 obs because model did not save previous values
        System.out.println("Should be model.getObservedValues()=[0.0,0.451798531137781,1.3094893124320952,1.102222423765312]");
        assertEquals("model.getObservedValues()", prd.toString(), "[0.0,0.451798531137781,1.3094893124320952,1.102222423765312]");
        //   public void announceObservedValues(ObservationDescriptions observationDescriptions) {
    }

}

