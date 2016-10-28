/* MOD_V2.0
* Copyright (c) 2010 OpenDA Association 
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
public class WaveModelTest extends TestCase {

    public static void testWaveModel() {
        System.out.println("=========================================================");
        System.out.println("Run wave model");
        System.out.println("=========================================================");
        IStochModelFactory factory = new WaveStochModelFactory();
        factory.initialize(null, new String[]{""});
        IStochModelInstance model = factory.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        String content = "time,index,value,std\n"
                + "0.0,6.0,8.1,0.1\n"
                + "0.1,6.0,8.2,0.1\n"
                + "0.2,6.0,8.3,0.1\n"
                + "0.3,6.0,8.4,0.1\n"
                + "0.4,6.0,8.5,0.1\n"
                + "0.5,6.0,8.2,0.1\n"
                + "0.6,6.0,8.6,0.1\n"
                + "0.7,6.0,8.7,0.1\n"
                + "0.8,6.0,8.8,0.1\n"
                + "0.9,6.0,8.9,0.1\n"
                + "1.0,6.0,8.0,0.1";
        IStochObserver observations = new CsvStochObserver(content);
        IObservationDescriptions obsDescriptions = observations.getObservationDescriptions();
        model.announceObservedValues(obsDescriptions);
        model.compute(new Time(1.0));
        System.out.println("model.compute(1.0)");
        //   public Vector getObservedValues(ObservationDescriptions observationDescriptions) {
        IVector predictions = model.getObservationOperator().getObservedValues(obsDescriptions);
        System.out.println("model.getObservationOperator().getObservedValues()=" + predictions.toString());
        // x(0) is taken at t=0.5 for all 3 obs because model did not save previous values
        System.out.println("Should be model.getObservationOperator().getObservedValues()=[0.0,0.09998797888368932,0.18174892847314097,0.19412691322953382,0.1323756648952993,0.020078649042881577,-0.09985050759709486,-0.1526996587110074,-0.17570283804550416,-0.13140230434484873,-0.03703081673406845]");
        assertEquals("model.getObservationOperator().getObservedValues()", predictions.toString(), "[0.0,0.09998797888368932,0.18174892847314097,0.19412691322953382,0.1323756648952993,0.020078649042881577,-0.09985050759709486,-0.1526996587110074,-0.17570283804550416,-0.13140230434484873,-0.03703081673406845]");
        //   public void announceObservedValues(ObservationDescriptions observationDescriptions) {
    }

}




