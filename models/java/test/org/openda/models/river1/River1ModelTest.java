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
package org.openda.models.river1;
import junit.framework.TestCase;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IVector;
import org.openda.utils.Time;
import org.openda.utils.Vector;


/**
 * Created by IntelliJ IDEA.
 * User: nils
 * Date: Apr 29, 2009
 * Time: 11:51:14 AM
 * To change this template use File | Settings | File Templates.
 */
public class River1ModelTest extends TestCase {

    public static void testRiver1ModelTest_1() {
        System.out.println("=========================================================");
        System.out.println("Model construction ");
        System.out.println("=========================================================");
        // Constructors
        IStochModelFactory fact1a = new River1StochModelFactory();
        fact1a.initialize(null, new String[]{"<river1Config></river1Config>"});
        IStochModelInstance mod1a = fact1a.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model
        //   public String toString(){
        String mod1aString = mod1a.toString();
        System.out.println("mod1a=" + mod1aString);
        System.out.println("Should be "
                + "mod1a=OscillatorStochModelInstance 1 ...");
        assertEquals("mod1a.tostring()"
                , mod1aString.substring(0, "org.openda.models.river1.River1StochModelInstance ".length())
                , "org.openda.models.river1.River1StochModelInstance ");
        IVector x1a = mod1a.getState();
        System.out.println("mod1a.getState()=" + x1a.toString());
        System.out.println("Should be mod1a.getState() =[0.0,0.0]");
        assertEquals("mod1a.getState()", x1a.toString(), "[0.0,0.0]");
    }

    public static void testRiver1ModelTest_2() {
        System.out.println("=========================================================");
        System.out.println("Time Stepping                                            ");
        System.out.println("=========================================================");
        // Constructors
        IStochModelFactory fact2 = new River1StochModelFactory();
        fact2.initialize(null, new String[]{"<river1Config></river1Config>"});
        IStochModelInstance mod1 = fact2.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a model

        mod1.compute(new Time(10.0));

        IVector x=mod1.getState();
        // exact answer is: 1.10 and 2.33
        assertEquals("mod2.getState()[0]",1.0607,x.getValue(0),0.001);
        assertEquals("mod2.getState()[1]",2.2468,x.getValue(1),0.001);

        IStochModelInstance mod2 = fact2.getInstance(IStochModelFactory.OutputLevel.Suppress); // get a second model
        IVector par = new Vector("[0.5, 3.0]");
        mod2.setParameters(par);
        mod2.compute(new Time(10.0));
        x=mod2.getState();

        // the equilibrium solution answer is: 1.05 and 2.465
        assertEquals("mod2.getState()[0]",1.0125,x.getValue(0),0.001);
        assertEquals("mod2.getState()[1]",2.3770,x.getValue(1),0.001);



    }




}
