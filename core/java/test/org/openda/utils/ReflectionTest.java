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
package org.openda.utils;
import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;

import java.util.Set;

public class ReflectionTest extends TestCase {

	public void testClasses(){
		System.out.println("==============================================================================");
		System.out.println(" Basic test for utilities that look through the available classes");
		System.out.println("==============================================================================");

		// list all relevant classes
		Reflection ref = new Reflection(new String[]{"org.openda.observers","org.openda.interfaces","org.openda.blackbox"});
		Set<String> classNames = ref.getClassNames();
		assertTrue(classNames.size()>10); //some classes were found
		int counter=0;
		for(String className : classNames){
			if(counter<10){
				System.out.println("className:"+className);
				counter++;
			}
		}

		// list all relevant interfaces
		Set<String> ifaces = ref.getInterfaceNames();
		assertTrue(classNames.size()>10); //some classes were found
		counter=0;
		for(String iface : ifaces){
			if(counter<10){
				System.out.println("interface:"+iface);
				counter++;
			}
		}

		// get classes that implement a certain interface
		String ifaceName = ref.findFullName("IDataObject");
		assertEquals("org.openda.interfaces.IDataObject", ifaceName);
		Set<String> selection = ref.getClassNames(ifaceName);
		System.out.println("There are "+selection.size()+" classes that implement the IDataObject");
		assertTrue(selection.size()>4); //some classes were found
		counter=0;
		System.out.println("classes that implement the IDataObject:");
		for(String classname : selection){
			System.out.println("  "+counter+" class name:"+classname);
			counter++;
		}
	}

	public void testImplementsInterface() throws ClassNotFoundException {
		System.out.println("==============================================================================");
		System.out.println(" Basic test for relation between classes that implement an interface");
		System.out.println("==============================================================================");
		Class aClass = Class.forName("org.openda.exchange.ioobjects.NoosTimeSeriesIoObject");
		Class[] interfaces = aClass.getInterfaces();
		boolean found=false;
		for(Class iface : interfaces){
			String iName = iface.getCanonicalName();
			System.out.println("interface : "+iName);
			if(iName.endsWith("IoObjectInterface")){
				found=true;
			}
		}
		assertTrue(found);
	}

	public void testInfoMain(){
		System.out.println("==============================================================================");
		System.out.println(" Produce info with main method");
		System.out.println("==============================================================================");
		if (BBUtils.RUNNING_ON_WINDOWS) {
			// native dll for reflection (sangoma) is not avaiable on Windows
			System.out.println(" Test disabled on Windows because it fails on missing sangoma.dll.");
		} else {
			Reflection.main(new String[0]);
		}
	}
}
