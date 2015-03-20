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
package org.openda.exchange;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IArrayGeometryInfo;
import org.openda.interfaces.IArrayTimeInfo;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.interfaces.IQuantityInfo;
import org.openda.utils.Array;

import junit.framework.TestCase;


public class ExchangeItemTest extends TestCase{
	
	public static void testDoubleExchangeItem() {
		DoubleExchangeItem item = new DoubleExchangeItem("id",Role.Input, 1.234);

		String id = item.getId();
		assertEquals("id", id);

		IQuantityInfo quantityInfo = new QuantityInfo("volume", "m^3");
		item.setQuantityInfo(quantityInfo);
		String quantity = item.getQuantityInfo().getQuantity();
		assertEquals("volume", quantity);
		String unit = item.getQuantityInfo().getUnit();
		assertEquals("m^3", unit);

		Role role = item.getRole();
		assertEquals(IPrevExchangeItem.Role.Input, role);

		item.setValue(2.0);
		double value = item.getValue();
		assertEquals(2.0, value,0.0001);
		
		DummyObserver observer = new DummyObserver();
		item.addObserver(observer);
		assertEquals(false, observer.changed);
		item.setValue(20.0);
		value = item.getValue();
		assertEquals(20.0, value,0.0001);
		assertEquals(true, observer.changed);	
	}

	public static void testIntExchangeItem() {
		IntExchangeItem item = new IntExchangeItem("id",Role.Input, 3);

		String id = item.getId();
		assertEquals("id", id);

		IQuantityInfo quantityInfo = new QuantityInfo("birdCount", "1/m^2");
		item.setQuantityInfo(quantityInfo);
		String quantity = item.getQuantityInfo().getQuantity();
		assertEquals("birdCount", quantity);
		String unit = item.getQuantityInfo().getUnit();
		assertEquals("1/m^2", unit);

		Role role = item.getRole();
		assertEquals(IPrevExchangeItem.Role.Input, role);

		item.setValue(2);
		int value = item.getValue();
		assertEquals(2, value);
		
		DummyObserver observer = new DummyObserver();
		item.addObserver(observer);
		assertEquals(false, observer.changed);
		item.setValue(20);
		value = item.getValue();
		assertEquals(20, value);
		assertEquals(true, observer.changed);	
	}

	public static void testStringExchangeItem() {
		StringExchangeItem item = new StringExchangeItem("id",Role.Input, "logfile.txt");

		String id = item.getId();
		assertEquals("id", id);

		Role role = item.getRole();
		assertEquals(IPrevExchangeItem.Role.Input, role);

		item.setValue("logfile.dat");
		String value = item.getValue();
		assertEquals("logfile.dat", value);
		
		DummyObserver observer = new DummyObserver();
		item.addObserver(observer);
		assertEquals(false, observer.changed);
		item.setValue("output.log");
		value = item.getValue();
		assertEquals("output.log", value);
		assertEquals(true, observer.changed);	
	}

	public static void testDoublesExchangeItem() {
		double[] originalValues={1.0,2.0,3.0,4.0};
		DoublesExchangeItem item = new DoublesExchangeItem("id",Role.InOut, originalValues );

		String id = item.getId();
		assertEquals("id", id);

		IQuantityInfo qInfo = new QuantityInfo("volume", "m^3");
		item.setQuantityInfo(qInfo);
		String quantity = item.getQuantityInfo().getQuantity();
		assertEquals("volume", quantity);
		String unit = item.getQuantityInfo().getUnit();
		assertEquals("m^3", unit);

		Role role = item.getRole();
		assertEquals(IPrevExchangeItem.Role.InOut, role);

		double values[] = item.getValuesAsDoubles();
		assertEquals(4, values.length);
		assertEquals(4.0, values[3],0.0001);
		
		DummyObserver observer = new DummyObserver();
		item.addObserver(observer);
		assertEquals(false, observer.changed);
		
		double[] newValues={0.1,0.2,0.3};
		item.setValues(newValues);
		values = item.getValuesAsDoubles();
		assertEquals(3, values.length);
		assertEquals(0.3, values[2],0.0001);
		assertEquals(true, observer.changed);
	}

	public static void testGridXYTExchangeItem() {
		
		XYTGridExchangeItem item = new XYTGridExchangeItem("pressure", Role.InOut);
		
		double[] p=new double[]{111.,112.,113.,121.,122.,123.,131.,132.,133.,141.,142.,143.,
				211.,212.,213.,221.,222.,223.,231.,232.,233.,241.,242.,243. };
		item.setArray(new Array(p,new int[]{2,4,3},true)); //TYX ordering
		double[] xValues = new double[]{1.0,2.0,3.0};
		double[] yValues = new double[]{10.0,20.0,30.0,40.0};
		item.setXYGrid(new Array(xValues), new Array(yValues));
		double[] timeValues=new double[]{100.0,200.0};
		item.setTimes(new Array(timeValues));

		String id = item.getId();
		assertEquals("pressure", id);

		IQuantityInfo qInfo = new QuantityInfo("pressure", "N/m^2");
		item.setQuantityInfo(qInfo);
		String quantity = item.getQuantityInfo().getQuantity();
		assertEquals("pressure", quantity);
		String unit = item.getQuantityInfo().getUnit();
		assertEquals("N/m^2", unit);

		System.out.println(item.toString());

		Role role = item.getRole();
		assertEquals(IPrevExchangeItem.Role.InOut, role);

		double values[] = item.getValuesAsDoubles();
		assertEquals(2*3*4, values.length);
		assertEquals(111., values[0],0.0001);
		assertEquals(112., values[1],0.0001);
		assertEquals(243., values[23],0.0001);
		
		DummyObserver observer = new DummyObserver();
		item.addObserver(observer);
		assertEquals(false, observer.changed);

		IArray pArray = item.getArray();
		pArray.setConstant(1234.);
		item.setArray(pArray);
		values = item.getValuesAsDoubles();
		assertEquals(2*3*4, values.length);
		assertEquals(1234., values[2],0.0001);
		assertEquals(true, observer.changed);
				
		/*
		 * TODO
		 * - copy slices with copyValuesFrom
		 * - 
		 */
	}

	public static void testArrayExchangeItem() {
		
		ArrayExchangeItem item = new ArrayExchangeItem("pressure", Role.InOut);
		
		double[] p=new double[]{111.,112.,113.,121.,122.,123.,131.,132.,133.,141.,142.,143.,
				211.,212.,213.,221.,222.,223.,231.,232.,233.,241.,242.,243. };
		item.setArray(new Array(p,new int[]{2,4,3},true)); //TYX ordering
		double[] xValues = new double[]{1.0,2.0,3.0};
		double[] yValues = new double[]{10.0,20.0,30.0,40.0};
		IArrayGeometryInfo geometryInfo = new ArrayGeometryInfo(new Array(yValues), new int[]{1}, null, 
		     new Array(xValues), new int[]{2}, null, null, null, null);
		item.setGeometryInfo(geometryInfo);
		IArrayTimeInfo timeInfo =new TimeInfo("201201010000,201201020000",true);
		item.setTimeInfo(timeInfo);

		String id = item.getId();
		assertEquals("pressure", id);

		IQuantityInfo qInfo = new QuantityInfo("pressure", "N/m^2");
		item.setQuantityInfo(qInfo);
		String quantity = item.getQuantityInfo().getQuantity();
		assertEquals("pressure", quantity);
		String unit = item.getQuantityInfo().getUnit();
		assertEquals("N/m^2", unit);

		System.out.println(item.toString());

		Role role = item.getRole();
		assertEquals(IPrevExchangeItem.Role.InOut, role);

		double values[] = item.getValuesAsDoubles();
		assertEquals(2*3*4, values.length);
		assertEquals(111., values[0],0.0001);
		assertEquals(112., values[1],0.0001);
		assertEquals(243., values[23],0.0001);
		
		DummyObserver observer = new DummyObserver();
		item.addObserver(observer);
		assertEquals(false, observer.changed);

		IArray pArray = item.getArray();
		pArray.setConstant(1234.);
		item.setArray(pArray);
		values = item.getValuesAsDoubles();
		assertEquals(2*3*4, values.length);
		assertEquals(1234., values[2],0.0001);
		assertEquals(true, observer.changed);
				
		/*
		 * TODO
		 * - copy slices with copyValuesFrom
		 * - select slices of data
		 */
	}

	public static void testPointGeometry() {
		double xt=3.0;
		double yt=53.0;
		double zt=0.1;
		PointGeometryInfo p = new PointGeometryInfo(xt,yt,zt);
		
		double delta=1e-6;
		double z=p.getHeight();
		assertEquals(zt, z, delta);
		double x=p.getLongitude();
		assertEquals(xt, x, delta);
		double y=p.getLatitude();
		assertEquals(yt, y, delta);
		
		System.out.println(p.toString());
	}

}
