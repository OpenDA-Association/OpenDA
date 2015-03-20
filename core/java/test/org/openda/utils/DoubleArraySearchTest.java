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

public class DoubleArraySearchTest extends TestCase {
	final double[] testArray = { -50., -20., -10., 0., 20. };

	public void testSearch() {
		DoubleArraySearch test = new DoubleArraySearch(testArray);
		assertEquals(3, test.search(0.));
		assertEquals(3, test.getResult());
		assertEquals(3, test.getLowerBound());
		assertEquals(3, test.getUpperBound());
		assertEquals(0., test.getPosition());
		assertFalse(test.isOutOfBounds());

		assertEquals(4, test.search(20.));
		assertEquals(4, test.getResult());
		assertEquals(4, test.getLowerBound());
		assertEquals(4, test.getUpperBound());
		assertEquals(0., test.getPosition());
		assertFalse(test.isOutOfBounds());

		assertEquals(-1, test.search(30.));
		assertEquals(-1, test.getResult());
		assertEquals(4, test.getLowerBound());
		assertEquals(-1, test.getUpperBound());
		assertEquals(0., test.getPosition());
		assertTrue(test.isOutOfBounds());

		assertEquals(1, test.search(-20.));
		assertEquals(1, test.getResult());
		assertEquals(1, test.getLowerBound());
		assertEquals(1, test.getUpperBound());
		assertEquals(0., test.getPosition());
		assertFalse(test.isOutOfBounds());

		assertEquals(1, test.search(-20.));
		assertEquals(1, test.getResult());
		assertEquals(1, test.getLowerBound());
		assertEquals(1, test.getUpperBound());
		assertEquals(0., test.getPosition());
		assertFalse(test.isOutOfBounds());

		assertEquals(0, test.search(-50.));
		assertEquals(0, test.getResult());
		assertEquals(0, test.getLowerBound());
		assertEquals(0, test.getUpperBound());
		assertEquals(0., test.getPosition());
		assertFalse(test.isOutOfBounds());

		assertEquals(-1, test.search(-60.));
		assertEquals(-1, test.getResult());
		assertEquals(-1, test.getLowerBound());
		assertEquals(0, test.getUpperBound());
		assertEquals(0., test.getPosition());
		assertTrue(test.isOutOfBounds());

		assertEquals(2, test.search(-10.));
		assertEquals(2, test.getResult());
		assertEquals(2, test.getLowerBound());
		assertEquals(2, test.getUpperBound());
		assertEquals(0., test.getPosition());
		assertFalse(test.isOutOfBounds());

		assertEquals(-1, test.search(-16.));
		assertEquals(-1, test.getResult());
		assertEquals(1, test.getLowerBound());
		assertEquals(2, test.getUpperBound());
		assertEquals(0.4, test.getPosition());
		assertFalse(test.isOutOfBounds());

		assertEquals(3, test.search(0.));
		assertEquals(3, test.getResult());
		assertEquals(3, test.getLowerBound());
		assertEquals(3, test.getUpperBound());
		assertEquals(0., test.getPosition());
		assertFalse(test.isOutOfBounds());

		assertEquals(-1, test.search(10.));
		assertEquals(-1, test.getResult());
		assertEquals(3, test.getLowerBound());
		assertEquals(4, test.getUpperBound());
		assertEquals(0.5, test.getPosition());

		assertEquals(-1, test.search(-70.));
		assertEquals(-1, test.getResult());
		assertEquals(-1, test.getLowerBound());
		assertEquals(0, test.getUpperBound());
		assertEquals(0., test.getPosition());
		assertTrue(test.isOutOfBounds());

		assertEquals(-1, test.search(70.));
		assertEquals(-1, test.getResult());
		assertEquals(4, test.getLowerBound());
		assertEquals(-1, test.getUpperBound());
		assertEquals(0., test.getPosition());
		assertTrue(test.isOutOfBounds());

		assertEquals(-1, test.search(-40.));
		assertEquals(-1, test.getResult());
		assertEquals(0, test.getLowerBound());
		assertEquals(1, test.getUpperBound());
		assertEquals(1. / 3., test.getPosition());
		assertFalse(test.isOutOfBounds());

		assertEquals(-1, test.search(15.));
		assertEquals(-1, test.getResult());
		assertEquals(3, test.getLowerBound());
		assertEquals(4, test.getUpperBound());
		assertEquals(0.75, test.getPosition());
		assertFalse(test.isOutOfBounds());
	}
}
