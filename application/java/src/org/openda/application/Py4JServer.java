/* MOD_V2.0
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
package org.openda.application;

import py4j.GatewayServer;

/**
 * Setup a Py4JServer to couple OpenDa to Python
 * @author Nils van Velzen
 */


public class Py4JServer {

	//public int addition(int first, int second) {
	//	System.out.println("bla");
	//	return first + second;
	//}

	public static void main(String[] args) {
		System.out.println("Starting up the Py4JServer to make OpenDA callable from Python");

		Py4JServer app = new Py4JServer();
		// app is now the gateway.entry_point
		GatewayServer server = new GatewayServer(app);
		server.start();
	}
}


