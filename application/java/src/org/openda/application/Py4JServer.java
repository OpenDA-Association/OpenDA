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

import org.openda.utils.generalJavaUtils.StringUtilities;
import py4j.GatewayServer;

import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * Setup a Py4JServer to couple OpenDa to Python
 * @author Nils van Velzen
 */


public class Py4JServer {
	private static final String JAVA_ADDRESS = "javaAddress";
	private static final String JAVA_PORT_NUMBER = "javaPortNumber";
	private static final String CALLBACK_CLIENT_ADDRESS = "callbackClientAddress";
	private static final String CALLBACK_CLIENT_PORT_NUMBER = "callbackClientPortNumber";
	private static final String DEFAULT_ADDRESS = "127.0.0.1";
	private static final int DEFAULT_JAVA_PORT_NUMBER = 25333;
	private static final int DEFAULT_CALLBACK_CLIENT_PORT_NUMBER = 25334;


	public static void main(String[] args) throws UnknownHostException {
		System.out.println("Starting up the Py4JServer to make OpenDA callable from Python");

		String javaAddress = DEFAULT_ADDRESS;
		int javaPortNumber = DEFAULT_JAVA_PORT_NUMBER;
		String callbackClientAddress = DEFAULT_ADDRESS;
		int callbackClientPortNumber = DEFAULT_CALLBACK_CLIENT_PORT_NUMBER;
		for (String argument : args) {
			String[] keyValue = StringUtilities.getKeyValuePair(argument);
			String key = keyValue[0];
			String value = keyValue[1];
			switch (key) {
				case JAVA_ADDRESS:
					javaAddress = value;
					continue;
				case CALLBACK_CLIENT_ADDRESS:
					callbackClientAddress = value;
					continue;
				case JAVA_PORT_NUMBER:
					javaPortNumber = Integer.parseInt(value);
					continue;
				case CALLBACK_CLIENT_PORT_NUMBER:
					callbackClientPortNumber = Integer.parseInt(value);
					continue;
				default:
					throw new RuntimeException("Unknown key " + key + ". Please specify only " + JAVA_ADDRESS + " and/or " + CALLBACK_CLIENT_ADDRESS + " as key=value pair");
			}
		}

		GatewayServer server = getGatewayServer(javaAddress, callbackClientAddress, javaPortNumber, callbackClientPortNumber);

		server.start();
	}

	private static GatewayServer getGatewayServer(String javaAddress, String callbackClientAddress, int javaPortNumber, int callbackClientPortNumber) throws UnknownHostException {
		Py4JServer app = new Py4JServer();
		// app is now the gateway.entry_point
		System.out.println("Using java address " + javaAddress);
		System.out.println("Using java port number " + javaPortNumber);
		System.out.println("Using callback client address " + callbackClientAddress);
		System.out.println("Using callback client port number " + callbackClientPortNumber);
		if (javaAddress.equals(DEFAULT_ADDRESS) && callbackClientAddress.equals(DEFAULT_ADDRESS) && javaPortNumber == DEFAULT_JAVA_PORT_NUMBER && callbackClientPortNumber == DEFAULT_CALLBACK_CLIENT_PORT_NUMBER) return new GatewayServer(app);
		return new GatewayServer.GatewayServerBuilder()
			.entryPoint(app)
			.javaPort(javaPortNumber)
			.javaAddress(InetAddress.getByName(javaAddress))
			.callbackClient(callbackClientPortNumber, InetAddress.getByName(callbackClientAddress))
			.build();
	}

}


