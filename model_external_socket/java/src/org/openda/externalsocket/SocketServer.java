/*
* Copyright (c) 2023 OpenDA Association 
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
package org.openda.externalsocket;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;

class SocketServer {

	private int portNumber;

	SocketServer(int portNumber) {
		this.portNumber = portNumber;
	}

    void runAndWaitForMessage() {
		try (
			ServerSocket serverSocket = new ServerSocket(portNumber);
			Socket clientSocket = serverSocket.accept();
			BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()))) {
			PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true);
			String inputLine;
			while ((inputLine = in.readLine()) != null) {
				System.out.println("Server received: " + inputLine);
				String[] split = inputLine.substring(2).split(";");
				StringBuilder builder = new StringBuilder(10);
				for (int i = 0; i < split.length; i++) {
					double receivedDouble = Double.parseDouble(split[i]);
					double returnDouble = receivedDouble - (receivedDouble / (receivedDouble - i)) / 2;
					builder.append(returnDouble).append(";");
				}
				String sendMessage = builder.toString();
				System.out.println("Server sends: " + sendMessage);
				out.println(sendMessage);
			}
		} catch (IOException e) {
			System.out.println("Exception caught when trying to listen on port " + portNumber + " or listening for a connection");
			System.out.println(e.getMessage());
		}
	}
}
