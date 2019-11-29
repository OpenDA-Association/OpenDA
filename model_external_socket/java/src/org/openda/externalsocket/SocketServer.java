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
