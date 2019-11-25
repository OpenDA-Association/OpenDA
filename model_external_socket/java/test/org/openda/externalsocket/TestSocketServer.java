package org.openda.externalsocket;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;

class TestSocketServer {

	private int portNumber;

	TestSocketServer(int portNumber) {
		this.portNumber = portNumber;
	}

    void runAndWaitForMessage() {
		try (
			ServerSocket serverSocket = new ServerSocket(portNumber);
			Socket clientSocket = serverSocket.accept();
			BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()))) {
			String inputLine;
			while ((inputLine = in.readLine()) != null) {
				PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true);
				out.println("Successfully received: " + inputLine);
			}
		} catch (IOException e) {
			System.out.println("Exception caught when trying to listen on port " + portNumber + " or listening for a connection");
			System.out.println(e.getMessage());
		}
	}
}
