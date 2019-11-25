package org.openda.externalsocket;

import java.io.*;
import java.net.*;

class SocketClient {

	private int port;

	SocketClient(int port) {
		this.port = port;
	}

    String sendAndReceive(String messageIn) {
		String hostName = "127.0.0.1";
		try (
			Socket echoSocket = new Socket(hostName, port);
			PrintWriter out = new PrintWriter(echoSocket.getOutputStream(), true)) {
			out.println(messageIn);
			BufferedReader in = new BufferedReader(new InputStreamReader(echoSocket.getInputStream()));
			return in.readLine();
		} catch (UnknownHostException e) {
			System.err.println("Don't know about host " + hostName);
		} catch (IOException e) {
			System.err.println("Couldn't get I/O for the connection to " + hostName);
		}
		return null;
	}
}
