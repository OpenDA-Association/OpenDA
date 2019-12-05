package org.openda.externalsocket;

import java.io.*;
import java.net.*;

class SocketClient {

	private final String hostAddress;
	private int port;

	SocketClient(int port) {
		try {
			this.hostAddress = InetAddress.getLocalHost().getHostAddress();
		} catch (UnknownHostException e) {
			throw new RuntimeException(e.getMessage(), e);
		}
		this.port = port;
	}

    String sendAndReceive(String messageIn) {
		try (
			Socket echoSocket = new Socket(hostAddress, port);
			PrintWriter out = new PrintWriter(echoSocket.getOutputStream(), true)) {
			out.println(messageIn);
			System.out.println("Message sent: " + messageIn);
			InputStream inputStream = echoSocket.getInputStream();
			System.out.println("Input stream available: " + inputStream.available());
			BufferedReader in = new BufferedReader(new InputStreamReader(inputStream));
			System.out.println("Start reading line from input stream...");
			return in.readLine();
		} catch (UnknownHostException e) {
			System.err.println("Don't know about host " + hostAddress);
		} catch (IOException e) {
			System.err.println("Couldn't get I/O for the connection to " + hostAddress);
		}
		return null;
	}
}
