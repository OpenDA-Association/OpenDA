package org.openda.externalsocket;

import java.io.*;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;

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
			PrintWriter out = new PrintWriter(echoSocket.getOutputStream(), true);
			InputStream inputStream = echoSocket.getInputStream()) {
			out.println(messageIn);
			System.out.println("Message sent: " + messageIn);
			while (inputStream.available() == 0) {
				try {
					Thread.sleep(100);
				} catch (InterruptedException e) {
					System.out.println("Waiting for input stream to have data available interrupted");
				}
			}
			int availableLength = inputStream.available();
			System.out.println("Input stream available: " + availableLength);
			byte[] bytes = new byte[availableLength];
			DataInputStream dataInputStream = new DataInputStream(inputStream);
			dataInputStream.readFully(bytes);
			return new String(bytes);
		} catch (UnknownHostException e) {
			System.err.println("Don't know about host " + hostAddress);
		} catch (IOException e) {
			System.err.println("Couldn't get I/O for the connection to " + hostAddress);
		}
		return null;
	}

    void send(String messageIn) {
		try (
			Socket echoSocket = new Socket(hostAddress, port);
			PrintWriter out = new PrintWriter(echoSocket.getOutputStream(), true)) {
			out.println(messageIn);
			System.out.println("Message sent: " + messageIn);
		} catch (UnknownHostException e) {
			System.err.println("Don't know about host " + hostAddress);
		} catch (IOException e) {
			System.err.println("Couldn't get I/O for the connection to " + hostAddress);
		}
	}
}
