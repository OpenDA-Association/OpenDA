package org.openda.externalsocket;

import junit.framework.TestCase;

public class SocketClientServerTest extends TestCase {

	public void setUp() throws Exception {
		super.setUp();
	}

	public void tearDown() {
	}

	public void testSendAndReceive() {
		final int port = 8124;
		Runnable socketServerRunnable = new Runnable() {
			@Override
			public void run() {
				SocketServer socketServer = new SocketServer(port);
				socketServer.runAndWaitForMessage();
			}
		};
		Thread thread = new Thread(socketServerRunnable);
		thread.start();
		SocketClient socketClient = new SocketClient(port);
		String messageReceived = socketClient.sendAndReceive("Z:0.1;0.2;0.3;");
		assertEquals("0.05;0.6000000000000001;1.15;", messageReceived);
	}
}
