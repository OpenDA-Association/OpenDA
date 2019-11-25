package org.openda.externalsocket;

import junit.framework.TestCase;

public class SocketClientServerTest extends TestCase {

	public void setUp() throws Exception {
		super.setUp();
	}

	public void tearDown() {
	}

	public void testSendAndReceive() {
		final int port = 8123;
		Runnable socketServerRunnable = new Runnable() {
			@Override
			public void run() {
				TestSocketServer socketServer = new TestSocketServer(port);
				socketServer.runAndWaitForMessage();
			}
		};
		Thread thread = new Thread(socketServerRunnable);
		thread.start();
		SocketClient socketClient = new SocketClient(port);
		String messageReceived = socketClient.sendAndReceive("Message sent");
		assertEquals("Successfully received: Message sent", messageReceived);
	}
}
