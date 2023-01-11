package org.openda.model_zero_mq;

import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IStochModelFactory;
import org.openda.utils.Results;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;

public class ZeroMqModelFactory implements IModelFactory {

	private static final Logger LOGGER = LoggerFactory.getLogger(ZeroMqModelFactory.class);
	//private static final String EXECUTABLE = "julia";
	private static final String PROTOCOL = "tcp://";
	private static final String PORT_SEPARATOR = ":";
	private String executable;
	private List<String> executableArguments;
	private String host;
	private Integer port;

	@Override
	public IModelInstance getInstance(String[] arguments, IStochModelFactory.OutputLevel outputLevel) {
		try {
			if(executable != null && !"".equals(executable)) {
				runProcess();
			}

			// Create ZeroMQClient
			ZContext context = new ZContext();
			ZMQ.Socket socket = context.createSocket(SocketType.REQ);
			StringBuilder addressBuilder = new StringBuilder();
			addressBuilder.append(PROTOCOL);
			addressBuilder.append(host);
			if(port != null) {
				addressBuilder.append(PORT_SEPARATOR);
				addressBuilder.append(port);
			}

			socket.connect(addressBuilder.toString());

			return new ZeroMqModelInstance(socket);
		} catch (Exception e) {
			LOGGER.error("failed to create instance", e);
			throw new RuntimeException(e);
		}
	}

	private void runProcess() {
		Executors.newSingleThreadExecutor().submit(() -> {
			try {
				List<String> commands = new ArrayList<>();
				commands.add(executable);
				commands.addAll(executableArguments);
				ProcessBuilder builder = new ProcessBuilder()
					.inheritIO()
					.command(commands);
				Process process = builder.start();
				process.waitFor();
			} catch (IOException ioException) {
				System.out.println("IO Exception: " + ioException.getMessage());
			} catch (InterruptedException interruptedException) {
				System.out.println("Interrupted Exception: " + interruptedException.getMessage());
			}
		});
	}

	@Override
	public void finish() {
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		// read config
		File zeroMqModelFactoryConfigFile = new File(workingDir, arguments[0]);
		if (!zeroMqModelFactoryConfigFile.exists()) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": Cannot find zeroMqModelFactoryConfig file "
				+ zeroMqModelFactoryConfigFile.getAbsolutePath());
		}

		// read model factory config.
		Results.putMessage(getClass().getSimpleName() + ": reading zeroMqModelFactory config file "
			+ zeroMqModelFactoryConfigFile.getAbsolutePath());
		ZeroMqModelFactoryConfigReader configReader = new ZeroMqModelFactoryConfigReader(zeroMqModelFactoryConfigFile);

		// extract executable, host and port
		executable = configReader.getExecutable();
		executableArguments = configReader.getExecutableArguments();
		host = configReader.getHost();
		port = configReader.getPort();

		// What todo with the next parts, are they all needed:
		/*this.forcingConfiguration = configReader.getZeroMqModelForcingConfigs();
		this.staticLimitConfiguration = configReader.getStaticLimitDataConfigs();
		this.modelStateExchangeItemInfos = configReader.getModelStateExchangeItemInfos();
		this.inputStateDir = configReader.getInputStateDir();
		this.outputStateDir = configReader.getOutputStateDir();
		this.modelMissingValue = configReader.getModelMissingValue();*/
	}
}
