package org.openda.model_zero_mq;

import org.openda.blackbox.config.BBUtils;
import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IStochModelFactory;
import org.openda.utils.DistributedCounter;
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
	private static final String PROTOCOL = "tcp://";
	private static final String PORT_SEPARATOR = ":";
	private String executable;
	private List<String> executableArguments;
	private String host;
	private Integer firstPortNumber;
	private String modelConfigFile;
	private File modelTemplateDirectory;
	private File instanceDirectoryWithoutPostfix;
	private String inputStateDirectory;
	private String outputStateDirectory;
	private double missingValue;
	private final DistributedCounter currentModelInstanceNumber = new DistributedCounter(-1);
	private List<ZeroMqModelStateExchangeItemsInfo> modelStateExchangeItemInfos;
	private ArrayList<ZeroMqModelForcingConfig> staticLimitConfiguration;
	private ArrayList<ZeroMqModelForcingConfig> forcingConfiguration;

	public ZeroMqModelFactory() {
		// Needed
	}

	@Override
	public IModelInstance getInstance(String[] arguments, IStochModelFactory.OutputLevel outputLevel) {

		this.currentModelInstanceNumber.inc();
		int instanceID = this.currentModelInstanceNumber.val();

		// create a output directory for this member
		File instanceDirectory = new File(this.instanceDirectoryWithoutPostfix.getAbsolutePath() + instanceID);
		BBUtils.makeDirectoryClone(this.modelTemplateDirectory, instanceDirectory);

		try {
			if (executable != null && !"".equals(executable)) {
				runProcess();
			}

			// Create ZeroMQClient
			ZContext context = new ZContext();
			ZMQ.Socket socket = context.createSocket(SocketType.REQ);
			StringBuilder addressBuilder = new StringBuilder();
			addressBuilder.append(PROTOCOL);
			addressBuilder.append(host);
			if (firstPortNumber != null) {
				addressBuilder.append(PORT_SEPARATOR);
				addressBuilder.append(firstPortNumber + instanceID);
			}

			socket.connect(addressBuilder.toString());

			return new ZeroMqModelInstance(this.currentModelInstanceNumber.val(), socket, instanceDirectory, modelConfigFile, missingValue, forcingConfiguration, staticLimitConfiguration, modelStateExchangeItemInfos);
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
				for (String executableArgument : executableArguments) {
					String translatedArgument = "%port%".equals(executableArgument) ? String.valueOf(firstPortNumber + currentModelInstanceNumber.val()) : executableArgument;
					commands.add(translatedArgument);
				}
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
		firstPortNumber = configReader.getFirstPortNumber();
		modelConfigFile = configReader.getModelConfigFile();

		// currently unused
		modelTemplateDirectory = configReader.getModelTemplateDirectory();
		instanceDirectoryWithoutPostfix = new File(this.modelTemplateDirectory.getParentFile(), "work");
		forcingConfiguration = configReader.getZeroMqModelForcingConfigs();
		staticLimitConfiguration = configReader.getStaticLimitDataConfigs();
		modelStateExchangeItemInfos = configReader.getZeroMqModelStateExchangeItemInfos();
		inputStateDirectory = configReader.getInputStateDirectory();
		outputStateDirectory = configReader.getOutputStateDirectory();
		missingValue = configReader.getMissingValue();
	}

	public static class ZeroMqModelStateExchangeItemsInfo {
		private final String stateId;
		private final String[] modelStateExchangeItemIds;
		private final Double[] modelStateExchangeItemLowerLimits;
		private final Double[] modelStateExchangeItemUpperLimits;
		private final String[] lowerLimitExchangeItemIds;
		private final String[] upperLimitExchangeItemIds;

		public ZeroMqModelStateExchangeItemsInfo(String stateId, String[] modelStateExchangeItemIds, Double[] modelStateExchangeItemLowerLimits, Double[] modelStateExchangeItemUpperLimits, String[] lowerLimitExchangeItemIds, String[] upperLimitExchangeItemIds) {
			this.stateId = stateId;
			this.modelStateExchangeItemIds = modelStateExchangeItemIds;
			this.modelStateExchangeItemLowerLimits = modelStateExchangeItemLowerLimits;
			this.modelStateExchangeItemUpperLimits = modelStateExchangeItemUpperLimits;
			this.lowerLimitExchangeItemIds = lowerLimitExchangeItemIds;
			this.upperLimitExchangeItemIds = upperLimitExchangeItemIds;
		}

		public String[] getModelStateExchangeItemIds() {
			return modelStateExchangeItemIds;
		}

		public Double[] getModelStateExchangeItemLowerLimits() {
			return modelStateExchangeItemLowerLimits;
		}

		public Double[] getModelStateExchangeItemUpperLimits() {
			return modelStateExchangeItemUpperLimits;
		}

		public String[] getLowerLimitExchangeItemIds() {
			return lowerLimitExchangeItemIds;
		}

		public String[] getUpperLimitExchangeItemIds() {
			return upperLimitExchangeItemIds;
		}

		public String getStateId() {
			return stateId;
		}
	}
}
