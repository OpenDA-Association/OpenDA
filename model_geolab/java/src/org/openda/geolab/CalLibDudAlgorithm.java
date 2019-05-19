package org.openda.geolab;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.openda.algorithms.Dud;
import org.openda.interfaces.IStochModelFactory;

public class CalLibDudAlgorithm extends Dud implements Runnable {
	@Override
	public void run() {
		try {
			this.prepare();
			while(this.hasNext()){
				this.next();
			}
			((CalLibStochModelInstance)this.stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress)).
				setAlgorithmDoneFlag(CalLibStochModelInstance.ExitStatus.DONE);
		} catch (Exception e) {
			String errorString = e.getMessage() + "\n" + ExceptionUtils.getStackTrace(e);
			System.out.println(errorString);
			((CalLibStochModelInstance)this.stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress)).
				setAlgorithmDoneFlag(CalLibStochModelInstance.ExitStatus.ERROR, errorString);
		}
	}

	String getConfigStringTemplate() {
		return DudXmlConfig;
	}

	private static final String DudXmlConfig = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
		"<DudConfig xmlns=\"http://www.openda.org\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.openda.org\n" +
		"\t\t\thttp://schemas.openda.org/algorithm/dudConfig.xsd\">\n" +
		"\t<costFunction weakParameterConstraint=\"false\" class=\"org.openda.algorithms.SimulationKwadraticCostFunction\"/>\n" +
		"\t<outerLoop maxIterations=\"outerLoop@maxIterations:50\" absTolerance=\"outerLoop@absTolerance:0.0001\" relTolerance=\"outerLoop@relTolerance:0.001\" relToleranceLinearCost=\"outerLoop@relToleranceLinearCost:0.0001\"/>\n" +
		"\t<lineSearch maxIterations=\"lineSearch@maxIterations:50\" maxRelStepSize=\"lineSearch@maxRelStepSize:3\">\n" +
		"\t\t<backTracking shorteningFactor=\"lineSearch@backTracking@shorteningFactor:0.5\" startIterationNegativeLook=\"lineSearch@backTracking@startIterationNegativeLook:3\"/>\n" +		"\t</lineSearch>\n" +
		"</DudConfig>\n";
}
