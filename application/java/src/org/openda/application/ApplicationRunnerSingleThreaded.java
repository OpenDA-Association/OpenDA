/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/
package org.openda.application;
import org.openda.interfaces.IModelState;
import org.openda.utils.InstanceStore;
import org.openda.utils.Results;
import org.openda.utils.performance.OdaTiming;

import java.io.File;

/**
 * Single threaded version of Application Runner
 */
public class ApplicationRunnerSingleThreaded extends ApplicationRunner {

	public ApplicationRunnerSingleThreaded() {
	}

	public void initialize(File workingDir, String odaFile) {
		Results.putProgression("Application initializing");
		this.algorithm = startApplication(workingDir, odaFile);
		Results.putProgression("Application initializing finished");
	}

	// used by .Net version
	public void initializeCreateComponentsOnly(File workingDir, String odaFile) {
		Results.putProgression("Application initializing part 1");
		Results.addResultWriter(new InstanceStore(new File(workingDir, odaFile), true));
		this.algorithm = createApplicationComponents(new File(workingDir,odaFile), null);
		Results.putProgression("Application initializing part 1 finished");
	}

	// used by .Net version
	public void setStochComponents() {
		Results.putProgression("Application initializing part 2");
		algorithm.setStochComponents(stochObserver, stochModelFactory);
		stochObserver.setParent(algorithm);
		Results.putProgression("Application initializing part 2 finished");
	}

	public void runSingleThreaded() {

		OdaTiming timerRunTotal  = new OdaTiming("Algorithm Total");
		OdaTiming timerRunInit   = new OdaTiming("Init");
		OdaTiming timerRun       = new OdaTiming("TimeSteps");
		OdaTiming timerRunFinish = new OdaTiming("Finish");
		timerRunTotal.AddSubTimer(timerRunInit);
		timerRunTotal.AddSubTimer(timerRun);
		timerRunTotal.AddSubTimer(timerRunFinish);

		timerRunTotal.start();
		timerRunInit.start();
		try {
			Results.putProgression("Initializing Algorithm");
			if (doReadRestart) {
				IModelState savedInternalState = this.algorithm.loadPersistentState(restartInFile);
				this.algorithm.restoreInternalState(savedInternalState);
			} else {
				this.algorithm.prepare();
				if (doWriteRestart) {
					this.writeRestart();
				}
			}
			timerRunInit.stop();
			Results.putProgression("Algorithm initialized");
			timerRun.start();
			while (this.algorithm.hasNext()) {
				Results.putProgression("Algorithm starting next step");
				this.algorithm.next();
				if (doWriteRestart) {
					this.writeRestart();
				}
			}
			timerRun.stop();
			timerRunFinish.start();
			Results.putProgression("Algorithm Done");
		}
		catch (Exception e){
			logAlgorithmStepErrorAndFinish(e);
		} finally {
			finishApplication();
		}
		Results.putProgression("Application Done");
		Results.reset();
		timerRunFinish.stop();
		timerRunTotal.stop();
		timerRunTotal.printAll(workingDir);
	}
}
