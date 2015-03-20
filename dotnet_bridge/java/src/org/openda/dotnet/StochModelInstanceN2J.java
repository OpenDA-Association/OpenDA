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
package org.openda.dotnet;
import org.openda.interfaces.*;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;

/**
 * Java wrapper around .net class for a Stoch Model Instance
 */
public class StochModelInstanceN2J extends ModelInstanceN2J implements IStochModelInstance {

	public StochModelInstanceN2J(cli.OpenDA.DotNet.Interfaces.IStochModelInstance dotNetStochModelInstance) {
		_dotNetModelInstance = dotNetStochModelInstance;
	}

	@Override
	public IVector getState() {
		cli.OpenDA.DotNet.Interfaces.IVector dotNetState =
				((cli.OpenDA.DotNet.Interfaces.IStochModelInstance) _dotNetModelInstance).get_State();
		return new org.openda.utils.Vector(dotNetState.get_Values());
	}

	@Override
	public void axpyOnState(double alpha, IVector vector) {
		cli.OpenDA.DotNet.Bridge.Vector dotNetVector = new cli.OpenDA.DotNet.Bridge.Vector(vector.getValues());
		((cli.OpenDA.DotNet.Interfaces.IStochModelInstance) _dotNetModelInstance).AxpyOnState(alpha, dotNetVector);
	}

	@Override
	public IVector getParameters() {
		cli.OpenDA.DotNet.Interfaces.IVector dotNetParameters =
				((cli.OpenDA.DotNet.Interfaces.IStochModelInstance) _dotNetModelInstance).get_Parameters();
		return new org.openda.utils.Vector(dotNetParameters.get_Values());
	}

	@Override
	public void setParameters(IVector parameters) {
		cli.OpenDA.DotNet.Bridge.Vector dotNetVector = new cli.OpenDA.DotNet.Bridge.Vector(parameters.getValues());
		((cli.OpenDA.DotNet.Interfaces.IStochModelInstance) _dotNetModelInstance).set_Parameters(dotNetVector);
	}

	@Override
	public void axpyOnParameters(double alpha, IVector vector) {
		cli.OpenDA.DotNet.Bridge.Vector dotNetVector = new cli.OpenDA.DotNet.Bridge.Vector(vector.getValues());
		((cli.OpenDA.DotNet.Interfaces.IStochModelInstance) _dotNetModelInstance).AxpyOnParameters(alpha, dotNetVector);
	}

	@Override
	public IStochVector getStateUncertainty() {
		cli.OpenDA.DotNet.Interfaces.IStochVector dotNetStochVector =
				((cli.OpenDA.DotNet.Interfaces.IStochModelInstance) _dotNetModelInstance).get_StateUncertainty();
		return convertDotNetStochVectorToJava(dotNetStochVector);
	}

	@Override
	public IStochVector getParameterUncertainty() {
		cli.OpenDA.DotNet.Interfaces.IStochVector dotNetStochVector =
				((cli.OpenDA.DotNet.Interfaces.IStochModelInstance) _dotNetModelInstance).get_StateUncertainty();
		return convertDotNetStochVectorToJava(dotNetStochVector);
	}

	@Override
	public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
		throw new UnsupportedOperationException("org.openda.dotnet.StochModelInstanceN2J.getWhiteNoiseUncertainty(): Not implemented yet.");
	}

	@Override
	public boolean isWhiteNoiseStationary() {
		throw new UnsupportedOperationException("org.openda.dotnet.StochModelInstanceN2J.isWhiteNoiseStationary(): Not implemented yet.");
	}

	@Override
	public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
		throw new UnsupportedOperationException("org.openda.dotnet.StochModelInstanceN2J.getWhiteNoiseTimes(): Not implemented yet.");
	}

	@Override
	public IVector[] getWhiteNoise(ITime timeSpan) {
		throw new UnsupportedOperationException("org.openda.dotnet.StochModelInstanceN2J.getWhiteNoise(): Not implemented yet.");
	}

	@Override
	public void setWhiteNoise(IVector[] whiteNoise) {
		throw new UnsupportedOperationException("org.openda.dotnet.StochModelInstanceN2J.setWhiteNoise(): Not implemented yet.");
	}

	@Override
	public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
		throw new UnsupportedOperationException("org.openda.dotnet.StochModelInstanceN2J.axpyOnWhiteNoise(): Not implemented yet.");
	}

	@Override
	public void setAutomaticNoiseGeneration(boolean value) {
		((cli.OpenDA.DotNet.Interfaces.IStochModelInstance) _dotNetModelInstance).SetAutomaticNoiseGeneration(value);
	}

	@Override
	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
		if (!(observationDescriptions instanceof ObservationDescriptionsN2J)) {
			throw new RuntimeException("Unknown observationDescriptions type: " +
					observationDescriptions.getClass().getName());
		}
		cli.OpenDA.DotNet.Interfaces.IVector dotNetObservedValues =
				((cli.OpenDA.DotNet.Interfaces.IStochModelInstance) _dotNetModelInstance).GetObservedValues(
				((ObservationDescriptionsN2J) observationDescriptions).getDotNetObservationDescriptions());
		return new org.openda.utils.Vector(dotNetObservedValues.get_Values());
	}

	@Override
	public void announceObservedValues(IObservationDescriptions observationDescriptions) {
		if (!(observationDescriptions instanceof ObservationDescriptionsN2J)) {
			throw new RuntimeException("Unknown observationDescriptions type: " +
					observationDescriptions.getClass().getName());
		}
		((cli.OpenDA.DotNet.Interfaces.IStochModelInstance) _dotNetModelInstance).AnnounceObservedValues(
				((ObservationDescriptionsN2J) observationDescriptions).getDotNetObservationDescriptions());
	}

	@Override
	public IVector getStateScaling() {
		throw new UnsupportedOperationException("org.openda.dotnet.StochModelInstanceN2J.getStateScaling(): Not implemented yet.");
	}

	@Override
	public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		throw new UnsupportedOperationException("org.openda.dotnet.StochModelInstanceN2J.getStateScaling(): Not implemented yet.");
	}

	private IStochVector convertDotNetStochVectorToJava(cli.OpenDA.DotNet.Interfaces.IStochVector dotNetStochVector) {
		cli.OpenDA.DotNet.Interfaces.IVector dotNetExpectations = dotNetStochVector.get_Expectations();
		cli.OpenDA.DotNet.Interfaces.IVector dotNetStandardDeviations = dotNetStochVector.get_StandardDeviations();
		return new StochVector(dotNetExpectations.get_Values(), dotNetStandardDeviations.get_Values());
	}
}
