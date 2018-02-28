/* OpenDA v2.4.3 
* Copyright (c) 2017 OpenDA Association 
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

import cli.OpenDA.DotNet.Bridge.DoublesExchangeItem;
import cli.OpenDA.DotNet.Bridge.ObservationDescriptions;
import org.openda.interfaces.*;
import org.openda.utils.Vector;

import java.io.File;
import java.util.List;

/**
 * Java wrapper around .net class for a Model Instance
 */
public class ModelInstanceN2J implements org.openda.interfaces.IModelInstance, IModelExtensions
	{
		protected cli.OpenDA.DotNet.Interfaces.IModelInstance _dotNetModelInstance;

		public ModelInstanceN2J(cli.OpenDA.DotNet.Interfaces.IModelInstance dotNetModelInstance)
		{
			_dotNetModelInstance = dotNetModelInstance;
		}

		public ModelInstanceN2J() {
		}

		public void initialize(File workingDir, String[] arguments) {
			// no action needed, taken care of by constructor
		}

		public org.openda.interfaces.IInstance getParent()
		{
			return null;
		}

		public org.openda.interfaces.ITime getTimeHorizon()
		{
			return new TimeN2J(_dotNetModelInstance.get_TimeHorizon());
		}

		public org.openda.interfaces.ITime getCurrentTime()
		{
			return new TimeN2J(_dotNetModelInstance.get_CurrentTime());
		}

		public void compute(org.openda.interfaces.ITime targetTime)
		{
			_dotNetModelInstance.Compute(new cli.OpenDA.DotNet.Bridge.Time(targetTime.getMJD()));
		}

		public IVector[] getObservedLocalization(String exchageItemID, IObservationDescriptions observationDescriptions, double distance) {

			// Check whether we are dealing with a C# implemention. No concersion is needed in that case
			cli.OpenDA.DotNet.Interfaces.IObservationDescriptions dotNetObservationDescriptions = null;
			if (observationDescriptions instanceof ObservationDescriptionsJ2N){
				dotNetObservationDescriptions=  ((ObservationDescriptionsJ2N) observationDescriptions)._dotNetObsDescr;
			}
			else {
				dotNetObservationDescriptions = translateObservationDesciptions(observationDescriptions);
			}

            /*
			List<IPrevExchangeItem> javaExchangeItems = observationDescriptions.getExchangeItems();
			String [] keys =observationDescriptions.getPropertyKeys();
            int nKeys=observationDescriptions.getPropertyCount();
			int nObs =observationDescriptions.getObservationCount();


			cli.OpenDA.DotNet.Interfaces.IExchangeItem[] dotnetExchangeItems =
					new cli.OpenDA.DotNet.Interfaces.IExchangeItem[javaExchangeItems.size()];
			for (int i = 0; i < javaExchangeItems.size(); i++) {
				IPrevExchangeItem javaExchangeItem = javaExchangeItems.get(i);
				cli.OpenDA.DotNet.Interfaces.IExchangeItem dotnetExchangeItem =
						new DoublesExchangeItem(javaExchangeItem.getId(),
								javaExchangeItem.getDescription(),
								javaExchangeItem.getRole().ordinal(), 0d);
				dotnetExchangeItem.set_Times(javaExchangeItem.getTimes());
				dotnetExchangeItem.set_Values(javaExchangeItem.getValuesAsDoubles());
				dotnetExchangeItems[i] = dotnetExchangeItem;
			}
			cli.OpenDA.DotNet.Interfaces.IObservationDescriptions dotNetObservationDescriptions;
			if (nKeys>0 && nObs>0){
				String[][] values = new String[nKeys][];
				for (int iKey=0; iKey<keys.length; iKey++){
					values[iKey] = observationDescriptions.getStringProperties(keys[iKey]);
				}
				dotNetObservationDescriptions = new ObservationDescriptions(dotnetExchangeItems,keys,values);
			}
			else {
				dotNetObservationDescriptions = new ObservationDescriptions(dotnetExchangeItems);
			}
			*/
            // Call method
			cli.OpenDA.DotNet.Interfaces.IVector[] dotNetVectors =
					_dotNetModelInstance.GetObservedLocalization(exchageItemID, dotNetObservationDescriptions, distance);
			IVector[] javaVectors = new IVector[dotNetVectors.length];
			for (int i = 0; i < dotNetVectors.length; i++) {
				javaVectors[i] = new Vector(dotNetVectors[i].get_Values());
			}
			return javaVectors;
		}

		public void announceObservedValues(IObservationDescriptions observationDescriptions) {
			// no action needed (yet), currently wrapped .net models store all their output in files
			// and therefore can always provide requested computed values at observation locations.
		}

		public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {

			// Check whether we are dealing with a C# implemention. No concersion is needed in that case
			cli.OpenDA.DotNet.Interfaces.IObservationDescriptions dotNetObservationDescriptions = null;
			if (observationDescriptions instanceof ObservationDescriptionsJ2N){
				dotNetObservationDescriptions=  ((ObservationDescriptionsJ2N) observationDescriptions)._dotNetObsDescr;
			}
			else {
				dotNetObservationDescriptions = translateObservationDesciptions(observationDescriptions);
			}

			/*


			List<IPrevExchangeItem> javaExchangeItems = observationDescriptions.getExchangeItems();
			cli.OpenDA.DotNet.Interfaces.IExchangeItem[] dotnetExchangeItems =
					new cli.OpenDA.DotNet.Interfaces.IExchangeItem[javaExchangeItems.size()];
			for (int i = 0; i < javaExchangeItems.size(); i++) {
				IPrevExchangeItem javaExchangeItem = javaExchangeItems.get(i);
				cli.OpenDA.DotNet.Interfaces.IExchangeItem dotnetExchangeItem =
						new DoublesExchangeItem(javaExchangeItem.getId(),
								javaExchangeItem.getDescription(),
								javaExchangeItem.getRole().ordinal(), 0d);
				dotnetExchangeItem.set_Times(javaExchangeItem.getTimes());
				dotnetExchangeItem.set_Values(javaExchangeItem.getValuesAsDoubles());
				dotnetExchangeItems[i] = dotnetExchangeItem;
			}

			cli.OpenDA.DotNet.Interfaces.IObservationDescriptions dotNetObservationDescriptions =
					new ObservationDescriptions(dotnetExchangeItems);
			*/
			cli.OpenDA.DotNet.Interfaces.IVector[] dotNetVectors =
					_dotNetModelInstance.GetObservedLocalization(dotNetObservationDescriptions, distance);
			IVector[] javaVectors = new IVector[dotNetVectors.length];
			for (int i = 0; i < dotNetVectors.length; i++) {
				javaVectors[i] = new Vector(dotNetVectors[i].get_Values());
			}
			return javaVectors;
        }


		public IVector getObservedValues(IObservationDescriptions observationDescriptions) {

			cli.OpenDA.DotNet.Interfaces.IObservationDescriptions dotNetObservationDescriptions = null;

			System.out.println("Nieuw spullen v1.0");

			// Check whether we are dealing with a C# implementation. No conversion is needed in that case
			if (observationDescriptions instanceof ObservationDescriptionsJ2N){
				dotNetObservationDescriptions=  ((ObservationDescriptionsJ2N) observationDescriptions)._dotNetObsDescr;
			}
			else {
				dotNetObservationDescriptions = translateObservationDesciptions(observationDescriptions);
			}
/*
				List<IPrevExchangeItem> javaExchangeItems = observationDescriptions.getExchangeItems();
				String [] keys =observationDescriptions.getPropertyKeys();
				int nKeys=observationDescriptions.getPropertyCount();
				int nObs =observationDescriptions.getObservationCount();


				cli.OpenDA.DotNet.Interfaces.IExchangeItem[] dotnetExchangeItems =
						new cli.OpenDA.DotNet.Interfaces.IExchangeItem[javaExchangeItems.size()];
				for (int i = 0; i < javaExchangeItems.size(); i++) {
					IPrevExchangeItem javaExchangeItem = javaExchangeItems.get(i);
					cli.OpenDA.DotNet.Interfaces.IExchangeItem dotnetExchangeItem =
							new DoublesExchangeItem(javaExchangeItem.getId(),
									javaExchangeItem.getDescription(),
									javaExchangeItem.getRole().ordinal(), 0d);
					dotnetExchangeItem.set_Times(javaExchangeItem.getTimes());
					dotnetExchangeItem.set_Values(javaExchangeItem.getValuesAsDoubles());
					dotnetExchangeItems[i] = dotnetExchangeItem;
				}
				if (nKeys>0 && nObs>0){
					String[][] values = new String[nKeys][];
					for (int iKey=0; iKey<keys.length; iKey++){
						values[iKey] = observationDescriptions.getStringProperties(keys[iKey]);
					}
					if (values[0].length != 0 && nObs == javaExchangeItems.size()) {
						dotNetObservationDescriptions = new ObservationDescriptions(dotnetExchangeItems,keys,values);
					}
				}
				if (dotNetObservationDescriptions == null) {
					dotNetObservationDescriptions = new ObservationDescriptions(dotnetExchangeItems);
				}
			}
			*/
			// Call method
			cli.OpenDA.DotNet.Interfaces.IVector dotNetVector =
					_dotNetModelInstance.GetObservedValues(dotNetObservationDescriptions);
			if (dotNetVector == null) {
				// dotNet model does not implement IModelExtensions.getObservedValues()
				return null;
			}
			IVector javaVector = new Vector(dotNetVector.get_Values());
			return javaVector;
		}

        public String[] getExchangeItemIDs()
		{
			return _dotNetModelInstance.get_ExchangeItemIDs();
		}

		public String[] getExchangeItemIDs(org.openda.interfaces.IPrevExchangeItem.Role role)
		{
			return _dotNetModelInstance.GetExchangeItemIDs(role.ordinal());
		}

		public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
			throw new UnsupportedOperationException("org.openda.dotnet.ModelInstanceN2J.getDataObjectExchangeItem(): Not implemented yet.");
		}

        public IPrevExchangeItem getExchangeItem(String str)
		{
			return new ExchangeItemN2J(_dotNetModelInstance.GetExchangeItem(str));
		}

		public IModelState saveInternalState()
		{
			return new ModelStateN2J(_dotNetModelInstance.SaveInternalState());
		}

		public void restoreInternalState(IModelState savedInternalState)
		{
			if (!(savedInternalState instanceof ModelStateN2J)) {
				throw new RuntimeException("Unexpected saved internal state type: " +
						savedInternalState.getClass().getName());
			}
			_dotNetModelInstance.RestoreInternalState(((ModelStateN2J) savedInternalState).getDotNetModelState());

		}

		public void releaseInternalState(IModelState savedInternalState)
		{
			if (!(savedInternalState instanceof ModelStateN2J)) {
				throw new RuntimeException("Unexpected saved internal state type: " +
						savedInternalState.getClass().getName());
			}
			_dotNetModelInstance.ReleaseInternalState(((ModelStateN2J) savedInternalState).getDotNetModelState());
		}

		public IModelState loadPersistentState(File persistentStateFile) {
			return new ModelStateN2J(_dotNetModelInstance.LoadPersistentState(persistentStateFile.getAbsolutePath()));
		}

		public java.io.File getModelRunDir()
		{
			String modelRunDirPath = _dotNetModelInstance.get_ModelRunDirPath();
			return modelRunDirPath != null ? new File(modelRunDirPath) : null;
		}

		public void finish()
		{
			_dotNetModelInstance.Finish();
		}

		public String toString()
		{
			return _dotNetModelInstance.toString();
		}

		public cli.OpenDA.DotNet.Interfaces.IModelInstance getDotNetModelInstance() {
            return _dotNetModelInstance;
        }


		/**
		 * Translate an Observation description instance into a C# instance.
		 * This approach is far from optimal and generic but for now it works
		 *
		 * @param observationDescriptions      I An observation description instance
		 *
		 */
		private cli.OpenDA.DotNet.Interfaces.IObservationDescriptions translateObservationDesciptions(IObservationDescriptions observationDescriptions){

			cli.OpenDA.DotNet.Interfaces.IObservationDescriptions dotNetObservationDescriptions = null;

			List<IPrevExchangeItem> javaExchangeItems = observationDescriptions.getExchangeItems();
			String [] keys =observationDescriptions.getPropertyKeys();
			int nKeys=observationDescriptions.getPropertyCount();
			int nObs =observationDescriptions.getObservationCount();


			cli.OpenDA.DotNet.Interfaces.IExchangeItem[] dotnetExchangeItems =
					new cli.OpenDA.DotNet.Interfaces.IExchangeItem[javaExchangeItems.size()];
			for (int i = 0; i < javaExchangeItems.size(); i++) {
				IPrevExchangeItem javaExchangeItem = javaExchangeItems.get(i);
				cli.OpenDA.DotNet.Interfaces.IExchangeItem dotnetExchangeItem =
						new DoublesExchangeItem(javaExchangeItem.getId(),
								javaExchangeItem.getDescription(),
								javaExchangeItem.getRole().ordinal(), 0d);
				dotnetExchangeItem.set_Times(javaExchangeItem.getTimes());
				dotnetExchangeItem.set_Values(javaExchangeItem.getValuesAsDoubles());
				dotnetExchangeItems[i] = dotnetExchangeItem;
			}
			if (nKeys>0 && nObs>0){
				String[][] values = new String[nKeys][];
				for (int iKey=0; iKey<keys.length; iKey++){
					values[iKey] = observationDescriptions.getStringProperties(keys[iKey]);
				}
				if (values[0].length != 0 && nObs == javaExchangeItems.size()) {
					dotNetObservationDescriptions = new ObservationDescriptions(dotnetExchangeItems,keys,values);
				}
			}
			if (dotNetObservationDescriptions == null) {
				dotNetObservationDescriptions = new ObservationDescriptions(dotnetExchangeItems);
			}
			return dotNetObservationDescriptions;
		}
    }
