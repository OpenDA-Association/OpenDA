using System;
using OpenDA.DotNet.Bridge;
using IExchangeItem=OpenDA.DotNet.Interfaces.IExchangeItem;
using IInstance=OpenDA.DotNet.Interfaces.IInstance;
using IModelState=OpenDA.DotNet.Interfaces.IModelState;
using IObservationDescriptions=OpenDA.DotNet.Interfaces.IObservationDescriptions;
using IStochModelInstance=OpenDA.DotNet.Interfaces.IStochModelInstance;
using IStochVector=OpenDA.DotNet.Interfaces.IStochVector;
using ITime=OpenDA.DotNet.Interfaces.ITime;
using IVector=OpenDA.DotNet.Interfaces.IVector;

namespace OpenDA.DotNet.SDK
{
	public class BBStochModelInstanceJ2N : IStochModelInstance
	{
		private readonly org.openda.interfaces.IStochModelInstance _javaStochModelInstance;

		public BBStochModelInstanceJ2N(org.openda.interfaces.IStochModelInstance javaStochModelInstance)
		{
			_javaStochModelInstance = javaStochModelInstance;
		}

		public IInstance GetParent()
		{
			return new InstanceJ2N(_javaStochModelInstance.getParent());
		}

		public ITime TimeHorizon
		{
			get { return new Time(
				_javaStochModelInstance.getTimeHorizon().getBeginTime().getMJD(),
				_javaStochModelInstance.getTimeHorizon().getEndTime().getMJD(),
				_javaStochModelInstance.getTimeHorizon().getStepMJD(),
				_javaStochModelInstance.getTimeHorizon().isSpan()
				); }
		}

		public ITime CurrentTime
		{
			get
			{
				return new Time(
					_javaStochModelInstance.getCurrentTime().getMJD()
					);
			}
		}

		public void Compute(ITime targetTime)
		{
			_javaStochModelInstance.compute(new org.openda.utils.Time(targetTime.MJD));
		}

		public string[] ExchangeItemIDs
		{
			get { return _javaStochModelInstance.getExchangeItemIDs(); }
		}

		public string[] GetExchangeItemIDs(int roleAsInt)
		{
			return _javaStochModelInstance.getExchangeItemIDs(UtilsJ2NAndN2J.RoleMapN2J(roleAsInt));
		}

		public IExchangeItem GetExchangeItem(string exchangeItemID)
		{
			return new ExchangeItemJ2N(_javaStochModelInstance.getExchangeItem(exchangeItemID));
		}

		public IModelState SaveInternalState()
		{
			throw new NotImplementedException();
		}

		public void RestoreInternalState(IModelState savedInternalState)
		{
			throw new NotImplementedException();
		}

		public void ReleaseInternalState(IModelState savedInternalState)
		{
			throw new NotImplementedException();
		}

		public IModelState LoadPersistentState(string algorithmStateFilePath)
		{
			throw new NotImplementedException();
		}

        public IVector[] GetObservedLocalization(IObservationDescriptions observationDescriptions, double distance)
        {
			throw new NotImplementedException();
        }

        public IVector[] GetObservedLocalization(String exchageItemID, IObservationDescriptions observationDescriptions, double distance)
        {
            throw new NotImplementedException();
        }

        public string ModelRunDirPath
		{
			get { throw new NotImplementedException(); }
		}

		public void Finish()
		{
			_javaStochModelInstance.finish();
		}

		public IVector State
		{
			get { throw new NotImplementedException(); }
		}

		public void AxpyOnState(double alpha, IVector vector)
		{
			throw new NotImplementedException();
		}

		public IVector Parameters
		{
			get { throw new NotImplementedException(); }
			set { throw new NotImplementedException(); }
		}

		public void AxpyOnParameters(double alpha, IVector vector)
		{
			throw new NotImplementedException();
		}

		public IStochVector StateUncertainty
		{
			get { throw new NotImplementedException(); }
		}

		public IStochVector ParameterUncertainty
		{
			get { throw new NotImplementedException(); }
		}

		public IStochVector[] GetWhiteNoiseUncertainty(ITime time)
		{
			throw new NotImplementedException();
		}

		public bool IsWhiteNoiseStationary
		{
			get { throw new NotImplementedException(); }
		}

		public ITime[] GetWhiteNoiseTimes(ITime timeSpan)
		{
			throw new NotImplementedException();
		}

		public IVector[] GetWhiteNoise(ITime timeSpan)
		{
			throw new NotImplementedException();
		}

		public void SetWhiteNoise(IVector[] whiteNoise)
		{
			throw new NotImplementedException();
		}

		public void AxpyOnWhiteNoise(double alpha, IVector[] vector)
		{
			throw new NotImplementedException();
		}

		public void SetAutomaticNoiseGeneration(bool value)
		{
			_javaStochModelInstance.setAutomaticNoiseGeneration(value);
		}

		public IVector GetObservedValues(IObservationDescriptions observationDescriptions)
		{
			throw new NotImplementedException();
		}

		public void AnnounceObservedValues(IObservationDescriptions observationDescriptions)
		{
			throw new NotImplementedException();
		}

		public IVector StateScaling
		{
			get { throw new NotImplementedException(); }
		}

		public IVector[] GetStateScaling(IObservationDescriptions observationDescriptions)
		{
			throw new NotImplementedException();
		}
	}
}