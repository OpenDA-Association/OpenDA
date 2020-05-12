import py_openda.interfaces.IModelInstance as IModelInstance

class IStochModelInstance(IModelInstance):
    """
    Interface to specify the functions for a stochastic model.
    Level 1 methods: needs to be implemented for most basic assimilation and calibration algorithms
    Level 2 methods: step up sometimes need to be implemented
    Lever 3 methods: don't implement these unless you really need them

    Note: this Python interface contains not contains all methods of the java counterparts only the high level ones used
          by algorithms

    @author: Nils van Velzen
    """

    def __init__(self):
        """
        empty constructor
        """

    def getState(self, iDomain=None):
        """
        Level 1:
        Get the full set of state variables from a model.
        :param iDomain state of localization domain (optional)
        :return A Vector containing the values of all state variables in the model.
        """
        raise NotImplemented("Function not implemented.")

    def axpyOnState(self, alpha, vector, iDomain=None):
        """
        Level 1:
        Peform a  state variable += alpha * vector operation on each state variable in the model.
        :param alpha The alpha in state variable += alpha * vector.
        :param vector A Vector containing the values for the axpy-operation on all state variables in the model.
        :param iDomain state of localization domain (optional)
        """
        raise NotImplemented("Function not implemented.")

    def getParameters(self, iDomain=None):
        """
        Level 1:
        Get the full set of parameters from a model.
        :return A Vector containing the values of all parameters in the model.
        """
        raise NotImplemented("Function not implemented.")

    def setParameters(self, parameters):
        """
        Level 1:
        Set the full set of parameters for a model.
        :param parameters A Vector containing values for all parameters in the model.
        """
        raise NotImplemented("Function not implemented.")

    def axpyOnParameters(self, alpha, vector):
        """
        Level 1:
        Peform a <c>parameter += alpha * vector</c> operation on each parameter in the model.
        :param alpha The alpha in parameter += alpha * vector.
        :param vector   A Vector containing the values for the axpy-operation on all parameters in the model.

        """
        raise NotImplemented("Function not implemented.")

    def setAutomaticNoiseGeneration(value):
        """
        Level 1:
        Set or unset automatic generation of noise within the stochastic model
        :param value true to set and false to unset.
        """
        raise NotImplemented("Function not implemented.")

    def getObservationOperator(self):
        """
        Level 1:
        Get operator H to get model values corresponding to a number of observations
        :return an interpolation operator for this model to mach values corresponding to each observation
        """
        raise NotImplemented("Function not implemented.")

    def announceObservedValues(self, observationDescriptions):
        """
        Level 2:
        Tell model that it can expect to be asked for model values corresponding to the observations
        described. The model can make arrangement to save these values. The method compute run over a long
        interval at once, not stopping at each time with observations. This is meant to increase the performance
        especially of calibration algorithms.
        :param observationDescriptions An ObservationDescriptions object with meta data for the observations
        """
        raise NotImplemented("Function not implemented.")

    def getStateScaling(self, observationDescriptions):
        """
        Level 2:
        Get model suggested scaling for applying a Schur-product on the state
        :param observationDescriptions The observation descriptions.
        :return An array of Vectors (one per obs) containing scaling values for each obs.
        """
        raise NotImplemented("Function not implemented.")


    def getStateUncertainty(self):
        """
        Level 3:
        Get the uncertainty of the stochastic model's state at initial time
        or uncertainty of background at the current time.
        This method is not used for computing the analysis in Kalman filter type algorithms!
        :return A Stochastic Vector containing uncertainty for the state. Returning null means that
                there is no state uncertainty specified.
        """
        raise NotImplemented("Function not implemented.")

    def getParameterUncertainty(self):
        """
        Level 3:
         Get the uncenrtainty for the stochastic model's parameters.

         :return A Stochastic Vector containing the uncertainty for the parameters.
        """
        raise NotImplemented("Function not implemented.")

    def getWhiteNoiseUncertainty(self, time):
        """
        Level 3:
        Get the stochastic model's white noise.
        :param time The time stamp or time span to retreive the noise for.
        :return A Stochastic Vector containing the white noise.
        """
        raise NotImplemented("Function not implemented.")

    def isWhiteNoiseStationary(self):
        """
        Level 3:
        Boolean flag indicating WhiteNoiseUncertainty is the same for all times
        :return <code>True</code> if WhiteNoiseUncertainty is the same for all times.
        """
        raise NotImplemented("Function not implemented.")

    def getWhiteNoiseTimes(self, timeSpan):
        """
        Level 3:
        Get a set of white noise from a model for the requested timespan.
        :param timeSpan use this timespan as a selection
        :return A Vector containing the values of all white noise in the model.
        """
        raise NotImplemented("Function not implemented.")

    def getWhiteNoise(self, timeSpan):
        """
        Level 3:
        Get the full set of white noise from a model.
        @param timeSpan use this timespan as a selection
        @return A Vector containing the values of all white noise in the model.
        """
        raise NotImplemented("Function not implemented.")

    def setWhiteNoise(self, whiteNoise):
        """
        Level 3:
        Set the full set of white noise of a model.
        :param whiteNoise An array of Vectors containing white noise for some span.
        """
        raise NotImplemented("Function not implemented.")

    def axpyOnWhiteNoise(self, alpha, vector):
        """
        Level 3:
        Peform a white noise element += alpha * vector operation on each white noise element in the model.
        :param vector         A Vector containing the values for the axpy-operation on all white noise in the model.
        :param alpha The alpha in white noise element += alpha * vector.
        """
        raise NotImplemented("Function not implemented.")
