class IModelInstance:
    """
    Model Instance interface.
    This interface specifies the functions for a deterministic model.

    Note: this Python interface contains not contains all methods of the java counterparts only the high level ones used
          by algorithms

    @author: Nils van Velzen
    """

    def __init__(self):
        """
        empty constructor
        """

    def getTimeHorizon(self):
        """
        Get the computational time horizon of the model (begin and end time).
        :return The time horizon (containing begin and end time).
        """
        raise NotImplemented("Function not implemented.")

    def getCurrentTime(self):
        """
        Get the stochastic model instance's current simulation time stamp.
        :return The model's current simulation time stamp.
        """
        raise NotImplemented("Function not implemented.")

    def compute(self, targetTime):
        """
        Let the stochastic model instance compute to the requested target time stamp.
        This function can not be used to go back in time. Use saveState and restoreState instead.
        Use Infinity if the targetTime is unknown
        """
        raise NotImplemented("Function not implemented.")

    def getLocalizationDomains(self):
        """
        Returns the localization domain based on the observation locations.
        :return Localization domain.
        """
        raise NotImplemented("Function not implemented.")

    def getObservedLocalization(self, observationDescriptions, distance):
        """
        Returns the localization weights for each observation location.
        This method assumes that there is only one state vector.
        :param observationDescriptions observation description
        :param distance characteristic distance for Cohn's formula
        :return weight vector for each observation location.

        The size of the returned array must equal the number of observation locations in the given observationDescriptions.
        The size of each vector in the returned array must equal the size of the state vector of the implementing modelInstance.
        """
        raise NotImplemented("Function not implemented.")

    def getObservedLocalization(self, observationDescriptions, distance, iDomain):
        """
        Returns the localization weights for each observation location.
        This method assumes that there is only one state vector.
        :param observationDescriptions observation description
        :param distance characteristic distance for Cohn's formula
        :param iDomain Selection of sub-domain
        :return weight vector for each observation location.
                 The size of the returned array must equal the number of observation locations in the given observationDescriptions.
                 The size of each vector in the returned array must equal the size of the state vector of the implementing modelInstance.
        """
        raise NotImplemented("Function not implemented.")
