class IStochObserver:
    """
    Interface of a stochObserver

    @author Nils van Velzen
    """
    def __init__(self):
        """
        Empty constructor
        """

    def create_selection(self, model_span):
        """
        Create a new observer, containing a selection of the present observer,
        based on the given time span.

        :param model_span: time span with selection.
        :return: stochastic observer containing the required selection.
        """
        raise NotImplemented("Function not implemented.")

    def get_times(self):
        """
        Get all different times in increasing order. There is at least one observation for each time.

        :return: some type of vector containing the times
        """
        raise NotImplemented("Function not implemented.")

    def get_count(self):
        """
        Total number of observations.

        :return: the number of observations.
        """
        raise NotImplemented("Function not implemented.")

    def get_observation_descriptions(self):
        """
        Get the observation descriptions.

        :return: observation descriptions which are compatible with the used model instance
        """
        raise NotImplemented("Function not implemented.")

    def get_sqrt_covariance(self):
        """
        Get the covariance matrix for the stochastic observations.

        :return: the covariance matrix as numpy array.
        """
        raise NotImplemented("Function not implemented.")


    def get_standard_deviation(self):
        """
        Get the standard deviation for each stochastic observation
        """
        raise NotImplemented("Function not implemented.")

    def get_realizations(self):
        """
        Get realization values for all observations, for one ensemble member.

        :return: the realizations.
        """
        raise NotImplemented("Function not implemented.")
