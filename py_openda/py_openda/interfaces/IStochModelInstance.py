class IStochModelInstance:
    """
    Interface of a stochModelFactory

    @author Nils van Velzen

    """

    def __init__(self):
        """
        Empty constructor
        """

    def get_time_horizon(self):
        """
        Get the computational time horizon of the model (begin and end time).

        :return: the time horizon (containing begin and end time).
        """
        raise NotImplemented("Function not implemented.")

    def get_current_time(self):
        """
        Get the model instance's current simulation time stamp.

        :return: The model's current simulation time stamp.
        """
        raise NotImplemented("Function not implemented.")

    def announce_observed_values(self, descriptions):
        """
        Tells model that it can expect to be asked for model values corresponding to the observations
        described. The model can make arrangement to save these values. The method compute run over a long
        interval at once, not stopping at each time with observations.
        This is meant to increase the performance especially of calibration algorithms.

        :param descriptions: an ObservationDescriptions object with meta data for the observations.
        :return:
        """
        raise NotImplemented("Function not implemented.")

    def compute(self, time):
        """
        Let the stochastic model instance compute to the requested target time stamp.
        This function can not be used to go back in time.

        :param time: Time to compute to.
        :return:
        """
        raise NotImplemented("Function not implemented.")

    def get_observations(self, descriptions):
        """
        Get model values corresponding to the descriptions.

        :param descriptions: An ObservationDescriptions object with meta data for the observations
        :return: python list with the model values corresponding to the descriptions
        """
        raise NotImplemented("Function not implemented.")

    def update_state(self, state_array, main_or_ens):
        """
        Update the state vector of the model.

        :param state_array: numpy array used to update the model state.
        :main_or_ens: "main" for updating the main model, "ens" for ensemble members.
        :return:
        """
        raise NotImplemented("Function not implemented.")

    def get_state(self):
        """
        Returns the state of the model.

        :return: State vector.
        """
        raise NotImplemented("Function not implemented.")
