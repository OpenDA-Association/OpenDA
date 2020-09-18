class IObservationDescription:
    """
    Interface of an ObservationDescription

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

    def get_exchange_items(self):
        """
        ONLY HERE FOR COMPATIBILITY WITH JAVA DO NOT IMPLEMENT
         Get the exchange items describing the measures available in the stoch. observer.
         :return All exchange items in the stoch. observer.
        """
        raise NotImplemented("Function not implemented.")

    def get_properties(self, key:str):
        """"
        Get properties (values) that correspond to a given key.

        :param Key key for which the value is asked
        :return Properties (column of data from observation descriptions).
        """
        raise NotImplemented("Function not implemented.")

    def get_property_keys(self):
        """"
        return All keys of the observation descriptions.
        """
        raise NotImplemented("Function not implemented.")

    def get_property_count(self):
        """"
        return Number of properties.
        """
        raise NotImplemented("Function not implemented.")

    def get_observation_count(self):
        """"
        return Number of observations
        """
        raise NotImplemented("Function not implemented.")

    def get_times(self):
        """"
        Get all different times in increasing order. There is at least one observation for each time.
        It is likely that observer.createSelection(time[i]) will be used to walk through the
        observations. The implementation of the stochobserver should garantee that al observations are
        returned in exactly one batch this way.
        :return Array with all uniquely different times.
"""
        raise NotImplemented("Function not implemented.")
