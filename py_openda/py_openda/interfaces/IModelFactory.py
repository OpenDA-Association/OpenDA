class IModelFactory:
    """
    Interface of a stochModelFactory

    @author Nils van Velzen
    """

    def __init__(self):
        """
        Empty constructor
        """

    def get_instance(self, noise_config, main_or_ens):
        """
        Create an instance of the stochastic Model.

        :param noise_config: dictionary as given by EnkfAlgorithm.xml for the noise configuration.
        :param main_or_ens: determines the ouput level of the model.
        :return: the stochastic Model instance.
        """
        raise NotImplemented("Function not implemented.")
