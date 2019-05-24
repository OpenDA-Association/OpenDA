# Example script performing a calibration using the scipy minumize function
# Author Nils van Velzen (VORtech)
#
# Note first start the server : oda_py4j.sh

from py4j.java_gateway import JavaGateway

# connect to the JVM
gateway = JavaGateway()


def py_list_to_j_array(py_x):
    """
    Create a java double array from python list
    :param py_x: python list of doubles
    :return: java array of doubles copy of py_x
    """
    n = len(py_x)
    double_class = gateway.jvm.double
    j_x = gateway.new_array(double_class, n)

    for i in range(n):
        j_x[i] = py_x[i]

    return j_x


def j_array_to_py_list(j_x):
    """
    Create a python list from a java array
    :param j_x: java array
    :return: python list with values of j_x
    """

    n = len(j_x)
    py_x = [None] * n
    for i in range(n):
        py_x[i] = j_x[i]

    return py_x


class OpendaCalibrationLib:

    def __init__(self, working_directory):
        """
        Initialise the python wrapper for the OpenDA calibration library
        :param working_directory: Working directory (where the calibration script is executed)
        """
        # Translate the input argument to java object (File)
        # Create the class and initialize it
        j_working_directory = gateway.jvm.java.io.File(working_directory)
        self.j_calibLib = gateway.jvm.org.openda.geolab.CalibrationLibrary()
        self.j_calibLib.initialize(j_working_directory)

    def get_algorithm_names(self):
        """
        returns: list containing the names of the available algorithms
        """
        return self.j_calibLib.getAlgorithmNames()

    def get_algorithm_setting_names(self, algorithm_name):
        """
        returns: the settings for the specified algorithm
        """
        return self.j_calibLib.getAlgorithmSettingNames(algorithm_name)

    def get_algorithm_setting_value(self, algorithm_name, setting_name):
        """
        returns: the settings for the specified algorithm
        """
        return self.j_calibLib.getAlgorithmSettingValue(algorithm_name, setting_name)

    def get_algorithm_setting_default(self, algorithm_name, setting_name):
        """
        returns: the settings for the specified algorithm
        """
        return self.j_calibLib.getAlgorithmSettingDefault(algorithm_name, setting_name)

    def set_algorithm_setting_value(self, algorithm_name, setting_name, value):
        """
        returns: the settings for the specified algorithm
        """
        self.j_calibLib.setAlgorithmSettingValue(algorithm_name, setting_name, value)

    def observer_set_observations_and_stddevs(self, obs_values, obs_stddevs):
        """
        (step 1) Start OpenDA, providing the configuration
        directory: full path to directory containing the OpenDA configuration
        """
        j_obs_values = py_list_to_j_array(obs_values)
        j_obs_stddevs = py_list_to_j_array(obs_stddevs)
        self.j_calibLib.observerSetObsAndStdDevs(j_obs_values, j_obs_stddevs)

    def model_set_parameter_definitions(self, initial_parameter_values, standard_deviations):
        """
        (step 2) Feed the parameter definitions to the OpenDA internal model
        initial_parameter_values: initial values of the parameters that have to be calibrated
        standard_deviations: standard deviations of the parameters ('band with' for calibration)
        The algorithm will
        """
        j_initial_parameter_values = py_list_to_j_array(initial_parameter_values)
        j_standard_deviations = py_list_to_j_array(standard_deviations)
        self.j_calibLib.modelSetParameterDefinitions(j_initial_parameter_values, j_standard_deviations)

    # (step 3) provide the parameters to the soil model and let it compute
    # (step 4) retrieve the result from the soil model and let it compute

    def model_set_results(self, model_results):
        """
        (step 5) Feed the model results to the OpenDA internal model
        model_results: the results of the soil model computation for the last set of parameters
        """
        j_model_results = py_list_to_j_array(model_results)
        self.j_calibLib.modelSetResults(j_model_results)

    def algorithm_get_next_parameter_values(self):
        """
        (step 6) Check if the algorithm desires more evaluations
        returns: list of new parameter values. null if no evaluations needed any more
        """
        j_next_parameter_values = self.j_calibLib.algorithmGetNextParameterValues()
        if j_next_parameter_values is not None:
            if len(j_next_parameter_values) == 0:
                message = self.j_calibLib.getErrorMessage()
                raise Exception(message)
            else:
                return j_array_to_py_list(j_next_parameter_values)
        else:
            return None

    def algorithm_get_optimal_parameter_values(self):
        """
        (step 7) Get the final result
        returns: list of optimal parameter values.
        """
        j_next_parameter_values = self.j_calibLib.algorithmGetOptimalParameterValues()
        return j_array_to_py_list(j_next_parameter_values)
