# Example script performing a calibration using the scipy minumize function
# Author Nils van Velzen (VORtech)
#
# Note first start the server : oda_py4j.sh

import os
from py4j.java_gateway import JavaGateway

gateway = JavaGateway()   # connect to the JVM

def py_list_to_j_array(py_x):
    """
    Create a java double array from python list
    :param py_x: python list of doubles
    :return: java array of doubles copy of py_x
    """
    n = len (py_x)
    double_class = gateway.jvm.double
    j_x = gateway.new_array(double_class, n)

    for i in range(n):
        j_x[i]=py_x[i]

    return j_x

def j_array_to_py_list(j_x):
    """
    Create a python list from a java array
    :param j_x: java array
    :return: python list with values of j_x
    """

    n = len(j_x)
    py_x =[None]*n
    for i in range(n):
        py_x[i]=j_x[i]

    return py_x

class OpendaCalibrationLib:

    def initialize(self, working_directory, openda_template_directory, openda_run_directory, oda_file):
        """
        Initialise the python wrapper for the OpenDA calibration library
        :param directory: Working directory (where the script is executed)
        :param openda_template_directory: Directory that contains the template for the OpenDA configuration
        :param openda_run_directory: Directory for the calibration run (template will be copied to that run dir)
        :param oda_file: name of the Main OpenDA configuration file (todo: make optional. default e.g. "geolab_calib.oda")
        """
        # Initialize ..

    def opendalib_write_configuration(self, directory, template_dir, odaFile):
        """
        (step 0) Write OpenDA configuration
        directory: full path to directory where the configurations has to be generated
        template_dir: full path to directory that contains the the configurations has to be generated
        odaFile: name of the main oda configuration file
        """
        return None

    def opendalib_write_observations(directory, template_dir):
        """
        (step 1) Start OpenDA, providing the configuration
        directory: full path to directory containing the OpenDA configuration
        """
        return -1

    def opendalib_initialize(self, oda_file_path):
        """
        (step 2) Start OpenDA, providing the configuration
        odaFilePath: full path to main oda configuration file
        """
        global j_calibLib

        # Create the class
        # Translate the input string to java objects (File and String array)
        j_calibLib = gateway.jvm.openda.geolab.application.OpenDaCalibrationLibrary()
        j_odaFilePath = gateway.jvm.java.io.File(oda_file_path)
        j_calibLib.initialize(j_odaFilePath)


    def opendalib_model_set_parameter_definitions(initialParameterValues, standardDeviations):
        """
        (step 3) Feed the parameter definitions to the OpenDA internal model
        initialParameterValues: initial values of the parameters that have to be calibrated
        standardDeviations: standard deviations of the parameters ('band with' voor calibration)
        The algorithm will
        """
        j_calibLib.s()

    # (step 4) provide the parameters to the soil model and let it compute

    # (step 5) retrieve the result from the soil model and let it compute

    def opendalib_model_set_results(self, modelResults):
        """
        (step 6) Feed the model results to the OpenDA internal model
        modelResults: the results of the soil model computation for the last set of parameters
        """
        return -1

    def opendalib_algorithm_get_next_parameter_values(self):
        """
        (step 7) Check if the algoritm desires more evaluations
        returns: list of new parameter values. null if no evaluations needed any more
        """
        return -1

    def opendalib_algorithm_get_optimal_parameter_values(self):
        """
        (step 8) Get the final result
        returns: list of optimal parameter values.
        """
        return -1



def test_stub(p):
    """
    test the openda_calibration_lib
    """

    # Create an OpenDA TreeVector with parameter value
    p_new = model_params.clone()
    j_p = py_list_to_j_array(p)
    p_new.setValues(j_p)

    #Compute object function
    val = cost_funtion.evaluate(p_new, "-")

    # Debug: Some relevant output
    print ("Val="+str(val)+" p="+str(p))

    """
    Setup the OpenDA model factory, stoch observer and object function

    :return:
    """
    global cost_funtion
    global model_params

    #Initialize the model factory
    #model_input_dir = "/Users/nils/Develop/py4j/swanModel/config"
    model_input_dir = os.path.join(working_dir, 'swanModel', 'config')
    model_config_xml = "swanStochModelConfig.xml"


    model_factory = gateway.jvm.org.openda.model_swan.SwanCalibStochModelFactory()
    initialize_openda_configurable(model_factory, model_input_dir, model_config_xml)

    #Initialize stoch observer
    #observer_input_dir = "/Users/nils/Develop/py4j/stochObserver"
    observer_input_dir = model_input_dir = os.path.join(working_dir, 'stochObserver')
    observer_config_xml = "swanStochObsConfig.xml"

    observer = gateway.jvm.org.openda.observers.IoObjectStochObserver()
    initialize_openda_configurable(observer, observer_input_dir, observer_config_xml)

    #Initialize cost function org.openda.algorithms
    cost_funtion = gateway.jvm.org.openda.algorithms.SimulationKwadraticCostFunction(model_factory, observer)


    #Get initial parameter
    outputLevel = gateway.jvm.org.openda.interfaces.IStochModelFactory.OutputLevel.Debug
    model_ini = model_factory.getInstance(outputLevel)
    p = model_ini.getParameters()
    model_params= p




def main():
    global model_params
    setup()

    j_p0 = model_params.clone().getValues()
    py_p0 = j_array_to_py_list(j_p0)

    print("Done")

if __name__ == "__main__":
    test_stub()
