# Example script performing a calibration using the scipy minumize function
# Author Nils van Velzen (VORtech)
#
# Note first start the server : oda_py4j.sh

import os
from py4j.java_gateway import JavaGateway
from scipy.optimize import minimize


gateway = JavaGateway()   # connect to the JVM

scriptdir = os.getcwd()


model_params = None   # Model paremeter vector. Only used for fast cloning
cost_funtion = None   # OpenDA cost function


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


def initialize_openda_configurable(openda_configurable, input_dir, config_xml):
    """
    Initialise an OpenDA configurable (e.g. ModelFactory, StochObserver)

    :param openda_configurable: OpenDA object implementing IConfigurable
    :param input_dir: Working directory/input directory
    :param config_xml: Configuration file
    :return:
    """

    # Translate the input strings to java objects (File and String array)
    j_input_dir = gateway.jvm.java.io.File(input_dir)
    string_class = gateway.jvm.String
    j_arguments = gateway.new_array(string_class, 1)
    j_arguments[0] = config_xml

    # Initialize ..
    openda_configurable.initialize(j_input_dir, j_arguments)

def setup():
    """
    Setup the OpenDA model factory, stoch observer and object function

    :return:
    """
    global cost_funtion
    global model_params

    #Initialize the model factory
    #model_input_dir = "/Users/nils/Develop/py4j/swanModel/config"
    model_input_dir = os.path.join(scriptdir, 'swanModel', 'config')
    model_config_xml = "swanStochModelConfig.xml"


    model_factory = gateway.jvm.org.openda.model_swan.SwanCalibStochModelFactory()
    initialize_openda_configurable(model_factory, model_input_dir, model_config_xml)

    #Initialize stoch observer
    #observer_input_dir = "/Users/nils/Develop/py4j/stochObserver"
    observer_input_dir = model_input_dir = os.path.join(scriptdir, 'stochObserver')
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



def object_function(p):
    """
    Compute the object function for parameters p
    :param p: parameters
    :return: value object function
    """

    # Create an OpenDA TreeVector with parameter value
    p_new = model_params.clone()
    j_p = py_list_to_j_array(p)
    p_new.setValues(j_p)

    #Compute object function
    val = cost_funtion.evaluate(p_new, "-")

    # Debug: Some relevant output
    print ("Val="+str(val)+" p=["+str(p)+"]")

    return val



def main():
    global model_params
    setup()

    j_p0 = model_params.clone().getValues()
    py_p0 = j_array_to_py_list(j_p0)

    results = minimize(object_function, py_p0, method='nelder-mead', options={'xtol': 1e-5, 'disp': True})
    # results = minimize(object_function, py_p0, method='powell', options={'xtol': 1e-5, 'disp': True, 'direc': [0.1, 1.0]})


    print("Optimal value ="+str(results.x))
    print("Done")



if __name__ == "__main__":
    main()
