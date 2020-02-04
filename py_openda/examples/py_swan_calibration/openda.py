from py4j.java_gateway import JavaGateway
 

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
    gateway = JavaGateway()
    
    
    # Translate the input strings to java objects (File and String array)
    j_input_dir = gateway.jvm.java.io.File(input_dir)
    string_class = gateway.jvm.String
    j_arguments = gateway.new_array(string_class, 1)
    j_arguments[0] = config_xml

    # Initialize ..
    openda_configurable.initialize(j_input_dir, j_arguments)
  
    
    