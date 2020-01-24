## Using the main program
The main program can be found at `enkf_swan.py`. This program can run an ensemble kalman filter simulation based on a `.oda` file along with its configuration `.xml` files. This is done by calling the function `main(input_string, observation_location=0)`, which uses the absolute file path given by `input_string` for configuration and plots the results at the location given by `observation_location`. The function also produces a tuple containing two numpy arrays with results; one from a simulation with ensemble kalman filtering and one without any data assimilation. The results are taken between the prediction step and the update step.

## Inserting your own model or observer class
If you want to use a different model factory, stochastic observer or time class for the simulation, you have to change the relevant import statement in `GenericEnsembleKalmanFilter.py` to insert the class into the algorithm. The algorithm should have no problems with using a different class, assuming that your class has the same methods as the default one and assuming it is able to communicate well enough with the other classes that you are planning on using for the algorithm. The data type of the output of some of the methods that produce vectors are generic, meaning that as long they are either a Python list, a 1D numpy array, a java array or an OpenDA IVector, the `GenericEnsembleKalmanFilter` will convert the output to something usable. If your output uses a different data type, you must convert it yourself to one of those four options. This can either be done premptively at the end of your method, or you can include the data type in the `input_to_*` functions in `py4j_utils.py`. If your data type is a java class, be sure to use the IVector for an example on how to handle class recognition with py4j. For more information on the requirements of the methods, be sure to read the documentation of the default objects found in `JObjects.py`. 

## The Time object
TODO: Time Object kan echt 3 keer beter.

## Using your own model factory/instance

When implementing a model factory, you just need a class with the following two methods:
```python
def __init__(self, config, scriptdir):
    """
    :param config: dictionary used for configuration.
    :param scriptdir: location of the main .oda file.
    """

def get_instance(self, noise_config, main_or_ens):
    """
    Create an instance of the stochastic Model.

    :param noise_config: dictionary as given by EnkfAlgorithm.xml for
    the noise configuration.
    :param main_or_ens: determines the ouput level of the model.
    :return: the stochastic Model instance.
    """
```

* `__init__(self, config, scriptdir)` initializes the object. Here `config` is the stochModelFactory element of the main `.oda` configuration file as a dictionary. `scriptdir` is the directory containing the `.oda` file.

TODO: main_or_ens even uitvogelen.

* `get_instance(self, noise_config, main_or_ens)` returns an instance of the coresponding model. `nosie_config` is a python dictionary containing model noise configuration. It has the keys `@stochInit`, `@stochParameter` and `@stochForcing`. Note that this is the only point in the algorithm where you will call the `__init__` method of your model instance, so you are free to give it whatever input you need.

A model instance requires the following methods:

```python
def get_time_horizon(self):
    """
    Get the computational time horizon of the model (begin and end time).

    :return: the time horizon (containing begin and end time).
    """

def get_current_time(self):
    """
    Get the model instance's current simulation time stamp.

    :return: The model's current simulation time stamp.
    """

def announce_observed_values(self, descriptions):
    """
    Tells model that it can expect to be asked for model values
    corresponding to the observations
    described. The model can make arrangement to save these values. The
    method compute run over a long interval at once, 
    not stopping at each time with observations.
    This is meant to increase the performance especially of calibration
    algorithms.

    :param descriptions: an ObservationDescriptions object with meta
    data for the observations.
    :return:
    """

def compute(self, time):
    """
    Let the stochastic model instance compute to the requested target time stamp.
    This function can not be used to go back in time.

    :param time: Time to compute to.
    :return:
    """

def get_observations(self, descriptions):
    """
    Get model values corresponding to the descriptions.

    :param descriptions: An ObservationDescriptions object with meta
    data for the observations
    :return: python list with the model values corresponding to the descriptions
    """

def update_state(self, state_array, main_or_ens):
    """
    Update the state vector of the model.

    :param state_array: numpy array used to update the model state.
    :main_or_ens: "main" for updating the main model, "ens" for ensemble members.
    :return:
    """
```

* `get_time_horizon(self)` should return a time object spanning the start and end of the simulation.

* `get_current_time(self)` returns the time up to which the state vector has been calculated.

* `announce_observed_values(self, descriptions)` function which can be used to make sure that the model will save the observations described by the descriptions. This function does get called by the main algorithm, but it does not have to do anything if your implementation does not need it to.

* `compute(self, time)` runs the model until the given time.

* `get_observations(self, descriptions)` returns values of the state vector that correspond to the given observation descriptions. These descriptions are given by the stoch observer, so pay special attention when trying to use them.

* `update_state(self, state_array, main_or_ens)` changes the state vector based on the input `state_array`. Note that the input is a little different depending on whether the model to be updated is an ensemble member or the main model. For an ensemblemember `state_array` should be added to the state vector, while for the main model it is supposed to replace the state vector. This is controlled by the `main_or_ens` argument, which is a string equal to either "main" or "ens".

## Using your own stoch observer
When creating your own stoch observer, the following methods are required:
```python
def __init__(self, config, scriptdir):
    """
    :param config: dictionary used for configuration.
    :param scriptdir: location of the main .oda file.
    """

def create_selection(self, model_span):
    """
    Create a new observer containing a selection of the present observer
    based on the given time span.

    :param model_span: time span with selection.
    :return: stochastic observer containing the required selection.
    """

def get_times(self):
    """
    Get all different times in increasing order. There is at least 
    one observation for each time.

    :return: some type of vector containing the times
    """

def get_count(self):
    """
    Total number of observations.

    :return: the number of observations.
    """

def get_observation_descriptions(self):
    """
    Get the observation descriptions.

    :return: observation descriptions which are compatible with the
    used model instance
    """

def get_sqrt_covariance(self):
    """
    Get the covariance matrix for the stochastic observations.

    :return: the covariance matrix as numpy array.
    """

def get_realizations(self):
    """
    Get realization values for all observations, for one ensemble member.

    :return: the realizations.
    """
```

TODO: Misschien leuk om init ook met alleen een xml file als input te doen?

* `__init__(self, config=None, scriptdir=None)` initializes the object. Here `config` is the stochObserver element of the main `.oda` configuration file as a dictionary. `scriptdir` is the directory containing the `.oda` file.

* `create_selection(self, model_span)` creates a new stoch observer, limited to the span given by the time object `model_span`. The class of the output should be the same as the class of `self`.

* `get_times(self)` lists all the times where there are observations available. The output will automatically be converted to a python list of time objects.

* `get_count(self)` simply returns an integer indicating the number of measurements available.

TODO: java observation description maken!

* `get_observation_descriptions(self)` returns the observation descriptions that the model instance can use to determine which elements of the state vector corresponds to quantities that are observed. Keep in mind that there is no utility routine for creating a java observation description from python, so for now you cannot combine a python observer with a java model.

* `get_sqrt_covariance(self)` generates an np array representing the squared covariance matrix. Note that you first have to convert the output yourself. For this you can use the utility routine `j_vector_array_to_np_array(j_sqrt)` if you have a java vector array.

* `get_realizations(self)` creates realizations of the observed values using the mean and the standarddeviation of the measurement. The output will be converted automatically.

