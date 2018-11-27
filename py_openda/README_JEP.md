

FIXME: We should use better name for the functions `get_ids` and `get_values`, which closer resemble the IDataObject interface. GEDAAN

TODO: Test on windows and update this document 

## Using Python code with OpenDA

[Jep](https://pypi.org/project/jep/) is a Java/Python package which allows Java to call Python code. Specifically, Jep opens a Python console within Java which can be used to import Python modules and use their methods. Jep returns a PyObject to Java and imports their methods. A Python class method can be called from Java which alters directly the PyObject. If a method returns some kind of Python object Jep will convert this into an equivalent Java object.

## Installing JEP

For Linux, running `pip install jep` should install everything properly in the package directory of your Python installation. 

After that you should make sure that the `jep.jar` file is available to Java by including the directory as part of the Java classpath:
```shell
CLASSPATH=/path/to/jar:$CLASSPATH
export CLASSPATH
```
Alternatively, you can add the dependency to the project in your Java IDE. 

Furthermore, Java needs to be able to access the shared libraries. This is done by setting the `LD_LIBRARY_PATH` environment variable to the Jep install directory, which again can be done from the command line or using the environment variable functionality that comes with your Java IDE. Be sure to visit the Jep [wiki](https://github.com/ninia/jep/wiki) for any help with setting up Jep.

## Using CPython extentions

If the Python code you wish to implement requires the use of a CPython extension, Jep might run into some problems. Specifically, Java may raise a JepException, stating:
```
jep.JepException: <class 'ModuleNotFoundError'>: No module named 'py4j.java_gateway'
```
These kinds of errors seem to be caused by the way some of the more complex CPython extensions use environment variables. To resolve this issue you will need to add some code to Java which allows the package to have a bit more freedom within the Jep instance. After the line
```java
conf.setRedirectOutputStreams(true);
```
include the lines:
```java
Set<String> modules = new HashSet<>();
modules.add("py4j");
conf.setSharedModules(modules);
```
Note that you can add multiple Python modules this way, if required. For this workaround to work you **have to** place the modules that are imported like this at one of the default positions given by `sys.path`, as adding directories to the path happens after this step. This example error is not the only way CPython might cause trouble, so if you get a different error as described by the [wiki](https://github.com/ninia/jep/wiki/Workarounds-for-CPython-Extensions) the workaround should still be useful.

## Implement a Python class which can be used with PythonDataObject

OpenDA uses so called data objects to read information from files. The data objects provides a list of `ExchangeItems` containing the data from the file. The data object classes have to implement the `IDataObject` interface. The `PythonDataObject` class implements this interace, but delegates the work to a Python class instead. The `PythonDataObject` class will take care of all the data type conversion, so there is no need to worry too much about the details of the Java classes. A Python class which implements a `DataObject` has the following structure:

```python
class swanDataObject:
    """
    Class for reading data from files.
    """
    def __init__(self, *args):
        """
        :param *args: First argument is the absolute file path of 
        the file to be read, others are unused.
        """

    def get_exchange_item_ids(self):
        """
        Finds the names of all observed variables found in file_name;
        first read from left to right, then from top to bottom.
        :return: list of variable names as used by the Java code.
        """

    def get_data_object_exchange_item(self, exchange_item_id):
        """
        Finds the values corresponding to the given names.
        :param test_names: list of names of the variables we are interested in.
        :return: list of desired values.
        """
```

* `__init__(self, *args)` initializes the object, where `*args[0]` is the absolute file path of the file that we wish to read, `*args[1]` is the name of the Python class and `*args[2]` is reserved for the pathname of the Python class. Arguments beyond that are not used by Java and can therefore freely be used to carry configuration info from the input XML file to Python.
TODO: we should not pass `arg[1]` and `arg[2]`
TODO: we should also pass the working dir


* `get_exchange_item_ids(self)` is a method that returns a list of strings with all the exchange item id's that you might want to make available. 
TODO: De twee zinnen die volgen zijn overbodig.
Take good care when formatting that the names are the same as the ones that Java uses. Again, when you return a list of strings the Java code will automatically turn it into a `String[]` for you, so there is no need to worry about that.

* `get_data_object_exchange_item(self, exchange_item_id)` returns the exchange item for the passed `exchange_item_id`. Note that `exchange_item_id` should be in the list returned by  `get_exchange_item_ids`. The method should return a float TODO:  or an array of floats? 

TODO: Hier moet nog wat komen te staan over waar het Python scriptje moet staan, nadat we de ongemakkelijke link in de javacode hebben opgelost. GEDAAN (Al moet we nog wel een default location verzinnen.)

## Configure the PythonDataObject to use your Python class

In OpenDA a data object can be used with the `BlackBoxStochModel` and with `IoObjectStochObserver`. 

In both case we have to configure the ioObject
```xml
<ioObject workingDirectory="WORK_DIR"
          className="org.openda.exchange.dataobjects.PythonDataObject">
  <fileName></fileName>
  <arg>myPythonDataObjectClass</arg>
  <arg>/path/to/script/</arg>
</ioObject>
```

Where `fileName` is the name of the file that you wish to read, the first argument is the name of the Python class and the second argument is the path to the directory containing the Python script. If your Python class requires you to pass more arguments they should be placed here.