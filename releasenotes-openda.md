Release Notes
=============

OpenDA - Version 3.1.1
----------------------

### Improvements

-   Error output stream of external process of black box actions is being written to OpenDA output
-   Improved logging and bugfix in HSPF model wrapper
-   Small fix in oda_run.sh

### New functionalities

-   Option added to SSKF to skip assimilation of observations when they differ too much from the model results

OpenDA - Version 3.1
----------------------

### Improvements

-   Localization improvements D-Flow FM and OpenDA core
-   Support for D-Flow FM _rst.nc restart files
-   Support D-Flow FM partitioning
-   Improved memory usage in OpenDA – D-Flow FM wrapper
-   Writing Kalman Gain according to CF conventions
-   D-Flow FM examples added
-   Updated model_efdc_dll
-   Documentation added and updated

### New functionalities

-   model_external_file added which can be used for integrating calibration via OpenDA in other software products

OpenDA - Version 3.0.4
----------------------

### Improvements

-   Removed obsolete Log4j library

OpenDA - Version 3.0.3
----------------------

### Improvements

-   Fix D-Flow FM simple waal kalman examples for recent D-Flow FM versions
-   Support 3D station data in DFlowFMRestartFileWrapper

OpenDA - Version 3.0.2
----------------------

### Improvements

-   Parse DFlowFM BC-files with tabs (D-FlowFM)
-   Close DFlowFM restart file after reading and halt on IO errors (D-FlowFM)
-   Fix conflicting function handle values (native model interface)
-   Fix relative directory errors for saving states on Linux (BlackBox model)

OpenDA - Version 3.0.1
----------------------

### Improvements

-   Update windows build for use with Simona.
-   Fix swapped calls in native `CTAI_Model_GetObsLocalization` function for new and old interface.
-   Disable debug output in native core.
-   Documentation updates.

OpenDA - Version 3.0.0
----------------------

### New features

-   Added `external_socket_model` to send and receive results and
    parameters over an external socket instead of letting OpenDA run the
    model itself.

### Improvements

-   Removed old `IPrevExchangeItem` interface and replaced all usages with
    IExchangeItem. This is a non backwards compatible change, third
    party classes which implement `IPrevExchangeItem` should be switched
    to IExchangeItem as well.

-   Occurrences of `IoObjectInterface` replaced by newer better
    maintainable `IDataObject` & `AbstractDataObject`

-   `IoObject` element in xml made deprecated and added `DataObject` as
    replacement.

-   Many netcdf related improvements, in handling different data
    structures, performance
