Release Notes
=============

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

