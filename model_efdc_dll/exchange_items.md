# Exchange Items

OpenDA exchanges data with the EFDC model through exhcange items. Exchang items are defined in `model_exchange_items.f90`. Exchange items exist for grid data and time series. For time series OpenDA communicates with the time series that are stored in the model instance memory (see `model_*ser_time_series.f90`). For grid data, exchange items access the model state for a given instancen (`model_state.f90`). 

All comunication between OpenDA and the EFDC dll is handled by `openDA_wrapper.f90`. Of importance are the following subroutines and functions.
    
checking if exchange_item is active in the EFDC configuration:

```fortran
    function supports_exchange_item(instance, exchange_item_id) result (ret_val)
```

setting/getting times for time series exchange items: 

```fortran
    function get_times_for_ei(instance, exchange_item_id, bc_index, values_count, times) result (ret_val)
    function set_times_for_ei(instance, exchange_item_id, bc_index, values_count, times) result (ret_val)
```

obtaining dimensions of exchange item:

```fortran
    function get_values_count(instance, exchange_item_id) result(ret_val)
    function get_values_count_for_location(instance, exchange_item_id, bc_index) result(ret_val)
    function get_time_series_count(instance, exchange_item_id) result(ret_val)
    function get_values_count_for_time_span(instance, exchange_item_id, bc_index, start_time, end_time) result(ret_val)
```

setting/getting data:

```fortran
    function get_values(instance, exchange_item_id, start_index, end_index, values) result(ret_val)
    function set_values(instance, exchange_item_id, start_index, end_index, values) result(ret_val)
    function get_values_for_time_span(instance, exchange_item_id, bc_index, start_time, end_time, values_count, values) result(ret_val)
    function set_values_for_time_span(instance, exchange_item_id, bc_index, start_time, end_time, values_count, values) result(ret_val)
```

Furthermore, the following subroutines are also of importance when adding an exchange item:
getting/changing/initializing the data-structure for exchange items:

```fortran
    function m_openda_wrapper_init_(parent_directory_c, template_directory_c) result (ret_val)
    function m_openda_wrapper_model_instance_(instance_dir_c) result(ret_val)
    subroutine add_instance_storage()
```


checking various properties of the exchange-item data-structure

```fortran
    function check_bc_indices(instance, exchange_item_id, bc_index, start_time, end_time, start_index, end_index) result(success)
    function check_grid_indices(instance, exchange_item_id, start_index, end_index) result(success) NOTE: only when adding a grid belonging to the exchange item.
```


obtaining dimensions of exchange item:

```fortran
    function m_openda_wrapper_get_layer_count_(instance, exchange_item_id) result(retval)
    function m_openda_wrapper_get_times_count_for_location_(instance, exchange_item_id, bc_index)
    function m_openda_wrapper_get_time_series_count_(instance, exchange_item_id) result(ret_val)
    function m_openda_wrapper_get_times_count_for_time_span_(instance, exchange_item_id, bc_index, start_time, end_time) result(ret_val)
```


setting/getting data:

```fortran
    function m_openda_wrapper_get_values_for_time_span_(instance, exchange_item_id, bc_index, start_time, end_time, values_count, values) result(ret_val)
    function m_openda_wrapper_set_values_for_time_span_(instance, exchange_item_id, bc_index, start_time, end_time, values_count, values) result(ret_val)
```

## Currently supported exchange items

In the current release the exchange items exist for the following quantaties:

Atmospheric forcings:

| Variable               | Time series Exchange item ID |
|--|--|
|Precipitation          | 101 |
|AirTemperature         | 102 |
|CloudCover             | 103 |
|GlobalRadiation        | 104 |
|AtmosphericPressure    | 105 |
|RelativeHumidity       | 106 |
|PotentialEvaporation   | 107 |

Wind forcings:

| Variable               | Time series Exchange item ID |
|--|--|
| WindSpeed               |151|
| WindDirection           |152|

Hydrodynamics:

| Variable               | Time series Exchange item ID  | Grid data Exchange item ID|
|--|--|--|
| WaterLevel             | 201         |      1201 |
| Discharge              | 301         |      1301 |
| WaterTemperature       | 401         |      1401 |

Water Quality:

| Variable               | Time series Exchange item ID  | Grid data Exchange item ID|
|--|--|--|
| AlgalCyanobacteria     | 501      |         1501 |
| AlgalDiatom            | 502      |         1502 |
| AlgalGreenAlgae        | 503      |         1503 |
| RefractoryPOCarbon     | 504      |         1504 |
| LabilePOCarbon         | 505      |         1505 |
| DissolvedOCarbon       | 506      |         1506 |
| RefractoryPOPhosphorus | 507      |         1507 |
| LabilePOPhosporus      | 508      |         1508 |
| DissolvedOPhosphorus   | 509      |         1509 |
| Phosphate              | 510      |         1510 |
| RefractoryPONitrogen   | 511      |         1511 |
| LabilePONitrogen       | 512      |         1512 |
| DissolvedONitrogen     | 513      |         1513 |
| Ammonia                | 514      |         1514 |
| Nitrate                | 515      |         1515 |
| DisolvedOxygen         | 519      |         1519 |

Toxics:

| Variable               | Time series Exchange item ID  | Grid data Exchange item ID|
|--|--|--|
|Cadmium                | 601          |     1601 |
|Copper                 | 602          |     1602 |
|Lead                   | 603          |     1603 |
|Zinc                   | 604          |     1604 |
|Benzopyrene            | 605          |     1605 |
|Pcb                    | 606          |     1606 |
|Phenol                 | 607          |     1607 |
|cisChlordane           | 608          |     1608 |
|transChlordane         | 609          |     1609 |
|Chlorbenzen            | 610          |     1610 |
|MethylParathion        | 611          |     1611 |
|PcbArchor1242          | 612          |     1612 |

Controls:

| Variable               | Time series Exchange item ID  |
|--|--|
| ControlsGateWaterLevel    | 701 |
| ControlsGateOpeningHeight | 702 |

X-species:

| Variable               | Time series Exchange item ID  | Grid data Exchange item ID|
|--|--|--|
|XSpeciesnn           |   8nn     |          18nn |

## Adding exchange items

Support for the exchange item should be added to the EFDC dll and at the OpenDA site in `/public/model_efdc_dll/java/src/org/openda/model_efdc_dll/EfdcExchangeItemType.java`. When a new exchange item needs to be added, first define an exchange item ID in `model_exchange_items.f90` and `EfdcExchangeItemType.java`.

The exchange item ID is passed when OpenDA calls one of the exchange item subroutines (see above). Support for the new exchange item has to be added to these subroutine using the case statement. When the exchange item can differ between model instances, it is important data is stored in model instance memory. Grid data is probably already stored in the state vector of each instance (see `model_state.f90`). For time series have one of the existing `model_*ser_time_series.f90` can be used as an example. Most time series in EFDC have parameter defined in the related file `*SER.INP`. If an exchange item is used to for setting the time series, a template `*SER.INP` file needs to exist in the work directory. This template file needs to consist of the required number of locations (as defined in `EFDC.INP`) and have the correct parameters. For example see the `PSER.INP` for `NPSER=2` locations:
In the `openDA_wrapper` do not forget to add use `model_*ser_time_series`


```
C ** pser.inp file, in free format across line, repeat NPSER times
C **
C ** MPSER(NS)   TCPSER(NS) TAPSER(NS) RMULADJ(NS) ADDADJ(NS)
C **
C ** TPSER(M,NS) PSER(M,NS)
C ** 
    2            86400      0          1           0 ! location one
    0.000        0.0
    730.000      0.0
    2            86400      0          1           0 ! location two
    0.000        0.0
    730.000      0.0
```