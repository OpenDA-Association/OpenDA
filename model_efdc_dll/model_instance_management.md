# Model Instance Management

OpenDA contains a number of filters that requires multiple instances of a model to be run. The EFDC is not designed to be run for multiple instances. Therefor, the OpenDA compatible EFDC shared library implements the required functionality is added in a number of Fortran source files. 

The original EFDC files (with some small alterations) are located in `/model_efdc_dll/native/efdc_fortran_dll/original_efdc_files/`. All the additional files are located in `/model_efdc_dll/native/efdc_fortran_dll/openDA_wrapper/`.

Additional functionality can be split in the following categories:

## Communication between OpenDA (Java implementation for the EFDC model ) and EFDC shared library

Implemented in `openDA_wrapper.f90`

### Data exchange with Exchange Items 

    (for information see [exchange_items.md](./exchange_items.md))

### Model instance mananagement
  
Implemented by following functions in `openDA_wrapper.f90`

```fortran
   function get_model_instance(instance_dir) result(ret_val)
   function save_instance(instance) result(ret_val)
   function restore_instance(instance) result(ret_val)
   function store_current_instance_restart_files() result (ret_val)
   function select_instance_from_restart_files(instance) result (ret_val)
```

Related files
```bash
   model_state.f90
   model_aser_time_series.f90
   model_cser_time_series.f90
   model_pser_time_series.f90
   model_qser_time_series.f90
```

### Time bookkeeping

OpenDA asks the model for run period, which is defined in TOX_EVENT.INP and other time related quantities.

Implemented by following functions in `openDA_wrapper.f90`
```fortran
   function get_reference_year(instance) result(ret_val)
   function get_start_time(instance, start_time) result(ret_val)
   function get_end_time(instance, end_time) result(ret_val)
   function get_delta_t(delta_t) result(ret_val)
   function get_reference_period(reference_period) result(ret_val)
   function get_current_time(instance, current_time) result(ret_val)
```

## Compute (integrating over time)

Related files
```bash
   model_make_time_step.f90
```

Implemented by following functions in `openDA_wrapper.f90`
```fortran
        function compute(instance, from_time_stamp, to_time_stamp) result(ret_val)
```

## EFDC init and end subroutines (originaly part of EFDC.for)

Related files
```bash
   model_init.f90
   model_init_1.for
   model_init_2.for
   model_init_3.for
   model_end.f90
```

Implemented by following functions in `openDA_wrapper.f90`
```fortran
   function init(parent_directory, template_directory) result(ret_val)
   function finish(instance) result(ret_val)
   subroutine destroy()
```
