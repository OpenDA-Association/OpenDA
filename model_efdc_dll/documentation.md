# OpenDA in memory coupling with EFDC 


OpenDA is an open interface standard for (and free implementation of) a set of tools to quickly implement data-assimilation and calibration for arbitrary numerical models. OpenDA wants to stimulate the use of data-assimilation and calibration by lowering the implementation costs and enhancing the exchange of software among researchers and end-users. A model that conforms to the OpenDA standard can use all the tools that are available in OpenDA. General information on OpenDA can be found on the OpenDA website: http://www.openda.org. Technical documentation for OpenDA is avalaible at http://www.openda.org/docu/openda_2.0/doc/developers_corner.html.

## Structure of the OpenDA wrapper for EFDC

To make the EFDC model available for OpenDA a mixed Java/Fortran wrapper is written. The Fortran source together with the EFDC fortran source is compiled as a shared library (for Unix/Linux)or a dynamic linked library (for Windows). This library is called from Java using Java Native Access (JNA), for details see http://github.com/twall/jna.

```bash
/public/model_efdc_dll/java/src/org/openda/model_efdc_dll/EfdcDLL.java
/public/model_efdc_dll/java/src/org/openda/model_efdc_dll/EfdcExchangeItemType.java
/public/model_efdc_dll/java/src/org/openda/model_efdc_dll/EfdcGridExchangeItem.java
/public/model_efdc_dll/java/src/org/openda/model_efdc_dll/EfdcModelFactory.java
/public/model_efdc_dll/java/src/org/openda/model_efdc_dll/EfdcModelFactoryConfigReader.java
/public/model_efdc_dll/java/src/org/openda/model_efdc_dll/EfdcModelInstance.java
/public/model_efdc_dll/java/src/org/openda/model_efdc_dll/EfdcScalarTimeSeriesExchangeItem.java
/public/model_efdc_dll/java/src/org/openda/model_efdc_dll/GfortranFunctionMapper.java
/public/model_efdc_dll/java/src/org/openda/model_efdc_dll/IEfdcFortranNativeDLL.java
/public/model_efdc_dll/java/src/org/openda/model_efdc_dll/IntelFortranFunctionMapper.java
```

OpenDA expects from a OpenDA compliant model that it provides Java classes which implement the methods from the IModelInstance and IModelFactory interfaces (see the OpenDA technical documentation). These classes are provided in EfdcModelInstance.java and EfdcModelFactory.java. At the Java side much of these methods are delegated to the shared library. This delegation is handled by EfdcDLL.java. IEfdcFortranNativeDLL.java provides interface declarations for the functions that are available in the shared library. Each Fortran compiler uses a different name-mangling convention for function names in the compiled library. The classes GfortranFunctionMapper.java and IntelFortranFunctionMapper.java translate the function names to the correct names for the shared libraries compiled by the gfortran and intel fortran compiler. For windows the function names in the DLL are set using preprocessor directives in the Fortran source code.

To exchange data between OpenDA and EFDC we implement the methods from the IExchangeItem interface of OpenDA. For more details see the OpenDA documentation (http://www.openda.org/docu/openda_2.1/doc/getting_started.html). For information on exchange items in the EFDC dll see exchange_items.txt.

## EFDC input files

All the normal input EFDC input files are required by the shared libary. The simulation time period is set in EFDC.INP and EVENT_TOX2.INP.

```
EFDC.INP
EVENT_TOX2.INP
CELL.INP
CELLT.INP
```

## Supress output

```
C71 ISSPH NPSPH ISRSPH ISPHXY
       0      24       0       0    !SAL
       0      24       0       0    !TEM
       0      24       0       0    !DYE
       0      24       0       0    !SFL
       0      24       0       0    !TOX
       0      24       0       0    !SED
       0      24       0       0    !SND

C72 ISPPH NPPPH ISRPPH IPPHXY
        0    24      0      0


C73 ISVPH NPVPH ISRVPH IVPHXY
        0    24      0      0
```

## Restart files

| Input  | Output | 
| ------ | ---- |
| RESTART.INP | RESTART.OUT |
| RSTWD.INP |  RSTWD.OUT |
| TEMP.RST | TEMP.RSTO  |
| WQWCRST.INP | TEMP.RSTO |

## Run time period

| Template file              | EFDC file       | Keyword  |
| -------------------------- | ------------------------ | ---- |
| `EFDC_TEMPLATE.INP`            |`EFDC.INP`  | `C7` `$N_REF_PERIODS$` |
| `EFDC_TEMPLATE.INP`          | `EFDC.INP`             | `C8` `$RELATIVE_TSTART$` (`TCON` must be 86400) |
| `TOX_EVENT2_TEMPLATE.INP`    | `TOX_EVENT2.INP`       | `$TSTART$` `$TSTOP$` |          

### Logging

| File  | Content |
| ----- | ------- |
| `model.log` | Initialisation of dll, displays exchange items supported by current EFDC configuration  |
| `instance001.log` | Per instance log, logs data exchange with exchange item id for times and values, compute steps, etc. |



