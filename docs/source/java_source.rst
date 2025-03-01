==========================
Developing the Java source
==========================


On this page, we explain how developments in the Java source code can be
made.

The OpenDA source code can be cloned from the `OpenDA GitHub
page <https://github.com/OpenDA-Association/OpenDA>`__.

The OpenDA software consists of four main modules. The first module is
the ``core`` module, which contains the core of the OpenDA software. The
three other modules are named after the conceptual components of data
assimilation: ``models``, ``observers``, and ``algorithms``. Each of
these modules contains all programs and files related to the respective
data-assimilation component. The ``core`` module contains programs which
interface the other three modules. Modules for larger models with
concrete applications are stored separately (``model_*``).

In addition to the source code, a 64-bit `Java Development
Kit <https://docs.aws.amazon.com/corretto/latest/corretto-11-ug/downloads-list.html>`__
(at least version 11) should be installed. Linux users can easily
download JDK using the package manager, Windows users can download the
software
`here <https://docs.aws.amazon.com/corretto/latest/corretto-11-ug/downloads-list.html>`__.

Developing the Java source using IntelIJ IDEA
---------------------------------------------


This page contains information about developing the OpenDA Java source
code using `IntelliJ
IDEA <https://en.wikipedia.org/wiki/IntelliJ_IDEA>`__. A free community
version can be downloaded from the `JetBrains
website <https://www.jetbrains.com/idea/download/>`__. By opening the
project file\ ``openda.ipr`` in the main folder
``<path_to_openda_source>``, most of the settings for this project will
be set correctly.

In order to use the Java Development Kit in
IntelliJ, you can refer to it via
``File -> Project Structure -> Platform Settings -> SDKs``.

To build the source code into usable binaries,
`Ant <https://en.wikipedia.org/wiki/Apache_Ant>`__ is used which is
already present in recent versions of IntelliJ. The Ant sidebar can be
opened by heading to ``View -> Tool Windows -> Ant``.

By selecting the ``<path_to_openda_source>/build.xml`` in the Ant
plugin, a list of build targets can be run. Common practice is to run
the ``build-x64`` target in this list by selecting it and choosing
``Run Target``. By running it, the OpenDA binaries will be generated in
``bin``. These binaries can be used to run OpenDA by non-developer users
outside of IntelliJ.

If one of the XML schema files (``.xsd``) has changed, then it is
necessary to run the `Castor
framework <https://en.wikipedia.org/wiki/Castor_(framework)>`__ to
convert XML files to Java source code. More information about these
configuration files can be found in the :ref:`introduction to
OpenDA <Introduction OpenDA>`.
Castor is run by selecting the corresponding ``build_castor.xml`` target
in the Ant plugin (see, for example, ``core/build_castor.xml``), and
then executing it by choosing ``Run Target``. After this step, the
standard build target should again be run.

In each module folder (``core``, ``models``, ``model_*``, ``observers``,
and ``algorithms``), unit tests are available to determine whether that
particular part of the software is fit for use. The unit tests can be
found in the folder ``<path_to_module>/java/test/`` and run by selecting
``Run all tests`` after clicking the right-mouse button.

Developing the Java source without an IDE
-----------------------------------------


On this page, an explanation is given about the development of the Java
source without using an IDE.

To build the OpenDA software, a command-line tool called
`Ant <https://en.wikipedia.org/wiki/Apache_Ant>`__ is used. Ant is
similar to ``make``, but written in Java, such that it is portable
between different platforms. Linux users can easily install Ant using
the package manager. Windows users can download the software
`here <http://ant.apache.org/bindownload.cgi>`__. Before installing Ant,
make sure that the Java Development Kit is installed.

When Ant is installed, the complete source code can be built by simply
typing ``./ci_build.sh`` (Linux) or ``ant build`` in the main
``<path_to_openda_source>`` (they do the same). Individual components
(``core``, ``models``, ``model_*``, ``observers``, or ``algorithms``)
can be compiled using ``ant build`` in the respective module directory.

If one of the XML schema files (``.xsd``) has changed, then it is
necessary to run the `Castor
framework <https://en.wikipedia.org/wiki/Castor_(framework)>`__ to
convert XML files to Java source code. More information about these
configuration files can be found in the :ref:`introduction to
OpenDA <Introduction OpenDA>`.
Castor is run by executing ``./ci_build_castor.sh`` (Linux), or by
visiting all directories containing a ``build_castor.xml`` file and
executing ``ant -f build_castor.xml``. After this step, the standard Ant
build should again be performed.

To remove the files generated by a build, you can use ``ant clean`` on
the command line. From within a module directory, this command will
remove the ``bin``, ``build`` and ``javadoc`` directories, and the
``MANIFEST.MF`` file (this file contains meta information about the used
JDK version etc.). It does not affect other modules or the folder
``bin`` in the OpenDA main directory. Executing the command line
``ant clean`` from the OpenDA main directory will delete the folders
``bin``, ``doc``, and ``xmlSchemas`` in the main directory, as well as
remove all modules’ generated files. Note: to be able to delete files,
they cannot be in use (obviously), so close them first.

In each module folder (``core``, ``models``, ``model_*``, ``observers``,
and ``algorithms``), unit tests are available to determine whether that
particular part of the software is fit for use. The unit tests can be
found in the folder ``<path_to_module>/java/test/``. The unit tests are
run by executing an ``ant test`` script in the main OpenDA directory
(see the ``build.xml`` for different versions, the original ``ant test``
should work). Separate tests are better run using IntelIJ.
