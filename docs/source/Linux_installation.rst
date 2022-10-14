=======================================
OpenDA installation on a Linux platform
=======================================


With the following instructions, the OpenDA software can be installed on
a Linux machine:

-  Download the `OpenDA
   binaries <https://github.com/OpenDA-Association/OpenDA/releases>`__.
-  Download a 64-bit Java 11 installation via the package manager.
-  Extract the OpenDA distribution file to the desired location on your
   computer. Note: OpenDA does not work when it is installed on a
   location with a space in the path (like “``My Documents``”).
-  Several system variables need to be set before OpenDA can be run.

   -  The first variable that should be set is ``$OPENDADIR``. This
      variable should point to the ``bin`` directory of your OpenDA
      installation:

      ``export OPENDADIR=<path_to_openda_release>/bin``

      (make sure not adding a ``/`` after ``bin``). By adding this line
      to the ``~/.bashrc`` file, this is done automatically each time a
      shell is launched.

   -  Other local settings are configured using a script in the ``bin``
      directory. There is a default local-settings script that might
      work out of the box for your system. You can use this script by
      typing ``. $OPENDADIR/settings_local.sh linux`` (mind the ``.``).
      If that does not work, then the following steps should be
      executed:

      -  Check your hostname, using the ``hostname`` command;
      -  Copy the file ``$OPENDADIR/settings_local_base.sh`` to a new
         file named ``settings_local_<hostname>.sh`` in the same
         directory.
      -  Then edit that file: enable the relevant lines and change the
         values of the environment variables.
      -  Finally, execute this script by
         ``. $OPENDADIR/settings_local_<hostname>.sh``.

-  The OpenDA GUI can be opened by running ``oda_run.sh -gui`` in the
   folder ``<path_to_openda_release>/bin``. Optionally the path to the
   ``.oda`` file can be supplied as an argument, which will open that
   OpenDA configuration (``oda_run.sh -gui <path_to_oda_file>``).
-  Try to run an example. You can find examples in the
   ``<path_to_openda_release>/examples`` directory. An example that can
   easily be executed is found in
   ``model_example_blackbox/blackbox_example_calibration/Dud.oda``.
   After opening this file in the GUI, you can run the simulation. For
   the examples ``model_dflowfm_blackbox`` and ``model_delft3d`` in this
   folder, it is necessary to connect to existing models.
