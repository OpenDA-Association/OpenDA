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

-  For developing the OpenDA Java source code using IntelIJ IDEA, click
   `here <https://openda-association.github.io/wiki/IntelIJ>`__
-  For developing the OpenDA Java source code without an IDE, click
   `here <https://openda-association.github.io/wiki/non_IDE>`__
