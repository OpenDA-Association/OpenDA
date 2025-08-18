The OpenDA documentation can be viewed online at: https://docs.openda.org. 

# Prerequisites
Before building the documentation locally, make sure you have the following installed: 

- Python
- Sphinx and the Read the Docs theme: 
```
pip install sphinx
pip install sphinx_rtd_theme
```
- Make

# Platform-specific notes
- Windows: you may need to modify the `Makefile` by changing the `SPHINXBUILD` command to
```
SPHINXBUILD   ?= py -m sphinx.cmd.build
```
- Linux: No changes are needed, the `Makefile` can remain as is.

# Building the documentation
The source files are written in **reStructuredText (RST)** format and are located 
in the `source` folder. You can build the documentation into HTML files using
`make HTML`.

This will generate the HTML files in the `build` folder that can be viewed in a browser.
Once merged into the `master` branch, the documentation can also be inspected online. 

There is also a `read_the_docs` branch available on the documentation website.
This allows you to merge local branches into `read_the_docs` to preview how 
changes will appear online.
