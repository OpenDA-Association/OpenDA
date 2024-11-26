from setuptools import setup, find_packages
with open("README.txt", 'r') as f:
    long_description = f.read()
    
setup(name = 'py_openda',
      version = '0.1',
      packages = find_packages(),
      description = ['...'],
      long_description = long_description,
      install_requires = ['py4j', 'numpy', 'scipy', 'pandas', 'matplotlib', 'xmlschema'])