# Information for developers

Most OpenDA developers use IntelIJ as IDE (either on Linux, Windows or MacOS). IntelIJ can be downloaded [here](https://www.jetbrains.com/help/idea/installation-guide.html). 
However, you don't **need** to install IntelIJ for developing OpenDA. You may edit JAVA source code in any other editor and use [Apache Ant](https://ant.apache.org/) as build tool.

## Build and test OpenDA with Apache Ant

Prerequisites:
* Git is installed, i.e. available on the commandline as `git`
* Ant is installed, i.e. available on the commandline as `ant`.

```bash
    git clone https://github.com/OpenDA-Association/OpenDA.git <work_dir>
    cd <work_dir>
    ant build
    ant test
```
Type `ant help` for an overview of other build targets available in file [build.xml](build.xml). 

## Build and test OpenDA with IntelIJ

Prerequisites:
* Git is installed
* IntelIJ is installed
* An OpenJDK is installed, we recommend [this one](https://docs.aws.amazon.com/corretto/latest/corretto-11-ug/what-is-corretto-11.html).

Project settings for IntelIJ are stored in a project file (`*.ipr`). 
File [openda.ipr](openda.ipr) is found on the repository. For historical reasons, 
this projectfile assumes that the source code of OpenDA is in a directory called `public`.

You can directly clone into directory `public`  
```bash
    git clone https://github.com/OpenDA-Association/OpenDA.git public
```
or rename your <working_directory> to public now. 

Then start IntelIJ and open file `~\public\openda.ipr`. 

After opening the project for the first time you must set the project SDK. 
File -> Project Structure opens a new window. Under Project Settings on the left,
choose Project and set SDK: to the location on your file system where OpenJDK is
installed.

You should now be able to build the project and run and debug tests with the
dropdown menu's of IntelIJ.
