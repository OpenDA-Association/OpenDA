# Instructions for creating distribution for the OpenDA course

For use with the OpenDA course a custom OpenDA Windows distribution is created.

## Requirements

- OpenDA Win64 build
- Java JRE x64 
- Visual Studio Code portable mode

## Directory structure

Create the following directory structure:

    - course_distribution
        - openda
        - course
        - jre
        - visual studio code

## OpenDA build




## Java JRE

Download the custom Windows 64 JRE which is created for use with OpenDA from the [Teamcity Build server](https://build.deltares.nl/project.html?projectId=OpenDA_OpenjdkJreBuild_Win64&tab=projectOverview). Unzip the ` openda-jre-windows-*.zip` and copy the contents to ` course_distribution\jre`. Make sure the `jre` directory directly contains the `bin`, `conf`, etc subdirectories.

## Visual Studio Code portable mode

Visual Studio Code supports a [Portable mode](https://code.visualstudio.com/docs/editor/portable). This mode enables all data created and maintained by VS Code to live near itself, so it can be moved around across environments.

Follow the instructions provided by Microsoft on https://code.visualstudio.com/docs/editor/portable. When unzipping Visual Studio Code make sure that target location is `course_distribution\visual studio code`.

After setup install the following useful extensions (`File -> Preferences -> Extensions`):

- Log File Highlighter `emilast.logfilehighlighter`
- Rainbow CSV `mechatroner.rainbow-csv`
- Edit csv `janisdd.vscode-edit-csv`
- Data Preview `randomfractalsinc.vscode-data-preview`
- XML `redhat.vscode-xml`
- Python `ms-python.python`

For the the XML plugin a JRE is required. Configure `xml.java.home` in the Visual Studio Code `settings.json` file like
```json
{
    "xml.java.home": "..\\jre",
}
```
In this way we use the JRE that is inside the course distribution.
