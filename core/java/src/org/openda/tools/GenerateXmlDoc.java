/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/
package org.openda.tools;

import java.io.File;

import com.altova.automation.XMLSpy.Application;
import com.altova.automation.XMLSpy.Dialogs;
import com.altova.automation.XMLSpy.Document;
import com.altova.automation.XMLSpy.Enums.SPYSchemaDocumentationFormat;
import com.altova.automation.XMLSpy.SchemaDocumentationDlg;
import com.altova.automation.libs.AutomationException;

/**
 * A simple example that starts XMLSpy COM server and performs a few operations on it.
 * Feel free to extend.
 */
public class GenerateXmlDoc
{
	public static void main(String[] args) 
	{

		System.out.println("\nStarted GenerateXmlDoc\n");

		/*
		 * parse arguments
		 */
		int filePos = 0;
		File sourceDir = new File(System.getProperty("user.dir"));
		File destDir= new File(System.getProperty("user.dir"));
		if ((args.length >0)) {
			if (args[filePos].startsWith("-s")) {
				filePos++;
				if (args.length >= filePos)
					if (args[filePos].startsWith("-d")) {
						throw new IllegalArgumentException("Incorrect argument found.\n" +
								"usage: java.exe -classpath XMLSpyAPI.jar;AltovaAutomation.jar;openda_core.jar org.openda.tools.GenerateXmlDoc [-s <SourceDir>] [-d <DestDir>]\n\n" +
								"The library AltovaAutomation.dll should be found somewhere in the library path.\n");
					}
					else {
						sourceDir = new File(args[filePos]);
						filePos++;
						if (!sourceDir.isDirectory()) {
							throw new IllegalArgumentException("Source dir " + sourceDir.getAbsolutePath() + " was not found");
						}
					}
				else {
					throw new IllegalArgumentException("Incorrect argument found.\n" +
							"usage: java.exe -classpath XMLSpyAPI.jar;AltovaAutomation.jar;openda_core.jar org.openda.tools.GenerateXmlDoc [-s <SourceDir>] [-d <DestDir>]\n\n" +
							"The library AltovaAutomation.dll should be found somewhere in the library path.\n");
				}  
			}
			else if (args[filePos].startsWith("-h")) {
				System.out.println("usage: -classpath XMLSpyAPI.jar;AltovaAutomation.jar;openda_core.jar org.openda.tools.GenerateXmlDoc [-s <SourceDir>] [-d <DestDir>]\n\n" +
								   "The library AltovaAutomation.dll should be found somewhere in the library path.\n");
				return;
			}
			if (args[filePos].startsWith("-d")) {
				filePos++;
				if (args.length >= filePos) {
					destDir = new File(args[filePos]);
					filePos++;
					if (!destDir.isDirectory()) {
						destDir.mkdir();
						if (!destDir.isDirectory()) {
							throw new IllegalArgumentException("Destination dir " + destDir.getAbsolutePath() + " is not a directory or cannot be created");
						}
						else {
							System.out.println("Created destination directory " + destDir.getAbsolutePath() + "\n");  
						}
					}
				}
				else {
					throw new IllegalArgumentException("Incorrect argument found.\n" +
							"usage: java.exe -classpath XMLSpyAPI.jar;AltovaAutomation.jar;openda_core.jar org.openda.tools.GenerateXmlDoc [-s <SourceDir>] [-d <DestDir>]\n\n" +
							"The library AltovaAutomation.dll should be found somewhere in the library path.\n");
				}  
			}
		}

		System.out.println("Source directory is " + sourceDir.getAbsolutePath() + "\n" +
				"Destination directory is " + destDir.getAbsolutePath() + "\n");  


		// an instance of the application.
		Application xmlSpy = null;

		// instead of COM error handling use Java exception mechanism.
		try 
		{

			/*
			 * Get list of XML schema files in source dir
			 */

			// Get number of .xsd files in source dir
			String files[] = sourceDir.list();
			int numXsdFiles = 0;
			for (int i = 0; i < files.length; i++) {
				int dotindex = files[i].lastIndexOf('.');
				if(dotindex>0) {
					String extension = files[i].substring(dotindex);
					if (extension.equalsIgnoreCase(".xsd")) {
						numXsdFiles++;
					}
				}
			}

			System.out.println("Number of XSD files: " + numXsdFiles + "\n");

			if (numXsdFiles>0) {
				// Get file list
				File xsdFiles[] = new File[numXsdFiles];
				int xsdFileIndex = 0;
				for (int i = 0; i < files.length; i++) {
					int dotindex = files[i].lastIndexOf('.');
					if(dotindex>0) {
						String extension = files[i].substring(dotindex);
						if (extension.equalsIgnoreCase(".xsd")) {
							xsdFiles[xsdFileIndex] = sourceDir.listFiles()[i];
							xsdFileIndex++;					
						}
					}
				}

				/*
				 * Setup XMLSpy
				 */

				// Start XMLSpy as COM server.
				xmlSpy = new Application();
				// COM servers start up invisible so we make it visible
				xmlSpy.setVisible(false);

				// Setup Standard Schema Documentation Dialog
				Dialogs dialog =xmlSpy.getDialogs();
				SchemaDocumentationDlg xml2Htmldialog =dialog.getSchemaDocumentationDlg();

				//generate a standard schema documentation setting
				xml2Htmldialog.setCreateDiagramsFolder(true);
				xml2Htmldialog.setEmbedCSSInHTML(true);
				xml2Htmldialog.setGenerateRelativeLinks(true);
				xml2Htmldialog.setMultipleOutputFiles(false);
				xml2Htmldialog.setOutputFormat(SPYSchemaDocumentationFormat.spySchemaDoc_HTML);
				xml2Htmldialog.setUseFixedDesign(true);

				// Which information to include in html
				xml2Htmldialog.includeAll(true);
				xml2Htmldialog.setIncludeIndex(false);

				// Which details to show in html
				xml2Htmldialog.allDetails(false);
				xml2Htmldialog.setShowAnnotations(true);
				xml2Htmldialog.setShowAttributes(true);
				xml2Htmldialog.setShowIdentityConstraints(true);
				xml2Htmldialog.setShowDiagram(true);

				/*
				 * Loop through all files and generate html
				 */

				for (File thisXsdFile : xsdFiles) {
					xmlSpy.getDocuments().openFile(thisXsdFile.getAbsolutePath(), false);

					Document CurrentDocument = xmlSpy.getActiveDocument();

					String destFile = destDir.getAbsolutePath()+ File.separator + thisXsdFile.getName();
					int dotindex = destFile.lastIndexOf('.');
					destFile = destFile.substring(0, dotindex+1) + "html";
					xml2Htmldialog.setOutputFile(destFile);

					xmlSpy.getActiveDocument().generateSchemaDocumentation(xml2Htmldialog);
					xmlSpy.getActiveDocument().close(true);
					System.out.println("Documentation generated for " + CurrentDocument.getName());
					CurrentDocument.close(true);
				}
				
				xmlSpy.quit();

			}
			else {
				if (args.length==0) {
					System.out.println("usage: java.exe -classpath XMLSpyAPI.jar;AltovaAutomation.jar;openda_core.jar org.openda.tools.GenerateXmlDoc [-s <SourceDir>] [-d <DestDir>]\n\n" +
							   		   "The library AltovaAutomation.dll should be found somewhere in the library path.\n");
				}
			}
		} 
		catch (AutomationException e)
		{
			e.printStackTrace();
		}
		finally
		{
			// Make sure that XMLSpy can shut down properly.
			if (xmlSpy != null) {
				xmlSpy.dispose();
			}

			System.out.println("\nDone GenerateXmlDoc \n");

		}
	}
}