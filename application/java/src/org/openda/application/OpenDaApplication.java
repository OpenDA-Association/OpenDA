/* OpenDA v2.4.3 
* Copyright (c) 2017 OpenDA Association 
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

package org.openda.application;

import org.openda.application.gui.ApplicationScreen;
import org.openda.utils.Results;
import org.openda.utils.VersionUtils;
import org.openda.utils.performance.OdaTiming;

import javax.swing.*;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.jar.Attributes;
import java.util.jar.Manifest;

import static org.openda.costa.CtaParallel.finalizeParallelEnvironment;
import static org.openda.costa.CtaParallel.initParallelEnvironment;

/**
 * Main class for an OpenDA Application
 */
public class OpenDaApplication {

   public static void main(String[] args) {

      boolean useGui = false;
      boolean runParallel = false;

      /*
       * parse arguments
       */
      int filePos = 0;
      File workingDir = new File(System.getProperty("user.dir"));
      String fileName = "";
      if (args.length < 1) {
         // throw new IllegalArgumentException("No argument found.\n" +
         // "usage: OpenDaApplication [-gui] <fileName>\n");
         useGui = true; // use gui by default
      }
      else {
         // -gui option
         if (args[0].startsWith("-gui")) {
            useGui = true;
            filePos++;
         }
         else if (args[0].startsWith("-p")) {
            // this is a parallel run. Initialize parallel environment
            runParallel = true;
            initParallelEnvironment("parallel_config.xml");
            filePos++;
         }

         // get fileName
         if (args.length > filePos) { // there is an argument that is possibly a file
            File inputFile = new File(args[filePos]);
            if (!inputFile.exists()) {
               if (useGui) {
                  JOptionPane.showMessageDialog(null, "File " + inputFile.getAbsolutePath()
                           + " was not found.\nPlease select a different file");
               }
               else {
                  throw new IllegalArgumentException("File " + inputFile.getAbsolutePath() + " was not found");
               }
            }
            else {
               workingDir = inputFile.getParentFile();
               if (workingDir == null) {
                  workingDir = new File(System.getProperty("user.dir"));
               }
               fileName = inputFile.getName();
            }
         }
      }

      /*
       * Start gui or batch
       */
      if (useGui) {
         /*
          * start the gui
          */
         Results.setRunningInGui(true);
         new ApplicationScreen(workingDir, fileName);
      }
      else {
         /*
          * start batch run
          */
         runApplicationBatch(workingDir, fileName);
      }

      if (runParallel) {
         finalizeParallelEnvironment();
      }
   }


	public static void runApplicationBatch(File workingDir, String fileName) {
	   ApplicationRunnerSingleThreaded myRun = new ApplicationRunnerSingleThreaded();
	   myRun.initialize(workingDir, fileName);
	   myRun.runSingleThreaded();
      if (ApplicationRunner.getRunningInTest() && myRun.getStatus() == ApplicationRunner.Status.ERROR) { throw new RuntimeException(
               "Error, see exception thrown by ApplicationRunner)"); }
   }
}
