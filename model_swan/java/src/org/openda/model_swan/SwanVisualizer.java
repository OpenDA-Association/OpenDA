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

package org.openda.model_swan;

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IStochModelPostProcessor;
import org.openda.model_swan.io.SwanVisualizerConfigReader;

import java.io.File;
import java.io.IOException;


public class SwanVisualizer implements IStochModelPostProcessor {

    private File instanceDir = null;

    private SwanVisualizerConfig visualizerConfig;

    // DATools / OpenDa
    public void initialize(File instanceDir, File configDir, String[] args, boolean firstInstance) {

        if (args.length != 1) {
            throw new RuntimeException("Initialize function expects 1 argument: visualisation config");
        }
        this.instanceDir = instanceDir;

        File visualizerConfigFile = new File(configDir, args[0]);
        SwanVisualizerConfigReader configReader = new SwanVisualizerConfigReader(instanceDir, visualizerConfigFile);
        visualizerConfig = configReader.getVisualizerConfig();

        // prepare swivt output settings for the current model instance
        File figuresDir = visualizerConfig.getFiguresDir();

        if (firstInstance) {
            if (figuresDir.exists()) {
                if (!figuresDir.isDirectory()) {
                    throw new RuntimeException("Please remove the file " + figuresDir.getAbsolutePath() +
                            ", it is needed as directory for the output of the SWIVT visualization utils");
                }
                BBUtils.deleteDirectory(figuresDir);
            }
        }

        if (!figuresDir.exists()) {
            if (!figuresDir.mkdir()) {
                throw new RuntimeException("Could not create figures directory " + figuresDir.getAbsolutePath());
            }
        }

        File outputDir = visualizerConfig.getOutputDirectory();
        if (outputDir.exists()) {
            if (!outputDir.isDirectory()) {
                throw new RuntimeException("Please remove the file " + outputDir.getAbsolutePath() +
                        ", it is needed as directory for the output of the SWIVT visualization utils");
            }
        } else {
            if (!outputDir.mkdir()) {
                throw new RuntimeException("Could not create figures output directory " + outputDir.getAbsolutePath());
            }
        }
    }

    public void produceAdditionalOutput() {
        File swanVisualizerExe = visualizerConfig.getVisualizerExe();
        File swivtPresentationSettingsFile = visualizerConfig.getPresentationSettingsDirectory();
        File outputDir = visualizerConfig.getOutputDirectory();
        System.out.println("SWIVT OUT:\n\t" + swanVisualizerExe.getAbsolutePath() + "\n\t" +
                swivtPresentationSettingsFile.getAbsolutePath() + "\n\t" +
                outputDir.getAbsolutePath());
        try {
            String dirSep = File.separator;
            String[] arguments = new String[]{
                    swanVisualizerExe.getParentFile().getCanonicalPath() + dirSep,
                    swivtPresentationSettingsFile.getCanonicalPath() + dirSep,   // TODO:  + dirSep for Swivt 2.0
                    visualizerConfig.getObservationDirectory().getCanonicalPath() + dirSep,
                    visualizerConfig.getModelDirectory().getCanonicalPath() + dirSep,
                    visualizerConfig.getOutputDirectory().getCanonicalPath() + dirSep
            };
            BBUtils.runExecutable(swanVisualizerExe.getAbsolutePath(), swanVisualizerExe.getParentFile(), arguments);
        } catch (IOException e) {
            throw new RuntimeException("Could not run " + swanVisualizerExe.getAbsolutePath());
        }
    }
}