/* ================================================================
 * Deltares OpenDA components
 * ================================================================
 *
 * (C) Copyright 2008, by Deltares
 *
 * OpenDA:  www.openda.org
 *
 * Deltares:  www.deltares.nl
 *
 * ----------------------------------------------------------------
 *
 * Original Author: martin.verlaan@deltares.nl
 * Contributor(s):  stef.hummel@deltares.nl
 *
 */

package org.openda.blackbox.interfaces;

import org.openda.interfaces.IExchangeItem;

import java.io.File;

/**
 * Interface for the generic io-object. This interface is needed to give a single interface to
 * various sources of io: ascii-files, model specific binary files, databases, etc..
 */
public interface IoObjectInterface {

    /**
     * Initialize the IoObject
     * @param workingDir   Working directory
     * @param fileName The name of the file containing the data (relative to the working dir.)
     * @param arguments Additional arguments (may be null zero-length)
     */
    public void initialize(File workingDir, String fileName, String[] arguments);

    /**
     * Ask which elements can be accessed
     * @return The list of element identifiers can be accessed
     */
    public IExchangeItem[] getExchangeItems(); //

    public void finish();
}
