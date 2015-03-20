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

package org.openda.exchange;

import org.openda.blackbox.config.KeyTypeSwanRestartString;
import org.openda.blackbox.interfaces.IKeyType;

import java.io.File;

/**
 * Swan restart Exchange item for Template config usage
 */
public class SwanRestartTemplateKeyExchangeItem extends SwanTemplateKeyExchangeItem {
    private String restartFileName;
    private File restartFile;

    public SwanRestartTemplateKeyExchangeItem(String id, IKeyType keyType) {
        super(id, keyType);
    }
    @Override
    public String calculateValue() {
        if (getKeyType() instanceof KeyTypeSwanRestartString) {
            return ((KeyTypeSwanRestartString)getKeyType()).parseRestartFile(restartFile, restartFileName);
        } else {
            throw new RuntimeException(this.getClass().getName()+".calculateValue(): keyType must be of type KeyTypeSwanRestartString but is of type " + getKeyType().getClass().getName());
        }

    }

    public void setRestartFileName(String restartFileName) {
        this.restartFileName = restartFileName;
    }

    public void setRestartFile(File restartFile) {
        this.restartFile = restartFile;
    }

}
