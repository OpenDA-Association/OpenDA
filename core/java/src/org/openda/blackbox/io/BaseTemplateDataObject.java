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
package org.openda.blackbox.io;
import org.openda.blackbox.config.*;
import org.openda.blackbox.interfaces.IKeyType;
import org.openda.exchange.TemplateKeyExchangeItem;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintWriter;

/**
 * Base Template IDataObject to replace key strings with values from valuesFile using templateConfig xml
 */
public class BaseTemplateDataObject implements IDataObject {
    protected BBTemplateConfig bbTemplateConfig;
    protected File templateConfigFile;
    protected IExchangeItem[] exchangeItems;
    private File workingDir;

    public void initialize(File workingDir, String fileName, String[] arguments) {
        // read template config file
        this.workingDir = workingDir;
        templateConfigFile = new File(workingDir, fileName);
        BBTemplateConfigReader templateConfigReader = new BBTemplateConfigReader(templateConfigFile);
        bbTemplateConfig = templateConfigReader.getBBTemplateConfig();

        //create exchange items for each key id from the list
        TemplateKeyDefinitions keyDefinitions = bbTemplateConfig.getKeyDefinitions();
        if (keyDefinitions == null || keyDefinitions.getKeyDefinitions() == null) {
            throw new RuntimeException("Unable to create exchangeItems in BaseTemplateDataObject: TemplateKeyDefintions are null \n" +
                                    " in template config file " + templateConfigFile.getAbsolutePath());
        }

        exchangeItems = new IExchangeItem[keyDefinitions.getKeyDefinitions().size()];

        int k=0;
        for (TemplateKeyDefinition keyDefinition : keyDefinitions.getKeyDefinitions().values()) {
            String keyId = keyDefinition.getId();
            IKeyType keyType = keyDefinition.getKeyType();
            exchangeItems[k++] = new TemplateKeyExchangeItem(keyId, keyType);

        }
    }

    public IPrevExchangeItem[] getExchangeItems() {
        return exchangeItems;
    }

    @Override
    public String[] getExchangeItemIDs() {
        if (exchangeItems == null) return new String[]{};
        IPrevExchangeItem[] items = getExchangeItems();
        String[] ids = new String[items.length];
        int k=0;
        for (IPrevExchangeItem exchangeItem : items) {
            ids[k++] = exchangeItem.getId();
        }
        return ids;
    }

    @Override
    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        return getExchangeItemIDs();

    }

    @Override
    public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
        int indexExchangeItem = Integer.MAX_VALUE;
        IPrevExchangeItem[] items = getExchangeItems();
        for (int i = 0; i < items.length; i++) {
            if (exchangeItemID.equals(items[i].getId())) {
                indexExchangeItem = i;
                break;
            }
        }
        if (indexExchangeItem == Integer.MAX_VALUE) {
            throw new RuntimeException("unknown exchange item: " + exchangeItemID);
        } else {
            if (!(items[indexExchangeItem] instanceof IExchangeItem)) {
                throw new RuntimeException("Found exchange item is not instance of IExchangeItem " + exchangeItemID);
            }
            return (IExchangeItem)items[indexExchangeItem];
        }

    }

    @Override
    public void finish() {
        if (exchangeItems.length == 0)
            throw new RuntimeException("No exchange items to writeValuesFile");
        //first we need to fill the valuesFile by values, loop over the exchange items.

        String valuesFileName = bbTemplateConfig.getValuesFileName();
        File valuesFile = new File(workingDir, valuesFileName);
        writeValuesFile(valuesFile.getAbsolutePath(), true);

        //writeValuesFile the targetFiles for all defined template files
        for (BBTemplateFile bbTemplateFile : bbTemplateConfig.getTemplateFiles()) {
            bbTemplateFile.generateTargetFile(workingDir);
        }
    }


   public void writeValuesFile(String filename, boolean overwriteExistingFiles) {
      File outFile = new File(filename);
      if (outFile.exists()) {
         if (overwriteExistingFiles) {
            boolean deleteSuccess = outFile.delete();
            if (!deleteSuccess) {
                throw new RuntimeException("Problem deleting file " + outFile);
            }
         }
         else {
            outFile = null; // disable writing to this file
         }
      }
      if (outFile != null) {
         try {
            FileOutputStream out = new FileOutputStream(outFile);
            writeValuesFile(new PrintWriter(out));
            out.close();
         }
         catch (Exception e) {
            throw new RuntimeException("Problem writing to file " + filename + " : " + e.getMessage());
         }
      }
}

    public void writeValuesFile(PrintWriter printer) {
        for (IPrevExchangeItem exchangeItem: exchangeItems) {
            printer.println(exchangeItem.getId() + "=" + exchangeItem.getValues());
        }

        printer.flush();
    }

    public BBTemplateConfig getBBTemplateConfig() {
        return bbTemplateConfig;
    }

    @Override
    public void initialize(File workingDir, String[] arguments) {
        String fileName = arguments[0];
        String[] remainingArguments = new String[arguments.length-1];
        System.arraycopy(arguments, 1, remainingArguments, 0, remainingArguments.length);
        initialize(workingDir, fileName, remainingArguments);

    }
}
