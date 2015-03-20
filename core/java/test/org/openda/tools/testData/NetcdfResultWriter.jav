/*
OpenDA interface for Data Assimilation.
Copyright (C) 2007  Stef Hummel / Nils van Velzen

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

package org.openda.resultwriters;

import org.openda.costa.CtaTreeVector;
import org.openda.costa.CtaVector;
import org.openda.interfaces.IInstance;
import org.openda.interfaces.ResultWriter;
import org.openda.interfaces.TreeVector;
import org.openda.interfaces.Vector;
import org.openda.utils.Instance;
import org.openda.utils.simple.SimpleMatrix;
import org.openda.utils.simple.SimpleTreeVector;
import org.openda.utils.simple.SimpleVector;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.Collection;
import java.util.HashMap;

/**
 * Result writer that produces output in a netcdf file.
 */
public class NetcdfResultWriter implements ResultWriter {

    private static final String commentPrefix = "# ";
    private PrintStream outputStream = null;
    private HashMap<String, Integer> iter = new HashMap<String, Integer>();

    // A list that contains the opened netcdf files: each variable (id),
    // needs its own netcdfile. The netcdffile should be opened before/during first writing ,
    // All netcdf files should be closed together in a finalize call.

    private HashMap<String, Integer> idlist = new HashMap<String, Integer>();    

    private String netcdfnameprefix = " ";
    private String netcdfnamevar = " ";
    private int ctafilehandle=0;

    public NetcdfResultWriter(File workingDir, String configString) {
        if ( configString.startsWith("<xml") ) {  // TODO: right prefix
            // TODO: read from config file
        } else {
            if ( configString.toLowerCase().endsWith(".nc") ) {
                String prefixname = configString.substring(0, configString.length()-3);

                netcdfnameprefix = new File(workingDir,prefixname).getAbsolutePath();
                
                // first trigger the openDAbridge by making a dummy ctavector
                Vector dumctavec = new CtaVector(1);

                // The ctanetcdfinit has been moved to the putvalue function: there,
                // the id-list will be inspected. If the variable is  'new', then
                // the init function will be called for this variable.
                
                // ctafilehandle = ctaNetcdfInit(netcdfnameprefix);

                
                // configString directly indicates the netcdf output file

            }
        }
    }

    public void putMessage(Source source, String comment) {
      //   throw new RuntimeException("NetcdfResultWriter: putMessage not implemented ");

        //comment = comment.replaceAll("\n", "\n%");
        //outputStream.println(commentPrefix + comment);
    }

    public void putMessage(IInstance source, String comment) {
       // throw new RuntimeException("NetcdfResultWriter: putMessage not implemented ");

        //comment = comment.replaceAll("\n", "\n%");
        //outputStream.println(commentPrefix + " " + Instance.identifySource(source) + " " +  comment);
    }


    public void putValue(Source source, String id, Object result) {
        putValue(source, id, result, -1);
    }

    public void putValue(IInstance source, String id, Object result) {
        putValue(source, id, result, -1);
    }

    public void putValue(IInstance source, String id, Object result, int iteration) {
        putValue(Instance.identifySource(source), id, result, iteration);
    }

    public void putValue(Source source, String id, Object result, int iteration) {
        putValue("", id, result, iteration);
    }
    public void putValue(String source, String id, Object result, int iteration) {
        String prefix = commentPrefix;
        if (!source.matches("")) {
            prefix = commentPrefix.concat(source);
        }
    	// TODO create a counter for time being; one counter per id
    	Integer currentIter =0;
    	if(this.iter.containsKey(id)){
    		currentIter = this.iter.get(id);
    	}
    	this.iter.put(id, currentIter+1);

               // Administration for opening a netcdf file:
        if (this.idlist.containsKey(id)){
             // the variable has already written before; get filehandle
             ctafilehandle = this.idlist.get(id);
        } else {
             // first time this variable occurs; first initialize the netcdffile!
            
             netcdfnamevar=netcdfnameprefix.concat(id);
             netcdfnamevar=netcdfnamevar.concat(".nc");
             ctafilehandle = ctaNetcdfInit(netcdfnamevar);
             this.idlist.put(id, ctafilehandle);

        }

        if (result instanceof CtaTreeVector) {


          int retval = ((TreeVector) result).export(ctafilehandle);
          if (retval != 0 ) {
              throw new RuntimeException("NetcdfResultWriter: putValue for CtaTreeVector not succeeded ");
          }
          }

        //MVL outputStream.print(id + (iteration>-1 ? "{"+(iteration+1)+"}" : "") + "=");
      //  outputStream.print(id + "{"+(currentIter+1)+"}	=");
        else if (result instanceof SimpleMatrix) {
            int ncols = ((SimpleMatrix) result).getNumberOfColumns();
            int nrows = ((SimpleMatrix) result).getNumberOfRows();
            Vector vec = new SimpleVector(ncols*nrows);
            for (int i = 0; i < nrows; i++) {
               for (int j = 0; j < ncols; j++) {
                 vec.setValue(j*nrows + i, ((SimpleMatrix) result).getValue(i,j));
               }
            }
            TreeVector treevec = new SimpleTreeVector("from_SimpleMatrix", (SimpleVector) vec);
            // TODO: set metainfo: ncols, nrows. The native setgrid and metainfo functions should
            // TODO  be accessed from java. If time permits.

            int retval = ((SimpleTreeVector) treevec).export(ctafilehandle);
            if (retval != 0 ) {
                 throw new RuntimeException("NetcdfResultWriter: putValue for SimpleMatrix not succeeded ");
            }


        } else if (result instanceof CtaVector) {
                 int retval = ((CtaVector) result).export(ctafilehandle);
          if (retval != 0 ) {
              throw new RuntimeException("NetcdfResultWriter: putValue for CtaVector not succeeded ");
          }



        } else if (result instanceof SimpleVector) {
             TreeVector treevec = new SimpleTreeVector("from_SimpleVector", (SimpleVector) result);
             int retval = ((SimpleTreeVector) treevec).export(ctafilehandle);
             if (retval != 0 ) {
                 throw new RuntimeException("NetcdfResultWriter: putValue for SimpleVector not succeeded ");            
             }


        } else if (result instanceof SimpleTreeVector) {

             int retval = ((SimpleTreeVector) result).export(ctafilehandle);
             if (retval != 0 ) {
                 throw new RuntimeException("NetcdfResultWriter: putValue for SimpleTreeVector not succeeded ");
             }


        } else {
            throw new RuntimeException("NetcdfResultWriter: putValue for this object not implemented ");

        }
  //        outputStream.println(";");
    }

    public void putIterationReport(IInstance source, int iteration, double cost, Vector parameters) {
    }

    public void free()
    {
       // Get list of all Keys;


        for (String key2 : idlist.keySet()){

          ctafilehandle=idlist.get(key2);
            int retval = ctaNetcdfClose(ctafilehandle);

            if (retval != 0 ) {
             throw new RuntimeException("NetcdfResultWriter: closing the netcdf file has not succeeded ");

            }
        }
        idlist.clear();





}

    public native int ctaNetcdfInit(String netcdfname);
    public native int ctaNetcdfClose(int ctafilehandle);


}
