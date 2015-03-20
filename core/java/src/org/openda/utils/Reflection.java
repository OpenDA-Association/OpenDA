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
package org.openda.utils;
import java.io.File;
import java.io.IOException;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public class Reflection {

	String filterList[] = new String[]{"org.openda","nl.deltares"};
	Set<String> classCache = new HashSet<String>(); 

	/**
	 * Consider all classes in jar and zip files and in directories on the java classpath
	 * and select only those that match the filters in the list e.g. {"org.apache","org.junit"}
	 * @param filterList Specification of the name spaces to be checked.
	 */
	public Reflection(String filterList[]){
		this.filterList = filterList;
	}

	/**
	 * Consider all classes in jar and zip files and in directories on the java classpath
	 * and select only those that match the default filter, ie. org.openda and nl.deltares
	 */
	public Reflection(){
		//Nothing to do yet
	}

	/**
	 * Get all class names.
	 * @return Set with class names
	 */
	public Set<String> getClassNames(){
		refreshCache();
		return this.classCache;
	}

	/**
	 * Get all class names that implement some interface.
	 * @param interfaceName Name of interface eg. org.openda.blackbox.interfaces.IoObjectInterface
	 * @return Set with class names
	 */
	public Set<String> getClassNames(String interfaceName){
		refreshCache();
		Set<String> result = new HashSet<String>();
		Class iface;
		try {
			iface = Class.forName(interfaceName);
		} catch (ClassNotFoundException e1) {
			throw new RuntimeException("Could not find class :"+interfaceName);
		}
		if(!iface.isInterface()){
			throw new RuntimeException("This is not an interface :"+interfaceName);
		}
		for(String className : this.classCache){
			Class aClass;
			try {
				if(!className.contains("Cta") && !className.contains("NetcdfResultWriter")){
					aClass = Class.forName(className);
					boolean test = iface.isAssignableFrom(aClass); //MVL
					if(test){
						//System.out.println(className+" implements "+interfaceName);
						if(!aClass.isInterface()){
							result.add(className);
						}
					}
				}
			} catch (ClassNotFoundException e) {
				e.printStackTrace();
				throw new RuntimeException("Problem working with class :"+className);
			}
		}
		return result;
	}

	
	/**
	 * Only get class names of interfaces
	 * @return Set with class names
	 */
	public Set<String> getInterfaceNames(){
		refreshCache();
		Set<String> result = new HashSet<String>();
		for(String className : this.classCache){
			Class aClass;
			try {
				aClass = Class.forName(className);
				if(aClass.isInterface()){
					result.add(className);
				}
			} catch (ClassNotFoundException e) {
				e.printStackTrace();
				throw new RuntimeException("Problem working with class :"+className);
			}
		}
		return result;
	}
	
	/**
	 * Check if there is an interface with some full name, eg. org.openda.interfaces.IAlgorithm.
	 * Also returns false when the name can not be found. Use isClass to check for existence.
	 * @param className Name of class to be checked.
	 * @return True if the class specified by class name implements an interface
	 */
	public boolean isInterface(String className){
		boolean result=false;
		try {
			Class aClass = Class.forName(className);
			if(aClass.isInterface()){
				result=true;
			}
		} catch (ClassNotFoundException e) {
			// Do nothing ; result becomes false
		}
		return result;
	}

	/**
	 * Find the full name for a class, eg. Set could return java.util.Set
	 * @param shortName Short name (no name space prefix) of the class
	 * @return full class name.
	 */
	public String findFullName(String shortName){
		refreshCache();
		String result=null;
		for(String className : this.classCache){
			if(className.endsWith(shortName)){
				result = className;
				break;
			}
		}
		return result;
	}
	
	
	
	private void refreshCache(){
		if(this.classCache.size()==0){
			// No arguments, look in CLASSPATH
			String s = System.getProperties().getProperty("java.class.path");
			//  break apart with path sep.
			String pathSep = System.getProperties().getProperty("path.separator");
			StringTokenizer st = new StringTokenizer(s, pathSep);
			// Process each classpath
			while (st.hasMoreTokens()) {
				String cand = st.nextToken();
				if (cand.endsWith(".zip") || cand.endsWith(".jar")){
					processOneZip(cand);
				}else{ //assume directory
					File dir = new File(cand);
					if(dir.isDirectory()){
						processDirWithClassFiles(dir,dir);
					}
				}
			}
		}
	}

	private void processOneZip(String zipFileName) {
		//System.out.println("processing archive:"+zipFileName);
		try {
			ZipFile zippy = 
				new ZipFile(new File(zipFileName));
			Enumeration all = zippy.entries();
			// For each entry, get its name and put it into "entries"
			while (all.hasMoreElements()) {
				String className = ((ZipEntry)(all.nextElement())).getName();
				className=className.replaceAll("/",".");
				if(className.endsWith(".class")){
					className=className.replace(".class", "");
					if(matchesFilterList(className)){
						this.classCache.add(className);
					}
				}
			}
		} catch (IOException err) {
			throw new RuntimeException("Problem opening archive :"+zipFileName);
		}
	}

	private void processDirWithClassFiles(File dir, File base) {
		// check that dir is a directory and exclude subversion directories
		if( (dir.isDirectory()) && (dir.getAbsolutePath().indexOf(".svn")<0) ){
			// get dir list
			for(File file : dir.listFiles()){
				if(  (file.isFile()) && (file.getName().endsWith(".class")) ){
					// class file
					String className = RelativePath.getRelativePath(base,file);
					String fileSeparator = File.separator;
					if (fileSeparator.equals("\\")) fileSeparator = "\\\\";
					className = className.replaceAll(fileSeparator, ".");
					className = className.replace(".class", "");
					if(matchesFilterList(className)){
						this.classCache.add(className);
					}
				}else if(file.isDirectory()){
					// another subdir with potentially more class files
					processDirWithClassFiles(file, base);
				}
			}
		}
	}

	private boolean matchesFilterList(String className){
		boolean result=false;
		for(String filter : this.filterList){
			if(className.startsWith(filter)){
				result=true;
			}
		}
		return result;
	}
	
	/**
	 * @param args Command line arguments.
	 */
	public static void main(String[] args) {
		if(args.length>1){
			throw new RuntimeException("Optional argument: interfaceName");
		}
		
		Reflection refl  = new Reflection();
		
		String ifaces[];
		if(args.length==1){ // interface given by user
			String iface = args[0];
			ifaces = new String[]{iface};
		}else{ //default all OpenDA interfaces that are regularly used by endusers
			ifaces = new String[]{"IDataObject","IAlgorithm","org.openda.interfaces.IResultWriter","IStochModelFactory","IStochObserver"};
		}
		// find full names
		for(int i=0;i<ifaces.length;i++){
			String iface = ifaces[i];
			int dotIndex = iface.lastIndexOf('.');
			if(dotIndex<0){
				iface=refl.findFullName(iface);
				if(iface==null){
					throw new RuntimeException("Could not find class with name :"+ifaces[i]);
				}
				if(!refl.isInterface(iface)){
					throw new RuntimeException("This class is not an interface :"+iface);
				}
				ifaces[i] = iface; 
			}
		}
		
		//list all classes that implement each of these interfaces
		for(String iface : ifaces){
			System.out.println("#=========================================");
			System.out.println("Interface "+iface+" is implemented by:");
			Set<String> classNames = refl.getClassNames(iface);
			for(String className : classNames){
				System.out.println("  "+className);
			}
		}
		System.out.println("#=========================================");
	}

}
