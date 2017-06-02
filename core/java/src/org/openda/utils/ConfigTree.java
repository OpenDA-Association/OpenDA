/* OpenDA v2.3.1 
* Copyright (c) 2016 OpenDA Association 
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
import nu.xom.*;

import java.io.*;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;

/**
 * ConfigTree provides a simple tool for handling configuration. It
 * uses a tree to store all the configuration.
 * @author Martin Verlaan
 *
 */
public class ConfigTree {
	/*
	 * internal data 
	 */
	private Document doc=null;
	private Element tree=null;
	private boolean validate=false; //default for validation
	private File workingDir=null;

	private class ResultOfFindTreeNode {
		public Element curElement;
		public String attributeName;
	}

	/**
	 * 
	 * Catch errors from XML parser
	 */
	public class SimpleErrorHandler implements ErrorHandler {
		
	    public void warning(SAXParseException e) throws SAXException {
	    	Results.putMessage("Warning, xml input may be wrong:"+ e.getMessage());
	    }

	    public void error(SAXParseException e) throws SAXException {
	    	Results.putMessage("Warning, xml input may be wrong:"+ e.getMessage());
	    }

	    public void fatalError(SAXParseException e) throws SAXException {
	    	Results.putMessage("Error, problem parsing xml input:"+ e.getMessage());
	    	throw new RuntimeException("Error, problem parsing xml input:"+ e.getMessage());
	    }
	}

	/**
	 * Constructor for the configuration tree.
	 * @param configstring either a url or xml
	 * @param validate exit on nonvalid if boolean is true
	 */
	public ConfigTree(String configstring, boolean validate){
		this.validate = validate;
		setTree(configstring);
		this.workingDir = new File(".");
	}

	/**
	 * Constructor for the configuration tree with default for validation.
	 * @param configstring : either a url or xml
	 */
	public ConfigTree(String configstring){
		setTree(configstring);
	}

	/**
	 * Constructor with delayed setting of file
	 */
	public ConfigTree(){
	}

	/**
	 * Constructor for the configuration tree with default for validation.
	 * @param workingDir component's working directory
	 * @param configString xml string, file name, or file path (relative to working directory
	 */
	public ConfigTree(File workingDir, String configString){
		if(configString==null || configString.equals("")){
			// do nothing ; use default empty config
		}else if(configString.contains("<")){ //xml?
			setTree(configString);
		}else{ //xml-file?
			File configFile = new File(workingDir,configString);
			if(!configFile.exists()){
				throw new RuntimeException("Could not find configuration file:"+configFile.toString());
			}
			this.workingDir = workingDir;
			setTree(configFile);
		}
		// Results.putMessage("conf="+this.toString());
	}

	/**
	 * Constructor for the configuration tree with default for validation.
	 * @param workingDir component's working directory
	 * @param configString xml string, file name, or file path (relative to working directory
	 */
	public ConfigTree(File workingDir, String configString, boolean validate){
		this.validate = validate;
		if(configString==null || configString.equals("")){
			// do nothing ; use default empty config
		}else if(configString.contains("<")){ //xml?
			setTree(configString);
		}else{ //xml-file?
			File configFile = new File(workingDir,configString);
			if(!configFile.exists()){
				throw new RuntimeException("Could not find configuration file:"+configFile.toString());
			}
			this.workingDir = workingDir;
			setTree(configFile);
		}
		// Results.putMessage("conf="+this.toString());
	}


	/**
	 * Contructor based on raw fields. Can be useful for automated generation from parts of an
	 * existing xom tree.
	 * @param doc xom Document
	 * @param tree xom root node
	 * @param validate do xsd validation
	 * @param workingDir path for current file
	 */
	ConfigTree(Document doc, Element tree, boolean validate, File workingDir){
		this.doc        = doc;
		this.tree       = tree;
		this.validate   = validate;
		this.workingDir = workingDir;
	}

	/**
	 * Create a new config-tree from a string with xml, or from a filename.
	 * @param configstring
	 */
	public void setTree(String configstring){
		Builder builder = null;
		try {
			if(configstring.contains("<")){ //this is xml
				builder=new Builder(false);
				this.doc = builder.build(configstring,"http://www.dummy.org/default.xml");
			}else{ // this is a url
				if(this.validate){
					SAXParserFactory factory = SAXParserFactory.newInstance();
					factory.setValidating(true);
					factory.setNamespaceAware(true);
					// only check if a schema was provided
					factory.setFeature("http://apache.org/xml/features/validation/dynamic", true);

					SAXParser parser = factory.newSAXParser();
					parser.setProperty("http://java.sun.com/xml/jaxp/properties/schemaLanguage", 
					      "http://www.w3.org/2001/XMLSchema");

					XMLReader reader = parser.getXMLReader();
					reader.setErrorHandler(new SimpleErrorHandler());

					builder = new Builder(reader);
					
					this.doc = builder.build(configstring);
				}else{
					builder=new Builder(validate);
					this.doc = builder.build(configstring);
				}
			}
			this.tree = this.doc.getRootElement();    
		}
		// indicates a well-formedness error
		catch (ParsingException ex) { 
			Results.putMessage(configstring + " is not well-formed.");
			Results.putMessage(ex.getMessage());
			if(this.validate){
				throw new RuntimeException("Xml not well formed error for input:"+ex.getMessage());
			}
		}  
		catch (IOException e) { 
			e.printStackTrace();
			Results.putMessage(e.getMessage());
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
			Results.putMessage(e.getMessage());
		} catch (SAXException e) {
			e.printStackTrace();
			Results.putMessage(e.getMessage());
		}  		
	}

	/**
	 * Set content from a file
	 * @param configFile ; java file 
	 */
	public void setTree(File configFile){
		Builder builder = new Builder(this.validate);
		try {
			if(!configFile.exists()){ //this is a file
				this.doc = null;
				throw new RuntimeException("Can not open file "+configFile.getName());
			}else{ // this is a url
				Results.putMessage("opening :"+configFile.getAbsolutePath());
				if(this.validate){
					SAXParserFactory factory = SAXParserFactory.newInstance();
					factory.setValidating(true);
					factory.setNamespaceAware(true);
					// only check if a schema was provided
					factory.setFeature("http://apache.org/xml/features/validation/dynamic", true);

					SAXParser parser = factory.newSAXParser();
					parser.setProperty("http://java.sun.com/xml/jaxp/properties/schemaLanguage", 
					      "http://www.w3.org/2001/XMLSchema");

					XMLReader reader = parser.getXMLReader();
					reader.setErrorHandler(new SimpleErrorHandler());

					builder = new Builder(reader);
					this.doc = builder.build(configFile);
				}else{
					builder=new Builder(validate);
					this.doc = builder.build(configFile);
				}
			}
			this.tree = this.doc.getRootElement();    
		}
		// indicates a well-formedness error
		catch (ParsingException ex) { 
			Results.putMessage("Xml is not well-formed: "+configFile.getName());
			Results.putMessage(ex.getMessage());
			if(this.validate){
				throw new RuntimeException("Xml not well formed error for input:"+ex.getMessage());
			}
		}  
		catch (IOException ex) { 
			Results.putMessage("IO error: "+ex.getMessage());
			throw new RuntimeException("Io error:"+ex.getMessage());
		}
		catch(Exception ex){
			Results.putMessage("Other error: "+ex.getMessage());
			throw new RuntimeException("Input error:"+ex.getMessage());
		}
	}


	/**
	 * Return configtree as xml
	 * @return string with xml
	 */
	public String toString(){
		String result="";
		try{
			result = this.doc.toXML();
		} catch(Exception ex) { 
			Results.putMessage(ex.getMessage());
		}
		return result;
	}

	/**
	 * Get element content at a given path
	 * @param path eg "/config/maxIter" of attribute "/config@maxIter"
	 * @return element content (empty if not found)
	 */
	public String getContentString(String path){
		String result=null;
		if(!(this.doc==null)){
			ResultOfFindTreeNode resultOfFindTreeNode = findElementOrAttribute(path);
			String attributeName = resultOfFindTreeNode.attributeName;
			Element curElement = resultOfFindTreeNode.curElement;
			//attribute or element content request?
			if(attributeName.length()>0){
				if(curElement!=null){
					result = curElement.getAttributeValue(attributeName);
				}
			}else{ //element content requested
				if(curElement!=null){
					result = curElement.getValue();
				}
			}
		}
		return result;
	}

	/**
	 * Set element content at a given path
	 * @param path eg "/config/maxIter" of attribute "/config@maxIter"
	 * @param value value to be set
	 */
	public void setContentString(String path, String value){
		if(!(this.doc==null)){
			ResultOfFindTreeNode resultOfFindTreeNode = findElementOrAttribute(path);
			String attributeName = resultOfFindTreeNode.attributeName;
			Element curElement = resultOfFindTreeNode.curElement;
			//attribute or element content?
			if(attributeName.length()>0){
				if(curElement!=null){
					Attribute attribute = curElement.getAttribute(attributeName);
					attribute.setValue(value);
				}
			}else{ //element content
				if(curElement!=null){
					curElement.removeChildren();
					curElement.appendChild(value);
				}
			}
		}
	}

	/**
	 * Get element content at a given path and parse as a string
	 * @param path eg "/config/comment" or attribute "/config@comment"
	 * @param defaultValue, in case content can not be parsed (missing or malformed)
	 * @return element content parsed as boolean (default if not found)
	 */
	public String getAsString(String path, String defaultValue){
		String result=defaultValue;
		String content = getContentString(path);
		if(content!=null){
			result=content;
		}
		return result;
	}


	/**
	 * Get element content at a given path
	 * @param path eg "/config/maxIter" of attribute "/config@maxIter"
	 * @param defaultValue default value, in case content can not be parsed (missing or malformed
	 * @return element content parsed as int(empty if not found)
	 */
	public int getAsInt(String path, int defaultValue){
		int result=defaultValue;
		String content = getContentString(path);
		if(content!=null){
			try{
				result = Integer.parseInt(content);
			}catch(Exception ex){
				// result = defaultValue;
			}
		}
		return result;
	}

	/**
	 * Get element content at a given path
	 * @param path eg "/config/useThisOption" or attribute "/config@useThis"
	 * @param defaultValue default value, in case content can not be parsed (missing or malformed)
	 * @return element content parsed as boolean (default if not found)
	 */
	public boolean getAsBoolean(String path, boolean defaultValue){
		boolean result=defaultValue;
		String content = getContentString(path);
		if(content!=null){
			try{
				result = Boolean.parseBoolean(content);
			}catch(Exception ex){
				// result = defaultValue;
			}
		}
		return result;
	}


	/**
	 * Get element content at a given path
	 * @param path eg "/eps" of attribute "/config@pi"
	 * @param defaultValue default value, in case content can not be parsed (missing or malformed
	 * @return element content parsed as double(empty if not found)
	 */
	public double getAsDouble(String path, double defaultValue){
		double result=defaultValue;
		String content = getContentString(path);
		if(content!=null){
			try{
				result = Double.parseDouble(content);
			}catch(Exception ex){
				// result = defaultValue;
			}
		}
		return result;
	}

	/**
	 * Write configuration to an xml file
	 * @param workingDir (java file)
	 * @param filename to write to (string)
	 */
	public void toFile(File workingDir, String filename){
		try {
			File file = new File(workingDir,filename);
			OutputStream out = new FileOutputStream(file);
			Serializer serializer = new Serializer(out, "UTF-8");
			serializer.setIndent(2);
			serializer.write(this.doc);
			serializer.flush();
			out.close();
		} catch (IOException e) {
			Results.putMessage("Exception: " + e.getMessage());
			//TODO proper handling of file error
		}
	}

	/**
	 * Look for parts of the configuration pointing to sub-parts of the configuration contained
	 * in other (external) xml-files. Also try to recover the corresponding working directory and
	 * classname if possible 
	 * @return array with String-arrays of length 4, containing filename, working directory, 
	 *  label and classname for each part found
	 */
	public String[][] getParts(){
		java.util.Vector<String[]> temp = new java.util.Vector<String[]>();
		if(!(this.doc==null)){
			Element curElement = this.tree;
			Elements children = curElement.getChildElements();
			int n = children.size();
			// navigate to right element
			for(int i=0;i<n;i++){
				String[] nextPart = new String[4]; //space for output of this part
				Element currentElement=children.get(i); //select candidate
				//System.out.println("name :"+currentElement.getLocalName());
				//System.out.println("content :"+currentElement.toXML());
				/*
				 * construct workingDir
				 */
				//Elements ff = currentElement.getChildElements();
				//for(int ii=0;ii<ff.size();ii++){
				//	System.out.println(" subname = " + ff.get(ii).getLocalName());
				//}
				// look for elements workingDir and workingDirectory
				Element workingDirElement = myGetFirstChildElement(curElement, "workingDir");
				String workingDirName = null;
				if(workingDirElement!=null){
					workingDirName = workingDirElement.getValue();
					//System.out.println("workingDir :"+workingDirName);
				}
				if(workingDirName==null){
					workingDirElement = myGetFirstChildElement(currentElement,"workingDirectory");
					if(workingDirElement!=null){
						workingDirName = workingDirElement.getValue();
						//System.out.println("workingDir :"+workingDirName);
					}
				}
				if(workingDirName==null){
					workingDirName = ".";
					//System.out.println("workingDir :"+workingDirName);
				}
				/*
				 * resolve workingDir
				 */
				File resolvedWorkingDir = new File(workingDirName);
				if(this.workingDir!=null){
					resolvedWorkingDir = new File(this.workingDir,workingDirName);
				}
				//System.out.println("ResolvedWorkingDir :"+resolvedWorkingDir.getPath());

				/*
				 * construct filename
				 */
				Element fileElement = myGetFirstChildElement(currentElement,"configString");
				String fileName = null;
				if(fileElement!=null){fileName=fileElement.getValue();}
				if(fileName==null){
					Element configElement = myGetFirstChildElement(currentElement,"configFile");
					if(configElement!=null){
						fileName = configElement.getValue();
						//System.out.println("fileName :'"+fileName+"'");
					}
				}
				if(fileName==null){
					String content = currentElement.getValue();
					File possibleFileName= new File(resolvedWorkingDir,content);
					if(possibleFileName.exists()){
						fileName = content;
						//System.out.println("Found filename in content!!");
						//System.out.println("fileName :'"+fileName+"'");
					}
				}
				/*
				 * construct className
				 */
				String className = currentElement.getAttributeValue("className");

				/*
				 * add to temp
				 */
				nextPart[0] = fileName;
				nextPart[1] = workingDirName;
				nextPart[2] = currentElement.getLocalName();
				nextPart[3] = className;
				// only add valid items
				if(fileName!=null && fileName.length()>0){
					temp.add(nextPart);
				}

				// process <argument>filename</argument>
				Elements arguments = currentElement.getChildElements("argument");
				int nargs = arguments.size();
				if(nargs>0){
					for(int j=0;j<nargs;j++){
						Element argElement = arguments.get(j); 
						if(argElement!=null){
							fileName = argElement.getValue();
							File possibleFile= new File(resolvedWorkingDir,fileName);
							if(possibleFile.exists()){
								//System.out.println("fileName :'"+fileName+"'");
								nextPart = new String[4];
								nextPart[0] = fileName;
								nextPart[1] = workingDirName;
								nextPart[2] = currentElement.getLocalName()+"["+j+"]";
								nextPart[3] = "java.io.File";
								temp.add(nextPart);
							}
						}
					}
				}
			} // loop over i
		}
		String[][] result = new String[temp.size()][];
		for(int i=0;i<temp.size();i++){
			result[i] = temp.get(i);
		}

		//System.out.println("parts \n-------------- \n"+parts2String(result));

		return result;
	}

	public static String parts2String(String[][] parts){
		String result="";
		for(int i=0;i<parts.length;i++){
			result +="part "+i;
			result +=" filename= ";
			if(parts[i][0]!=null){result+=parts[i][0];}
			result +=" dir= ";
			if(parts[i][1]!=null){result+=parts[i][1];}
			result +=" label= ";
			if(parts[i][2]!=null){result+=parts[i][2];}
			result +=" classname=";
			if(parts[i][3]!=null){result+=parts[i][3];}
			result +="\n";
		}
		return result;
	}


	/**
	 * Select parts of an existing tree as new trees.
	 * @param path parts to select
	 * @return a list of new ConfigTree's
	 */
	public ConfigTree[] getSubTrees(String path){
		ConfigTree result[] = null;
		if(!(this.doc==null)){
			Element curElement = this.tree;
			if(path.indexOf("@")>=0){
				throw new RuntimeException("No attribute sign @ allowed in a tree path specification");
			}
			String [] pathList = path.split("/");
			// navigate to right element
			for (int i=0;i<(pathList.length-1);i++) {
				String pathPart = pathList[i];
				if (pathPart.length() == 0) {
					continue;
				}
				curElement = myGetFirstChildElement(curElement,pathPart);
				if (curElement == null) {
					break;
				}
			}
			// now select relevant elements
			if((curElement!=null)&(pathList.length>0)){
				String namespace = curElement.getNamespaceURI();
				Elements treeElements = curElement.getChildElements(pathList[pathList.length-1],namespace);
				int nElements = treeElements.size();
				// TODO if??
						result = new ConfigTree[nElements];
						for(int i=0;i<nElements;i++){
							result[i] = new ConfigTree(treeElements.get(i).toXML());
							//new ConfigTree(this.doc, treeElements.get(i), this.validate, this.workingDir);
						}
			}
		}
		return result;
	}


	/**
	 * Select first child of an existing tree as a new tree.
	 * @return new ConfigTree
	 */
	public ConfigTree getFirstChild(){
		ConfigTree result = null;
		if(!(this.doc==null)){
			Element curElement = this.tree;
			Elements children = curElement.getChildElements();
			if(children.size()<1){
				throw new RuntimeException("expecting at least one child for:"+curElement.toXML());
			}
			result = new ConfigTree(children.get(0).toXML());
		}
		return result;
	}

	/**
	 * The method getFirstChildElement in XOM has arguments that change depending if a namespace is declared.
	 * This obscures the code, so this method provides a wrapper.
	 * @param currentElement xom element 
	 * @param childName name of the child
	 * @return child as xom element
	 */
	private Element myGetFirstChildElement(Element currentElement, String childName){
		Element result=null;
		String nameSpace=currentElement.getNamespaceURI();
		if(nameSpace==null){
			result=currentElement.getFirstChildElement(childName);
		}else{
			result=currentElement.getFirstChildElement(childName,nameSpace);
		}
		return result;
	}

	private ResultOfFindTreeNode findElementOrAttribute(String path) {
		ResultOfFindTreeNode resultOfFindTreeNode = new ResultOfFindTreeNode();
		resultOfFindTreeNode.curElement = this.tree;
		String[] pathList = path.split("@");
		resultOfFindTreeNode.attributeName = "";
		int n=pathList.length;
		if(n>2){
			throw new RuntimeException("Only one attribute sign @ allowed in a path specification");
		}else if(n==2){
			resultOfFindTreeNode.attributeName = pathList[1];
		}
		pathList = pathList[0].split("/");
		// navigate to right element
		for (String aPathList : pathList) {
			if (aPathList.length() == 0) {
				continue;
			}
			resultOfFindTreeNode.curElement = myGetFirstChildElement(resultOfFindTreeNode.curElement, aPathList);
			if (resultOfFindTreeNode.curElement == null) {
				break;
			}
		}
		return resultOfFindTreeNode;
	}
}

