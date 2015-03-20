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
package org.openda.application.gui;
import org.openda.utils.ConfigTree;
import org.openda.utils.Results;

import javax.swing.tree.DefaultMutableTreeNode;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

public class InputTree extends DefaultMutableTreeNode{

    public static final String fileDoesNotExistMessage = "File does not exist.";
    public static final String fileNotEditableMessage = "This is not an editable file.";
    private static final long maxSizeToShowFileContentInTree = 65536; // 2^16

    /**
     * inputInfo contains the private data that is added to the tree.
     * @author verlaanm
     *
     */
    private class inputInfo {
    	public String label=null;
        public String fileName=null;
        public String parentDir=null;
        public String workingDir=null;
        public String className=null;
        public String currentBuffer="";
        public int initialHash=0;
        public boolean textEditable=true;

        public inputInfo(String label,String fileName,String workingDir,String parentDir, String className) {
        	this.label = label;
            this.fileName = fileName;
            this.workingDir = workingDir;
            this.parentDir = parentDir;
            this.className = className;
        }

        public String toString() {
            return this.label+" "+this.fileName;
        }
    }

    /**
     * Recursively construct a tree with input files from top level file
     * @param parentDir
     * @param workingDir
     * @param fileName
     * @param label
     * @param className
     */
	public InputTree(String parentDir, String workingDir, String fileName, String label, String className){
		// create top node
		super();
		inputInfo info = new inputInfo(label,fileName,workingDir,parentDir,className);
		this.setUserObject(info);
		//System.out.println("label="+label+" fileName="+fileName+
		//		" workingDir="+workingDir+" parentDir"+parentDir+" className="+className);
		// look for includes
		File resolvedWorkingDir = new File(parentDir,workingDir);
		boolean isXmlFile = fileName.contains(".xml") || fileName.contains(".oda");
        File file = new File(resolvedWorkingDir, fileName);
        if( file.exists() && isXmlFile && file.length() < maxSizeToShowFileContentInTree){
			ConfigTree appConf = new ConfigTree(resolvedWorkingDir,fileName);
			String[][] parts = appConf.getParts();
			InputTree subNode;
            for (String[] part : parts) {
                subNode = new InputTree(resolvedWorkingDir.getPath(), part[1], part[0], part[2], part[3]);
                this.add(subNode);
            }
		}
	}

    public String getLabel(){
		inputInfo info = (inputInfo) this.getUserObject();
		return info.label;
	}

	public String getClassName(){
		inputInfo info = (inputInfo) this.getUserObject();
		return info.className;
	}

	public String getFileName(){
		inputInfo info = (inputInfo) this.getUserObject();
		return info.fileName;
	}

	public String getWorkingDir(){
		inputInfo info = (inputInfo) this.getUserObject();
		return info.workingDir;
	}

	public String getResolvedWorkingDir(){
		inputInfo info = (inputInfo) this.getUserObject();
        return (new File(info.parentDir,info.workingDir)).getAbsolutePath();
	}

	public String getParentDir(){
		inputInfo info = (inputInfo) this.getUserObject();
		return info.parentDir;
	}

	public String getCurrentBuffer(){
		inputInfo info = (inputInfo) this.getUserObject();
		int hash = info.initialHash;
		if(hash==0){ // read file because this is first time
			info.currentBuffer = readFile(this.getResolvedWorkingDir(),this.getFileName());
			info.initialHash = info.currentBuffer.hashCode();
            if (info.currentBuffer.indexOf("<uncertainties") >= 0) {
                info.textEditable = false;
            }
		}
		return info.currentBuffer;
	}

	public int getHash(){
		inputInfo info = (inputInfo) this.getUserObject();
		return info.initialHash;
	}

	public void setCurrentBuffer(String buffer){
		inputInfo info = (inputInfo) this.getUserObject();
		info.currentBuffer = buffer;
	}

	public void setHash(int hash){
		inputInfo info = (inputInfo) this.getUserObject();
		info.initialHash = hash;
	}

	public boolean hasChanged(){
		boolean result = false;
		inputInfo info = (inputInfo) this.getUserObject();
		if(info.initialHash==0){ //either not edited or new file in buffer
			if(info.currentBuffer.length()>0){
                if (!info.currentBuffer.equals(fileDoesNotExistMessage) &&
                        !info.currentBuffer.equals(fileNotEditableMessage))
                result = true;
			}
		}else{
			int currentHash = info.currentBuffer.hashCode();
			//System.out.println("current hash"+currentHash+" initial hash "+info.initialHash);
			if(currentHash!=info.initialHash){
				result = true;
			}
		}
		//System.out.println("file "+this.getFileName()+" has changed is "+result);
		return result;
	}

	public boolean treeHasChanged(){
		boolean result = false;
		if(this.hasChanged()){
			result=true;
		}else{
			InputTree[] children = this.getChildren();
            for (InputTree aChildren : children) {
                boolean subResult = aChildren.treeHasChanged();
                if (subResult) {
                    result = true;
                    break;  // no need to look further
                }
            }
		}
		return result;
	}

	public InputTree[] getChildren(){
		int n = this.getChildCount();
		InputTree children[] = new InputTree[n];
		for(int i=0;i<n;i++){
			children[i] = (InputTree) this.getChildAt(i);
		}
		return children;
	}

	public String toString(){
		return this.getLabel()+" "+this.getFileName();
	}

	public String toLongString(){
		return this.toLongString("");
	}

	public String toLongString(String prefix){
		String result = "";
		result += prefix;
		result += " label="+this.getLabel();
		result += " fileName="+this.getFileName();
		result += " workingDir="+this.getWorkingDir();
		result += " parentDir="+this.getParentDir();
		result += " className="+this.getClassName();
		result += "\n";
		InputTree[] children = this.getChildren();
		int n = children.length;
		for(int i=0;i<n;i++){
			result+=children[i].toLongString(prefix+"   ");
		}
		return result;
	}

	/**
	 * Save the data in this node if the data has been modified. Modification
	 * of the content is checked with the checksum. If no filename has been
	 * specified than a gui may appear to ask for one. If not changes are ignored.
	 * @param guiConfirm should
	 */
	public void saveNode(boolean guiConfirm){
		if(this.hasChanged()){
			inputInfo info = (inputInfo) this.getUserObject();
			String currentBuffer= info.currentBuffer;
			File dir = new File(info.parentDir,info.workingDir);
			// try saving buffer
			if(info.fileName==null){
				if(guiConfirm){
					File input=FileDialog.openInput(new File("."));
					info.fileName = input.getName();
					info.workingDir = input.getParent();
					dir = new File(info.workingDir);
				}else{
					// Don't write anything TODO ?!
				}
			}
			if(dir.exists()){
				if(info.fileName!=null){
				   writeFile(dir.getAbsolutePath(), info.fileName, currentBuffer);
				}
			}
			//update hash
			info.initialHash = currentBuffer.hashCode();
			this.setUserObject(info);
		}
	}

	public void saveTree(boolean guiConfirm){
		this.saveNode(guiConfirm);
		InputTree[] children = this.getChildren();
        for (InputTree aChildren : children) {
            aChildren.saveTree(guiConfirm);
        }
	}

	/**
	 * Utility function for getCurrentBuffer to read a file into the buffer if this has not been done
	 * before.
	 * @param workingDir dir of file to read
	 * @param fileName of file to read
	 * @return string with contents of the file
	 */
	public String readFile(String workingDir,String fileName){
		File dir =new File(workingDir);
		String result = "";
		try{
			FileReader file = new FileReader(new File(dir,fileName));
			BufferedReader buff = new BufferedReader(file);
			boolean eof=false;
			while(!eof){
                if (result.length() > maxSizeToShowFileContentInTree) {
                    result+="(... File too large to display fully. File truncated ...)";
                    eof=true;
                } else {
                    String line = buff.readLine();
                    if(line==null){
                        eof=true;
                    }else{
                        result+=line+"\n";
                    }
                }
			}
		}catch(IOException e){
			result = "";
		}
		return result;
	}

	/**
	 * Utility function for getCurrentBuffer to read a file into the buffer if this has not been done
	 * before.
	 * @param workingDir Working directory containing file to be read
	 * @param fileName Name of file to read
	 * @param buffer String with contents of the file
	 */
	public void writeFile(String workingDir,String fileName, String buffer){
        try {
           	File file = new File(workingDir,fileName);
            FileWriter out = new FileWriter(file);
            BufferedWriter buffOut = new BufferedWriter(out);
	    	buffOut.write(buffer);
            buffOut.close();
        } catch (IOException e) {
            Results.putMessage("Exception: " + e.getMessage());
            throw new RuntimeException(e.getMessage());
        }
	}

    public void setTextEditable(boolean editable) {
        inputInfo info = (inputInfo) this.getUserObject();
        info.textEditable = editable;
    }

    public boolean getTextEditable() {
        inputInfo info = (inputInfo) this.getUserObject();
        return info.textEditable;
    }
}
