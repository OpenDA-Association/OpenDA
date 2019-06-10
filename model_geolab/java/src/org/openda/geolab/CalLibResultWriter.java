package org.openda.geolab;

import org.openda.interfaces.*;
import org.openda.utils.Instance;
import org.openda.utils.Matrix;
import org.openda.utils.TreeVector;
import org.openda.utils.Vector;

import java.util.ArrayList;
import java.util.HashMap;

public class CalLibResultWriter implements IResultWriter {

	private ArrayList<String> lines = new ArrayList<>();

	private static final String commentPrefix = "# ";
	private HashMap<String, Integer> iter = new HashMap<>();

	CalLibResultWriter() {
		lines.add("import numpy as np");
	}

	public void putMessage(Source source, String comment) {
		comment = comment.replaceAll("\n", "\n"+commentPrefix);
		lines.add(commentPrefix + comment);
	}

	public void putMessage(IInstance source, String comment) {
		comment = comment.replaceAll("\n", "\n"+commentPrefix);
		lines.add(commentPrefix + " " + Instance.identifySource(source) + " " +  comment);
	}

	public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration) {
		lines.add(commentPrefix + " resultItem id: "+ id +", outputLevel: "+ outputLevel.toString() +", context: "+ context);
		Integer currentIter =0;
		if(this.iter.containsKey(id)){
			currentIter = this.iter.get(id);
		}else{
			lines.add(id+"=[]");
		}
		this.iter.put(id, currentIter+1);
		if (result instanceof ITreeVector) {
			String valueLine = commentPrefix + " " + ((ITreeVector)result).getId() + ": ";
			boolean printComma = false;
			for (String subTreeVectorId : ((ITreeVector)result).getSubTreeVectorIds()) {
				if (printComma) {
					valueLine += ", ";
				} else {
					printComma = true;
				}
				valueLine += subTreeVectorId;
			}
			lines.add(valueLine);
		}
		String valueLine = id + ".append(";
		if (result instanceof Matrix) {
			valueLine += this.serializeMatrix(((Matrix)result));
		} else if (result instanceof Vector) {
			valueLine += result.toString();
		} else if (result instanceof TreeVector) {
			throw new RuntimeException("CalLibResultWriter: logging TreeVector not implemented yet");
		} else {
			valueLine += result.toString();
		}
		valueLine += ")";
		lines.add(valueLine);
	}

	public void putIterationReport(IInstance source, int iteration, double cost, IVector parameters) {
	}

	public int getDefaultMaxSize() {
		return 1000;
	}

	public void free(){
	}

	private String serializeMatrix(IMatrix matrix) {
		StringBuilder line = new StringBuilder();
		line.append("[[");
		for(int i=0;i< matrix.getNumberOfRows();i++){
			if(i>0) line.append("],[");
			for(int j=0;j<Math.min(matrix.getNumberOfColumns(),40);j++){
				if(j>0) {
					line.append(",");
				}
				line.append(matrix.getValue(i, j));
			}
		}
		line.append("]]");
		return line.toString();
	}

	int getMessageCount() {
		return lines.size();
	}

	String getNextMessage() {
		if (lines.size() == 0) {
			throw new RuntimeException("CalLibResultWriter: no messages any more");
		}
		String line = lines.get(0);
		lines.remove(0);
		return line;
	}
}

