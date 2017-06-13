/* OpenDA v2.4 
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


package org.openda.utils;

import org.openda.interfaces.*;

import java.util.HashMap;

/**
 * TODO: description
 */
public class ResultSelectionConfig {

    private HashMap<String, ResultItem> resultItems;
    private HashMap<String, NonConfiguredCounter> nonConfiguredCounters;

    private boolean doWriteAlgorithmOutput;
    private boolean doWriteModelOutput;
    private boolean doWriteObserverOutput;
    private boolean doWriteNonConfiguredItems;

    public ResultSelectionConfig() {
        resultItems = new HashMap<String, ResultItem>();
        nonConfiguredCounters = new HashMap<String, NonConfiguredCounter>();

        doWriteAlgorithmOutput = true;
        doWriteModelOutput = true;
        doWriteObserverOutput = true;
        doWriteNonConfiguredItems = true;
    }

    /**
     * Simple constructor for selectiong only one result (eg cost or pOpt)
     * @param selectedResultId Identifying string in the calls to Results.put(selectedResultId, values);
     */
    public ResultSelectionConfig(String selectedResultId) {
        this();
        this.doWriteAlgorithmOutput = true;
        this.doWriteModelOutput = false;
        this.doWriteObserverOutput = false;
        this.doWriteNonConfiguredItems = false;
        resultItems.put(composeHashId(IResultWriter.Source.Algorithm, selectedResultId),
                new ResultItem(selectedResultId, IResultWriter.Source.Algorithm, true));
    }

    public void setWriteAlgorithmOutput(boolean doWriteAlgorithmOutput) {
        this.doWriteAlgorithmOutput = doWriteAlgorithmOutput;
    }

    public void setWriteModelOutput(boolean doWriteModelOutput) {
        this.doWriteModelOutput = doWriteModelOutput;
    }

    public void setWriteObserverOutput(boolean doWriteObserverOutput) {
        this.doWriteObserverOutput = doWriteObserverOutput;
    }

    public void setWriteNonConfiguredItems(boolean writeNonConfiguredItems) {
        this.doWriteNonConfiguredItems = writeNonConfiguredItems;
    }

	public void addResultItem(String id, int minSize, int maxSize, IResultWriter.OutputLevel outputLevel, String context, boolean doLog) {
		String key = composeHashKey(id, minSize, maxSize, outputLevel.toString(), context);
		resultItems.put(key, new ResultItem(key, doLog));
	}

	private String composeHashKey(String id, int minSize, int maxSize, String outputLevel, String context) {
		return id+","+minSize+","+maxSize+","+outputLevel+","+context;
	}

    public void addResultItem(String id, String sourceString, boolean doWrite) {
        IResultWriter.Source source;
        source = parseResultSource(sourceString);

        resultItems.put(composeHashId(source, id), new ResultItem(id, source, doWrite));
    }

    public int nextWriteCounter(IResultWriter.Source source, String id, IResultWriter.MessageType messageType) {
        if (doWrite(source)) {
			ResultItem resultItem = resultItems.get(composeHashId(source, id));
            if (resultItem != null) {
                if (resultItem.doWrite()) {
                    return resultItem.nextCount();
                } else {
					return -1;
				}
            }
            if (doWriteNonConfiguredItems) {
                NonConfiguredCounter counter = findOrCreateNonConfiguredCounter(id, messageType);
                return counter.nextCount();
            }
        }
        return -1;
    }

	public int nextWriteCounter(String id, int size, int defaultMaxSize, IResultWriter.OutputLevel outputLevel, String context, IResultWriter.MessageType messageType) {
		String key = getMatchingKey(id,size,defaultMaxSize,context,outputLevel);
		ResultItem resultItem  = resultItems.get(key);
		if (resultItem != null) {
			if (resultItem.doWrite()) {
				return resultItem.nextCount();
			} else {
				return -1;
			}
		}
        return -1;
    }

	private String getMatchingKey(String id, int size, int defaultMaxSize, String context, IResultWriter.OutputLevel outputLevel) {
		String[] keySet = resultItems.keySet().toArray(new String[resultItems.keySet().size()]);
		int indexId = 0;
		int indexMinSize = 1;
		int indexMaxSize = 2;
		int indexOutputLevel = 3;
		int indexContext = 4;
		for (int i=0; i<keySet.length; i++){
			String[] itemAttributes = keySet[i].split(",");
			if (itemAttributes.length>1){
                boolean idMatched = itemAttributes[indexId].equalsIgnoreCase(IResultWriter.defaultId) || itemAttributes[indexId].equalsIgnoreCase(id) || match(id, itemAttributes[indexId]);

                int specifiedMinSize = Integer.parseInt(itemAttributes[indexMinSize]);
                int specifiedMaxSize = Integer.parseInt(itemAttributes[indexMaxSize]);
                if (specifiedMaxSize==IResultWriter.defaultMaxSize){
                    specifiedMaxSize = defaultMaxSize;
                }
                boolean sizeMatched = (size <= specifiedMaxSize && size >= specifiedMinSize);
                int outLevelSpecified = IResultWriter.OutputLevel.valueOf(itemAttributes[indexOutputLevel]).ordinal();
                int outLevelThisItem = outputLevel.ordinal();
                boolean outputLevelMatched = outLevelThisItem <= outLevelSpecified;

                boolean contextMatched = itemAttributes[indexContext].matches(IResultWriter.defaultContext) || context.matches(IResultWriter.defaultContext) || context.matches(itemAttributes[indexContext]);

                if (idMatched && sizeMatched && outputLevelMatched && contextMatched) {
                    return keySet[i];
                }

			} else {
				// To support displaying cost for GUI, where ResultSelectionConfig(String selectedResultId)
				// is called from ControlGui.resetOutput() with selectedResultId = "cost".
				return itemAttributes[0];
			}
		}
		return null;
	}

	public boolean doWrite(IResultWriter.Source source) {
        return source == IResultWriter.Source.Algorithm && doWriteAlgorithmOutput ||
                source == IResultWriter.Source.Observer && doWriteObserverOutput ||
                source == IResultWriter.Source.Model && doWriteModelOutput;
    }

    public boolean doWrite(IInstance source) {
        return source instanceof IAlgorithm && doWriteAlgorithmOutput ||
                source instanceof IStochObserver && doWriteObserverOutput ||
                source instanceof IModelInstance && doWriteModelOutput;
    }

    public boolean doWrite(IResultWriter.Source source, String id) {
        if (doWrite(source)) {
            ResultItem resultItem = resultItems.get(composeHashId(source, id));
            if (resultItem != null) {
                return resultItem.doWrite();
            }
            return doWriteNonConfiguredItems;
        }
        return false;
    }

    public boolean doWrite(IInstance source, String id) {
        if (doWrite(source)) {
            ResultItem resultItem = resultItems.get(composeHashId(source, id));
            if (resultItem != null) {
                return resultItem.doWrite();
            }
            return doWriteNonConfiguredItems;
        }
        return false;
    }

    private IResultWriter.Source parseResultSource(String sourceString) {
        IResultWriter.Source source = IResultWriter.Source.Algorithm;
        if (sourceString != null) {
            if (sourceString.equalsIgnoreCase("model") ||
                    sourceString.equalsIgnoreCase("stochmodel") ||
                    sourceString.equalsIgnoreCase("stochasticmodel")) {
                source = IResultWriter.Source.Model;
            } else if (sourceString.equalsIgnoreCase("alg") ||
                    sourceString.equalsIgnoreCase("algorithm")) {
                source = IResultWriter.Source.Algorithm;
            } else if (sourceString.equalsIgnoreCase("obs") ||
                    sourceString.equalsIgnoreCase("observer") ||
                    sourceString.equalsIgnoreCase("stochobserver") ||
                    sourceString.equalsIgnoreCase("stochasticobserver")) {
                source = IResultWriter.Source.Observer;
            } else {
                throw new RuntimeException("Unknown result source: " + sourceString);
            }
        }
        return source;
    }

    private NonConfiguredCounter findOrCreateNonConfiguredCounter(String id, IResultWriter.MessageType messageType) {
        String resultId = composeResultId(id, messageType);
        NonConfiguredCounter nonConfiguredCounter = nonConfiguredCounters.get(resultId);
        if (nonConfiguredCounter == null) {
            nonConfiguredCounter = new NonConfiguredCounter();
            nonConfiguredCounters.put(id, nonConfiguredCounter);
        }
        return nonConfiguredCounter;
    }

    private String composeResultId(String id, IResultWriter.MessageType messageType) {
        return id + "-" + messageType.toString();
    }

    /**
     * @param text Text to test
     * @param pattern (Wildcard) pattern to test
     * @return True if the text matches the wildcard pattern
     */
    private static boolean match(String text, String pattern)
    {
        return text.matches(pattern.replace("?", ".?").replace("*", ".*?"));
    }

    private class NonConfiguredCounter {
        int count = 0;

        private int nextCount() {
            return ++count;
        }
    }

    /**
     * TODO: description
     */
    public static class ResultItem {

        private String id;
        private IResultWriter.Source source;
        private boolean write;
        private int count;

        public ResultItem(String id, IResultWriter.Source source, boolean write) {
            this.id = id;
            this.source = source;
            this.write = write;
            this.count = 0;
        }

		public ResultItem(String key, boolean doLog) {
			this.id = key;
			this.write = doLog;
			this.count = 0;
		}

		public String getId() {
            return id;
        }

        public IResultWriter.Source getSource() {
            return source;
        }

        public boolean doWrite() {
            return write;
        }

        public int nextCount() {
            return ++count;
        }
    }

    private static String composeHashId(IResultWriter.Source source, String id) {
        return id+source.toString();
    }

    private static String composeHashId(IInstance source, String id) {
        return id + source.toString();
    }
}
