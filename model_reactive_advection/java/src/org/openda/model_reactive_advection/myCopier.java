package org.openda.model_reactive_advection;

import java.io.File;
import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.interfaces.IPrevExchangeItem;

/**
 * Copy values for 'c' concentration on grid from one file to another.
 * Arguments [sourceFile] [destFile]
 * Assumes destination exists.
 *
 * @author verlaanm
 *
 */
public class myCopier {

	/**
	 * arg[0] is source file
	 * arg[1] is destination
	 *
	 * @param args
	 */
	public static void main(String args[]){
		if(args.length<2){
			throw new RuntimeException("SimpleBbAsciiCopier.main requires 2 arguments");
		}
		File inputFile = new File(args[0]);
		if(!inputFile.exists()){
			throw new RuntimeException("SimpleBbAsciiCopier.main source file not found:"+args[0]);
		}
		File outputFile = new File(args[1]);
		if(!outputFile.exists()){
			throw new RuntimeException("SimpleBbAsciiCopier.main destination file not found:"+args[1]);
		}


		IoObjectInterface output = new myWrapper();
		String ioArgs[] = {};
		output.initialize(outputFile.getParentFile(), outputFile.getName(), ioArgs);
		IPrevExchangeItem outputItem1=null;
		IPrevExchangeItem outputItem2=null;
		for(IPrevExchangeItem item:output.getExchangeItems()){
			String itemId = item.getId();
			//System.out.println("looking at item: "+itemId);
			if(itemId.equalsIgnoreCase("concentration1.grid")){
				outputItem1 = item;
			}
			if(itemId.equalsIgnoreCase("concentration2.grid")){
				outputItem2 = item;
			}
		}

		IoObjectInterface input = new myWrapper();
		input.initialize(inputFile.getParentFile(), inputFile.getName(), ioArgs);
		for(IPrevExchangeItem item:input.getExchangeItems()){
			String itemId = item.getId();
			//System.out.println("looking at item: "+itemId);
			if(itemId.equalsIgnoreCase("concentration1.grid")){
				//System.out.println("changing item: "+itemId);
				double values[] = item.getValuesAsDoubles();
				outputItem1.setValuesAsDoubles(values);
			}
			if(itemId.equalsIgnoreCase("concentration2.grid")){
				//System.out.println("changing item: "+itemId);
				double values[] = item.getValuesAsDoubles();
				outputItem2.setValuesAsDoubles(values);
			}
		}

		input.finish();
		output.finish();
	}

}
