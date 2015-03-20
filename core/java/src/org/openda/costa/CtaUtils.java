package org.openda.costa;

/**
 * Created by IntelliJ IDEA.
 * User: nils
 * Date: 7/13/12
 * Time: 9:32 AM
 * To change this template use File | Settings | File Templates.
 */
public class CtaUtils extends CtaObject{

	static public void print_native_memory(String where, int level){
		print_memory(where, level);
	}
	private static native void print_memory(String where, int level);
}
