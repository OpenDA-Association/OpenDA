package org.openda.exchange;

import org.openda.utils.IMyObservable;
import org.openda.utils.IMyObserver;

public 	class DummyObserver implements IMyObserver{
	public boolean changed=false;
	public IMyObservable changedBy=null;

	public void update(IMyObservable observable, Object arg) {
		this.changed=true;
		this.changedBy=observable;
	}
	
	public void reset(){
		this.changed=false;
		this.changedBy=null;
	}
}
