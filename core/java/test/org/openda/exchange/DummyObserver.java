/* OpenDA v2.4.1 
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
