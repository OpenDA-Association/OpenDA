/* MOD_V1.0 
* Copyright (c) 2010 OpenDA Association 
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
public class MyCancelTask implements ucar.nc2.util.CancelTask{

  /** Called routine should check often during the task and cancel the task if it returns true. */
  public boolean isCancel(){
     return false;
  };

  /** Called routine got an error, so it sets a message for calling program to show to user. */
  public void setError(String msg){
     System.out.println("Error: msg");
  };
}
