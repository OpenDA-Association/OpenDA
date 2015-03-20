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
package org.openda.uncertaintygui.genericplot;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;

//////////////////////////////////////////////////////////////////////////
//// PlotLive
/**
Plot signals dynamically, where points can be added at any time
and the display will be updated.  This should be normally used
with some finite persistence so that old points are erased as new
points are added.  Unfortunately, the most efficient way to erase
old points is to draw graphics using the "exclusive or" mode, which
introduces quite a number of artifacts.  When lines are drawn
between points, where they overlap the points the line becomes
white. Moreover, if two lines or points overlap completely, they
disappear.
<p>
This class is abstract, so it must be used by creating a derived
class.  To use it, create a derived class with an
addPoints() method. Your class may also set graph parameters like
titles and axis labels in the constructor by calling
methods in the Plot or PlotBox classes (both of which are base classes).
The addPoints() method should call addPoint() of the Plot base
class to dynamically add points to the plot.  This method is called
within a thread separate from the applet thread, so the zooming
mechanism and buttons remain live.

@author Edward A. Lee, Christopher Hylands, Contributor: Jeff Lane
@version $Id: PlotLive.java,v 1.58 2003/01/08 02:39:06 ptII Exp $
@since Ptolemy II 0.2
 */
public abstract class PlotLive extends Plot implements Runnable {

    ///////////////////////////////////////////////////////////////////
    ////                         public methods                    ////

    /** Redefine in derived classes to add points to the plot.
     *  Adding many points at once will make the plot somewhat faster
     *  because the thread yields between calls to this method.
     *  However, the plot will also be somewhat less responsive to user
     *  inputs such as zooming, filling, or stopping.  In the derived-class
     *  implementation, this method should probably be synchronized.
     *
     *  <p>Jeff Lane points out that if the derived class version of
     *  addPoints() does not return quickly, and it seems like then you
     *  may want to experiment with addPoints() to not being synchronized.
     */

    public abstract void addPoints();

    /** Make start and stop buttons.
     *  This method is deprecated.  Use setButtons() instead.
     *  @deprecated
     */

    public void makeButtons() {
        if (_startButton == null) {
            _startButton = new JButton("start");
            _startButton.addActionListener(new StartButtonListener());
            add(_startButton);
        }
        _startButton.setVisible(true);

        if (_stopButton == null) {
            _stopButton = new JButton("stop");
            _stopButton.addActionListener(new StopButtonListener());
            add(_stopButton);
        }
        _stopButton.setVisible(true);
        _stopButton.setEnabled(false);
        _startButton.setEnabled(true);
    }

    /** Pause the plot.  To resume, call start().
     */

    public void pause() {
        _paused = true;
        _plotting = false;
        _stopButton.setEnabled(false);
        _startButton.setEnabled(true);
    }

    /** This is the body of a thread that repeatedly calls addPoints()
     *  if the plot is active.  To make the plot active, call start().
     *  To pause the plot, call pause().  To stop the plot and destroy
     *  the thread, call stop().  The next time start() is called, a new
     *  thread will be started. Between calls to addPoints(), this method calls
     *  Thread.yield() so that the thread does not hog all
     *  the resources.  This somewhat slows down execution, so derived
     *  classes may wish to plot quite a few points in their
     *  addPoints() method, if possible.  However,
     *  plotting more points at once may also decrease the
     *  responsiveness of the user interface.
     */

    public void run() {
        while (_plotting || _paused) {
            if (_plotting) {
                addPoints();
                // Give the event thread a chance.
                Thread.yield();
            } else if (_paused) {
                // NOTE: Cannot synchronize this entire method because then
                // the Thread.yield() call above does not yield to any
                // synchronized methods (like _drawPlot()).
                synchronized(this) {
                    try {
                        wait();
                    } catch (InterruptedException e) {}
                }
            }
        }
    }

    /** If the argument is true, make a start, stop, and fill button
     *  visible at the upper right.  Otherwise, make the buttons invisible.
     *  NOTE: The buttons may infringe on the title space,
     *  if the title is long.  In an application, it is preferable to provide
     *  a menu with the commands.  This way, when printing the plot,
     *  the printed plot will not have spurious buttons.  Thus, this method
     *  should be used only by applets, which normally do not have menus.
     */

    public void setButtons(boolean visible) {
        super.setButtons(visible);

        if (_startButton == null) {
            _startButton = new JButton("start");
            _startButton.addActionListener(new StartButtonListener());
            add(_startButton);
        }
        _startButton.setVisible(visible);

        if (_stopButton == null) {
            _stopButton = new JButton("stop");
            _stopButton.addActionListener(new StopButtonListener());
            add(_stopButton);
        }
        _stopButton.setVisible(visible);

        if (visible) {
            _stopButton.setEnabled(false);
            _startButton.setEnabled(true);
        }
    }

    /** Make the plot active.  Start a new thread if necessary.
     */

    public synchronized void start() {
        _plotting = true;
        _paused = false;
        _stopButton.setEnabled(true);
        _startButton.setEnabled(false);
        if (_plotLiveThread == null) {
            _plotLiveThread = new Thread(this, "PlotLive Thread");
            _plotLiveThread.start();
        } else {
            synchronized(this) {
                notifyAll();
            }
        }
    }

    /** Stop the plot.  The plot thread exits.  This should be called by
     *  an applet's stop() method.
     */

    public void stop() {
        _plotting = false;
        _paused = false;
        _plotLiveThread = null;
    }

    ///////////////////////////////////////////////////////////////////
    ////                         private variables                 ////

    /** @serial Thread of this plotter */
    private Thread _plotLiveThread = null;

    /** @serial True if we are actually plotting. */
    private boolean _plotting = false;
    /** @serial True if we are paused. */
    private boolean _paused = false;

    /** @serial Start and Stop Buttons. */
    private JButton _startButton, _stopButton;

    ///////////////////////////////////////////////////////////////////
    ////                         inner classes                     ////

    class StartButtonListener implements ActionListener {
        public void actionPerformed(ActionEvent event) {
            start();
        }
    }

    // Despite the name, the stop button calls pause.
    class StopButtonListener implements ActionListener {
        public void actionPerformed(ActionEvent event) {
            pause();
        }
    }
}
