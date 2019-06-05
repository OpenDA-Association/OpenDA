/*
 * Copyright (c) 2019 OpenDA Association
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

// TO DO:
//   - steps between points rather than connected lines.
//   - cubic spline interpolation
//
// NOTE: The XOR drawing mode is needed in order to be able to erase
// plotted points and restore the grid line, tick marks, and boundary
// rectangle.  This introduces a number of artifacts, particularly
// where lines cross.  A better alternative in the long run would be
// use Java 2-D, which treats each notation on the screen as an object,
// and supports redrawing only damaged regions of the screen.

// NOTE: There are quite a few subjective spacing parameters, all
// given, unfortunately, in pixels.  This means that as resolutions
// get better, this program may need to be adjusted.

import java.awt.BasicStroke;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.Serializable;
import java.util.Enumeration;
import java.util.Vector;
import javax.swing.JComponent;

//////////////////////////////////////////////////////////////////////////
//// Plot
/**
A flexible signal plotter.  The plot can be configured and data can
be provided either through a file with commands or through direct
invocation of the public methods of the class.
<p>
When calling the public methods, in most cases the changes will not
be visible until paintComponent() is called.  To request that this
be done, call repaint().  One exception is addPoint(), which
makes the new point visible immediately if the plot is visible on
the screen and addPoint() is called from the event dispatching thread.
<p>
This base class supports a simple file syntax that has largely been
replaced by the XML-based PlotML syntax.  To read a file or a
URL in this older syntax, use the read() method.
This older syntax contains any number commands,
one per line.  Unrecognized commands and commands with syntax
errors are ignored.  Comments are denoted by a line starting with a
pound sign "#".  The recognized commands include those supported by
the base class, plus a few more.  The commands are case
insensitive, but are usually capitalized.  The number of data sets
to be plotted does not need to be specified.  Data sets are added as needed.
Each dataset can be optionally identified with
color (see the base class) or with unique marks.  The style of
marks used to denote a data point is defined by one of the following
commands:
<pre>
Marks: none
Marks: points
Marks: dots
Marks: various
Marks: pixels
</pre>
Here, "points" are small dots, while "dots" are larger.  If "various"
is specified, then unique marks are used for the first ten data sets,
and then recycled. If "pixels" are specified, then each point is
drawn as one pixel.
Using no marks is useful when lines connect the points in a plot,
which is done by default.  However, if persistence is set, then you
may want to choose "pixels" because the lines may overlap, resulting
in annoying gaps in the drawn line.
If the above directive appears before any DataSet directive, then it
specifies the default for all data sets.  If it appears after a DataSet
directive, then it applies only to that data set.
<p>
To disable connecting lines, use:
<pre>
Lines: off
</pre>
To reenable them, use
<pre>
Lines: on
</pre>
You can also specify "impulses", which are lines drawn from a plotted point
down to the x axis.  Plots with impulses are often called "stem plots."
These are off by default, but can be turned on with the
command:
<pre>
Impulses: on
</pre>
or back off with the command
<pre>
Impulses: off
</pre>
If that command appears before any DataSet directive, then the command
applies to all data sets.  Otherwise, it applies only to the current data
set.
To create a bar graph, turn off lines and use any of the following commands:
<pre>
Bars: on
Bars: <i>width</i>
Bars: <i>width, offset</i>
</pre>
The <i>width</i> is a real number specifying the width of the bars
in the units of the x axis.  The <i>offset</i> is a real number
specifying how much the bar of the <i>i</i><sup>th</sup> data set
is offset from the previous one.  This allows bars to "peek out"
from behind the ones in front.  Note that the frontmost data set
will be the first one.  To turn off bars, use
<pre>
Bars: off
</pre>
To specify data to be plotted, start a data set with the following command:
<pre>
DataSet: <i>string</i>
</pre>
Here, <i>string</i> is a label that will appear in the legend.
It is not necessary to enclose the string in quotation marks.
To start a new dataset without giving it a name, use:
<pre>
DataSet:
</pre>
In this case, no item will appear in the legend.
New datasets are plotted <i>behind</i> the previous ones.
If the following directive occurs:
<pre>
ReuseDataSets: on
</pre>
Then datasets with the same name will be merged.  This makes it
easier to combine multiple datafiles that contain the same datasets
into one file.  By default, this capability is turned off, so
datasets with the same name are not merged.
The data itself is given by a sequence of commands with one of the
following forms:
<pre>
<i>x</i>, <i>y</i>
draw: <i>x</i>, <i>y</i>
move: <i>x</i>, <i>y</i>
<i>x</i>, <i>y</i>, <i>yLowErrorBar</i>, <i>yHighErrorBar</i>
draw: <i>x</i>, <i>y</i>, <i>yLowErrorBar</i>, <i>yHighErrorBar</i>
move: <i>x</i>, <i>y</i>, <i>yLowErrorBar</i>, <i>yHighErrorBar</i>
</pre>
The "draw" command is optional, so the first two forms are equivalent.
The "move" command causes a break in connected points, if lines are
being drawn between points. The numbers <i>x</i> and <i>y</i> are
arbitrary numbers as supported by the Double parser in Java.
If there are four numbers, then the last two numbers are assumed to
be the lower and upper values for error bars.
The numbers can be separated by commas, spaces or tabs.
<p>Some of the methods, such as those that add points a plot, are
executed in the event thread, possibly some time after they are called.
If they are called from a thread different from the event thread,
then the order in which changes to the plot take effect may be
surprising.  We recommend that any code you write that changes
the plot in visible ways be executed in the event thread. You
can accomplish this using the following template:
<pre>
         Runnable doAction = new Runnable() {
             public void run() {
                ... make changes here (e.g. setMarksStyle()) ...
            }
         };
         plot.deferIfNecessary(doAction);
</pre>
<p>
This plotter has some <A NAME="ptplot limitations">limitations</a>:
<ul>
<li> If you zoom in far enough, the plot becomes unreliable.
     In particular, if the total extent of the plot is more than
     2<sup>32</sup> times extent of the visible area, quantization
     errors can result in displaying points or lines.
     Note that 2<sup>32</sup> is over 4 billion.
<li> The limitations of the log axis facility are listed in
     the <code>_gridInit()</code> method in the PlotBox class.
</ul>

@author Edward A. Lee, Christopher Hylands
@version $Id: Plot.java,v 1.210 2003/04/19 04:53:33 neuendor Exp $
@since Ptolemy II 0.2
*/
public class Plot extends PlotBox {

    ///////////////////////////////////////////////////////////////////
    ////                         public methods                    ////

    /** Add a legend (displayed at the upper right) for the specified
     *  data set with the specified string.  Short strings generally
     *  fit better than long strings.
     *  @param dataset The dataset index.
     *  @param legend The label for the dataset.
     */
    public synchronized void addLegend(int dataset, String legend) {
        _checkDatasetIndex(dataset);
        if (!_reuseDatasets) {
            super.addLegend(dataset, legend);
        } else {
            // If _reuseDataSets is true, then look to see if we
            // already have a dataset with the same legend.
            String possibleLegend = getLegend(dataset);
            if (possibleLegend == null
                    || (possibleLegend != null
                            && !possibleLegend.equals(legend))) {
                super.addLegend(dataset, legend);
            }
        }
    }

    /** In the specified data set, add the specified x, y point to the
     *  plot.  Data set indices begin with zero.  If the data set
     *  does not exist, create it.  The fourth argument indicates
     *  whether the point should be connected by a line to the previous
     *  point.  Regardless of the value of this argument, a line will not
     *  drawn if either there has been no previous point for this dataset
     *  or setConnected() has been called with a false argument.
     *  <p>
     *  In order to work well with swing and be thread safe, this method
     *  actually defers execution to the event dispatch thread, where
     *  all user interface actions are performed.  Thus, the point will
     *  not be added immediately (unless you call this method from within
     *  the event dispatch thread). All the methods that do this deferring
     *  coordinate so that they are executed in the order that you
     *  called them.
     *
     *  @param dataset The data set index.
     *  @param x The X position of the new point.
     *  @param y The Y position of the new point.
     *  @param connected If true, a line is drawn to connect to the previous
     *   point.
     */
    public synchronized void addPoint(final int dataset, final double x,
            final double y, final boolean connected) {
        Runnable doAddPoint = new Runnable() {
                public void run() {
                    _addPoint(dataset, x, y, 0, 0, connected, false);
                }
            };
        deferIfNecessary(doAddPoint);
    }

    /** In the specified data set, add the specified x, y point to the
     *  plot with error bars.  Data set indices begin with zero.  If
     *  the dataset does not exist, create it.  yLowEB and
     *  yHighEB are the lower and upper error bars.  The sixth argument
     *  indicates whether the point should be connected by a line to
     *  the previous point.
     *  The new point will be made visible if the plot is visible
     *  on the screen.  Otherwise, it will be drawn the next time the plot
     *  is drawn on the screen.
     *  This method is based on a suggestion by
     *  Michael Altmann <michael@email.labmed.umn.edu>.
     *  <p>
     *  In order to work well with swing and be thread safe, this method
     *  actually defers execution to the event dispatch thread, where
     *  all user interface actions are performed.  Thus, the point will
     *  not be added immediately (unless you call this method from within
     *  the event dispatch thread).  All the methods that do this deferring
     *  coordinate so that they are executed in the order that you
     *  called them.
     *
     *  @param dataset The data set index.
     *  @param x The X position of the new point.
     *  @param y The Y position of the new point.
     *  @param yLowEB The low point of the error bar.
     *  @param yHighEB The high point of the error bar.
     *  @param connected If true, a line is drawn to connect to the previous
     *   point.
     */
    public synchronized void addPointWithErrorBars(final int dataset,
            final double x, final double y, final double yLowEB,
            final double yHighEB, final boolean connected) {
        Runnable doAddPoint = new Runnable() {
                public void run() {
                    _addPoint(dataset, x, y, yLowEB, yHighEB, connected, true);
                }
            };
        deferIfNecessary(doAddPoint);
    }

    /** Clear the plot of all data points.  If the argument is true, then
     *  reset all parameters to their initial conditions, including
     *  the persistence, plotting format, and axes formats.
     *  For the change to take effect, you must call repaint().
     *  @param format If true, clear the format controls as well.
     *  <p>
     *  In order to work well with swing and be thread safe, this method
     *  actually defers execution to the event dispatch thread, where
     *  all user interface actions are performed.  Thus, the clear will
     *  not be executed immediately (unless you call this method from within
     *  the event dispatch thread).  All the methods that do this deferring
     *  coordinate so that they are executed in the order that you
     *  called them.
     */
    public synchronized void clear(final boolean format) {
        Runnable doClear = new Runnable() {
                public void run() {
                    _clear(format);
                }
            };
        deferIfNecessary(doClear);
    }

    /** Clear the plot of data points in the specified dataset.
     *  This calls repaint() to request an update of the display.
     *  <p>
     *  In order to work well with swing and be thread safe, this method
     *  actually defers execution to the event dispatch thread, where
     *  all user interface actions are performed.  Thus, the point will
     *  not be added immediately (unless you call this method from within
     *  the event dispatch thread).  If you call this method, the addPoint()
     *  method, and the erasePoint() method in any order, they are assured
     *  of being processed in the order that you called them.
     *
     *  @param dataset The dataset to clear.
     */
    public synchronized void clear(final int dataset) {
        Runnable doClear = new Runnable() {
                public void run() {
                    _clear(dataset);
                }
            };
        deferIfNecessary(doClear);
    }

    /** Erase the point at the given index in the given dataset.  If
     *  lines are being drawn, also erase the line to the next points
     *  (note: not to the previous point).  The point is not checked to
     *  see whether it is in range, so care must be taken by the caller
     *  to ensure that it is.
     *  <p>
     *  In order to work well with swing and be thread safe, this method
     *  actually defers execution to the event dispatch thread, where
     *  all user interface actions are performed.  Thus, the point will
     *  not be erased immediately (unless you call this method from within
     *  the event dispatch thread).  All the methods that do this deferring
     *  coordinate so that they are executed in the order that you
     *  called them.
     *
     *  @param dataset The data set index.
     *  @param index The index of the point to erase.
     */
    public synchronized void erasePoint(final int dataset, final int index) {
        Runnable doErasePoint = new Runnable() {
                public void run() {
                    _erasePoint(dataset, index);
                }
            };
        deferIfNecessary(doErasePoint);
    }

    /** Rescale so that the data that is currently plotted just fits.
     *  This overrides the base class method to ensure that the protected
     *  variables _xBottom, _xTop, _yBottom, and _yTop are valid.
     *  This method calls repaint(), which eventually causes the display
     *  to be updated.
     *  <p>
     *  In order to work well with swing and be thread safe, this method
     *  actually defers execution to the event dispatch thread, where
     *  all user interface actions are performed.  Thus, the fill will
     *  not occur immediately (unless you call this method from within
     *  the event dispatch thread).  All the methods that do this deferring
     *  coordinate so that they are executed in the order that you
     *  called them.
     */
    public synchronized void fillPlot() {
        Runnable doFill = new Runnable() {
                public void run() {
                    _fillPlot();
                }
            };
        deferIfNecessary(doFill);
    }

    /** Return whether the default is to connect
     *  subsequent points with a line.  If the result is false, then
     *  points are not connected.  When points are by default
     *  connected, individual points can be not connected by giving the
     *  appropriate argument to addPoint().  Also, a different default
     *  can be set for each dataset, overriding this global default.
     */
    public boolean getConnected() {
        return _connected;
    }

    /** Return whether a line will be drawn from any
     *  plotted point down to the x axis.
     *  A plot with such lines is also known as a stem plot.
     */
    public boolean getImpulses() {
        return _impulses;
    }

    /** Get the marks style, which is one of
     *  "none", "points", "dots", or "various".
     *  @return A string specifying the style for points.
     */
    public synchronized String getMarksStyle() {
        // NOTE: If the number of marks increases, we will need to do
        // something better here...
        if (_marks == 0) {
            return "none";
        } else if (_marks == 1) {
            return "points";
        } else if (_marks == 2) {
            return "dots";
        } else if (_marks == 3) {
            return "various";
        } else {
            return "pixels";
        }
    }

    /** Return the maximum number of data sets.
     *  This method is deprecated, since there is no longer an upper bound.
     *  @deprecated
     */
    public int getMaxDataSets() {
        return Integer.MAX_VALUE;
    }

    /** Return the actual number of data sets.
     *  @return The number of data sets that have been created.
     */
    public synchronized int getNumDataSets() {
        return _points.size();
    }


    /** Return false if setReuseDatasets() has not yet been called
     *  or if setReuseDatasets(false) has been called.
     *  @return false if setReuseDatasets() has not yet been called
     *  or if setReuseDatasets(false) has been called.
     *  @since Ptplot 5.3
     *  @see #setReuseDatasets(boolean)
     */
    public boolean getReuseDatasets() {
        return _reuseDatasets;
    }

    /** Read a file with the old syntax (non-XML).
     *  Override the base class to register that we are reading a new
     *  data set.
     *  @param inputStream The input stream.
     *  @exception IOException If the stream cannot be read.
     */
    public synchronized void read(InputStream inputStream) throws IOException {
        super.read(inputStream);
        _firstInSet = true;
        _sawFirstDataSet = false;
    }

    /** Turn bars on or off (for bar charts).  Note that this is a global
     *  property, not per dataset.
     *  @param on If true, turn bars on.
     */
    public void setBars(boolean on) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;
        _bars = on;
    }

    /** Turn bars on and set the width and offset.  Both are specified
     *  in units of the x axis.  The offset is the amount by which the
     *  i < sup>th</sup> data set is shifted to the right, so that it
     *  peeks out from behind the earlier data sets.
     *  @param width The width of the bars.
     *  @param offset The offset per data set.
     */
    public synchronized void setBars(double width, double offset) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;
        barWidth = width;
        _barOffset = offset;
        //_bars = true;  //Remove automatic setting
    }

    /** If the argument is true, then the default is to connect
     *  subsequent points with a line.  If the argument is false, then
     *  points are not connected.  When points are by default
     *  connected, individual points can be not connected by giving the
     *  appropriate argument to addPoint().  Also, a different default
     *  can be set for each dataset, overriding this global default.
     *  @param on If true, draw lines between points.
     *  @see #setConnected(boolean, int)
     */
    public void setConnected(boolean on) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;
        _connected = on;
    }

    /** If the first argument is true, then by default for the specified
     *  dataset, points will be connected by a line.  Otherwise, the
     *  points will not be connected. When points are by default
     *  connected, individual points can be not connected by giving the
     *  appropriate argument to addPoint().
     *  Note that this method should be called before adding any points.
     *  Note further that this method should probably be called from
     *  the event thread.
     *  @param on If true, draw lines between points.
     *  @param dataset The dataset to which this should apply.
     *  @see #setConnected(boolean)
     */
    public synchronized void setConnected(boolean on, int dataset) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;
        _checkDatasetIndex(dataset);
        Format fmt = (Format)_formats.elementAt(dataset);
        fmt.connected = on;
        fmt.connectedUseDefault = false;
    }

    /** If the argument is true, then a line will be drawn from any
     *  plotted point down to the x axis.  Otherwise, this feature is
     *  disabled.  A plot with such lines is also known as a stem plot.
     *  @param on If true, draw a stem plot.
     */
    public synchronized void setImpulses(boolean on) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;
        _impulses = on;
    }

    /** If the first argument is true, then a line will be drawn from any
     *  plotted point in the specified dataset down to the x axis.
     *  Otherwise, this feature is
     *  disabled.  A plot with such lines is also known as a stem plot.
     *  @param on If true, draw a stem plot.
     *  @param dataset The dataset to which this should apply.
     */
    public synchronized void setImpulses(boolean on, int dataset) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;
        _checkDatasetIndex(dataset);
        Format fmt = (Format)_formats.elementAt(dataset);
        fmt.impulses = on;
        fmt.impulsesUseDefault = false;
    }

    /** Set the marks style to "none", "points", "dots", or "various".
     *  In the last case, unique marks are used for the first ten data
     *  sets, then recycled.
     *  This method should be called only from the event dispatch thread.
     *  @param style A string specifying the style for points.
     */
    public synchronized void setMarksStyle(String style) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;
        if (style.equalsIgnoreCase("none")) {
            _marks = 0;
        } else if (style.equalsIgnoreCase("points")) {
            _marks = 1;
        } else if (style.equalsIgnoreCase("dots")) {
            _marks = 2;
        } else if (style.equalsIgnoreCase("various")) {
            _marks = 3;
        } else if (style.equalsIgnoreCase("pixels")) {
            _marks = 4;
        }
    }

    /** Set the marks style to "none", "points", "dots", "various",
     *  or "pixels" for the specified dataset.
     *  In the last case, unique marks are used for the first ten data
     *  sets, then recycled.
     *  @param style A string specifying the style for points.
     *  @param dataset The dataset to which this should apply.
     */
    public synchronized void setMarksStyle(String style, int dataset) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;
        _checkDatasetIndex(dataset);
        Format fmt = (Format)_formats.elementAt(dataset);
        if (style.equalsIgnoreCase("none")) {
            fmt.marks = 0;
        } else if (style.equalsIgnoreCase("points")) {
            fmt.marks = 1;
        } else if (style.equalsIgnoreCase("dots")) {
            fmt.marks = 2;
        } else if (style.equalsIgnoreCase("various")) {
            fmt.marks = 3;
        } else if (style.equalsIgnoreCase("pixels")) {
            fmt.marks = 4;
        }
        fmt.marksUseDefault = false;
    }

    /** Specify the number of data sets to be plotted together.
     *  This method is deprecated, since it is no longer necessary to
     *  specify the number of data sets ahead of time.
     *  @param numSets The number of data sets.
     *  @deprecated
     */
    public void setNumSets(int numSets) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;
        if (numSets < 1) {
            throw new IllegalArgumentException("Number of data sets ("+
                    numSets + ") must be greater than 0.");

        }
        _currentdataset = -1;
        _points.removeAllElements();
        _formats.removeAllElements();
        _prevx.removeAllElements();
        _prevy.removeAllElements();
        for (int i = 0; i < numSets; i++) {
            _points.addElement(new Vector());
            _formats.addElement(new Format());
            _prevx.addElement(new Long(0));
            _prevy.addElement(new Long(0));
        }
    }

    /** Calling this method with a positive argument sets the
     *  persistence of the plot to the given number of points.  Calling
     *  with a zero argument turns off this feature, reverting to
     *  infinite memory (unless sweeps persistence is set).  If both
     *  sweeps and points persistence are set then sweeps take
     *  precedence.
     *  <p>
     *  Setting the persistence greater than zero forces the plot to
     *  be drawn in XOR mode, which allows points to be quickly and
     *  efficiently erased.  However, there is a bug in Java (as of
     *  version 1.3), where XOR mode does not work correctly with
     *  double buffering.  Thus, if you call this with an argument
     *  greater than zero, then we turn off double buffering for this
     *  panel <i>and all of its parents</i>.  This actually happens
     *  on the next call to addPoint().
     */
    public void setPointsPersistence(int persistence) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;
        // NOTE: No file format.  It's not clear it makes sense to have one.
        _pointsPersistence = persistence;
    }

    /** If the argument is true, then datasets with the same name
     *  are merged into a single dataset.
     *  @param on If true, then merge datasets.
     *  @see #getReuseDatasets()
     */
    public void setReuseDatasets(boolean on) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;
        _reuseDatasets = on;
    }

    /** Calling this method with a positive argument sets the
     *  persistence of the plot to the given width in units of the
     *  horizontal axis. Calling
     *  with a zero argument turns off this feature, reverting to
     *  infinite memory (unless points persistence is set).  If both
     *  X and points persistence are set then both are applied,
     *  meaning that points that are old by either criterion will
     *  be erased.
     *  <p>
     *  Setting the X persistence greater than zero forces the plot to
     *  be drawn in XOR mode, which allows points to be quickly and
     *  efficiently erased.  However, there is a bug in Java (as of
     *  version 1.3), where XOR mode does not work correctly with
     *  double buffering.  Thus, if you call this with an argument
     *  greater than zero, then we turn off double buffering for this
     *  panel <i>and all of its parents</i>.  This actually happens
     *  on the next call to addPoint().
     */
    public void setXPersistence(double persistence) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;
        // NOTE: No file format.  It's not clear it makes sense to have one.
        _xPersistence = persistence;
    }

    /** Write plot data information to the specified output stream in PlotML.
     *  @param output A buffered print writer.
     */
    public synchronized void writeData(PrintWriter output) {
        super.writeData(output);
        for (int dataset = 0; dataset < _points.size(); dataset++) {

            StringBuffer options = new StringBuffer();

            Format fmt = (Format)_formats.elementAt(dataset);

            if (!fmt.connectedUseDefault) {
                if (_isConnected(dataset)) {
                    options.append(" connected=\"yes\"");
                } else {
                    options.append(" connected=\"no\"");
                }
            }

            if (!fmt.impulsesUseDefault) {
                if (fmt.impulses) options.append(" stems=\"yes\"");
                else output.println(" stems=\"no\"");
            }

            if (!fmt.marksUseDefault) {
                switch(fmt.marks) {
                case 0:
                    options.append(" marks=\"none\"");
                case 1:
                    options.append(" marks=\"points\"");
                case 2:
                    options.append(" marks=\"dots\"");
                case 3:
                    options.append(" marks=\"various\"");
                case 4:
                    options.append(" marks=\"pixels\"");
                }
            }

            String legend = getLegend(dataset);
            if (legend != null) {
                options.append(" name=\"" + getLegend(dataset) + "\"");
            }

            output.println("<dataset" + options.toString() + ">");

            // Write the data
            Vector pts = (Vector)_points.elementAt(dataset);
            for (int pointnum = 0; pointnum < pts.size(); pointnum++) {
                PlotPoint pt = (PlotPoint)pts.elementAt(pointnum);
                if (!pt.connected) {
                    output.print("<m ");
                } else {
                    output.print("<p ");
                }
                output.print("x=\"" + pt.x + "\" y=\"" + pt.y + "\"");
                if (pt.errorBar) {
                    output.print(" lowErrorBar=\"" + pt.yLowEB
                            + "\" highErrorBar=\"" + pt.yHighEB + "\"");
                }
                output.println("/>");
            }

            output.println("</dataset>");
        }
    }

    /** Write plot format information to the specified output stream in
     *  PlotML, an XML scheme.
     *  @param output A buffered print writer.
     */
    public synchronized void writeFormat(PrintWriter output) {
        super.writeFormat(output);

        if (_reuseDatasets) output.println("<reuseDatasets/>");

        StringBuffer defaults = new StringBuffer();

        if (!_connected) defaults.append(" connected=\"no\"");

        switch(_marks) {
        case 1:
            defaults.append(" marks=\"points\"");
            break;
        case 2:
            defaults.append(" marks=\"dots\"");
            break;
        case 3:
            defaults.append(" marks=\"various\"");
            break;
        case 4:
            defaults.append(" marks=\"pixels\"");
            break;
        }

        // Write the defaults for formats that can be controlled by dataset
        if (_impulses) defaults.append(" stems=\"yes\"");

        if (defaults.length() > 0) {
            output.println("<default" + defaults.toString() + "/>");
        }

        if (_bars) output.println(
                "<barGraph width=\"" + barWidth
                + "\" offset=\"" + _barOffset + "\"/>");
    }

    ///////////////////////////////////////////////////////////////////
    ////                         protected methods                 ////


    /** Check the argument to ensure that it is a valid data set index.
     *  If it is less than zero, throw an IllegalArgumentException (which
     *  is a runtime exception).  If it does not refer to an existing
     *  data set, then fill out the _points Vector so that it does refer
     *  to an existing data set. All other dataset-related vectors are
     *  similarly filled out.
     *  @param dataset The data set index.
     */
    protected synchronized void _checkDatasetIndex(int dataset) {
        if (dataset < 0) {
            throw new IllegalArgumentException("Plot._checkDatasetIndex: Cannot"
                    + " give a negative number for the data set index.");
        }
        while (dataset >= _points.size()) {
            _points.addElement(new Vector());
            _formats.addElement(new Format());
            _prevx.addElement(new Long(0));
            _prevy.addElement(new Long(0));
        }
    }

    /** Draw bar from the specified point to the y axis.
     *  If the specified point is below the y axis or outside the
     *  x range, do nothing.  If the <i>clip</i> argument is true,
     *  then do not draw above the y range.
     *  Note that paintComponent() should be called before
     *  calling this method so that _xscale and _yscale are properly set.
     *  This method should be called only from the event dispatch thread.
     *  It is not synchronized, so its caller should be.
     *  @param graphics The graphics context.
     *  @param dataset The index of the dataset.
     *  @param xpos The x position.
     *  @param ypos The y position.
     *  @param clip If true, then do not draw outside the range.
     */
    protected void _drawBar(Graphics graphics, int dataset,
            long xpos, long ypos, boolean clip) {
        if (clip) {
            if (ypos < _uly) {
                ypos = _uly;
            } if (ypos > _lry) {
                ypos = _lry;
            }
        }
        if (ypos <= _lry && xpos <= _lrx && xpos >= _ulx) {
            // left x position of bar.
            int barlx = (int)(xpos - barWidth * _xscale/2 +
                    dataset * _barOffset * _xscale);
            // right x position of bar
            int barrx = (int)(barlx + barWidth * _xscale);
            if (barlx < _ulx) barlx = _ulx;
            if (barrx > _lrx) barrx = _lrx;
            // Make sure that a bar is always at least one pixel wide.
            if (barlx >= barrx) barrx = barlx+1;
            // The y position of the zero line.
            long zeroypos = _lry - (long) ((0-_yMin) * _yscale);
            if (_lry < zeroypos) zeroypos = _lry;
            if (_uly > zeroypos) zeroypos = _uly;

            if (_yMin >= 0 || ypos <= zeroypos) {
                graphics.fillRect(barlx, (int)ypos,
                        barrx - barlx, (int)(zeroypos - ypos));
            } else {
                graphics.fillRect(barlx, (int)zeroypos,
                        barrx - barlx, (int)(ypos - zeroypos));
            }
        }
    }

    /** Draw an error bar for the specified yLowEB and yHighEB values.
     *  If the specified point is below the y axis or outside the
     *  x range, do nothing.  If the <i>clip</i> argument is true,
     *  then do not draw above the y range.
     *  This method should be called only from the event dispatch thread.
     *  It is not synchronized, so its caller should be.
     *  @param graphics The graphics context.
     *  @param dataset The index of the dataset.
     *  @param xpos The x position.
     *  @param yLowEBPos The lower y position of the error bar.
     *  @param yHighEBPos The upper y position of the error bar.
     *  @param clip If true, then do not draw above the range.
     */
    protected void _drawErrorBar(Graphics graphics, int dataset,
            long xpos, long yLowEBPos, long yHighEBPos,
            boolean clip) {
        _drawLine(graphics, dataset, xpos - _ERRORBAR_LEG_LENGTH, yHighEBPos,
                xpos + _ERRORBAR_LEG_LENGTH, yHighEBPos, clip);
        _drawLine(graphics, dataset, xpos, yLowEBPos, xpos, yHighEBPos, clip);
        _drawLine(graphics, dataset, xpos - _ERRORBAR_LEG_LENGTH, yLowEBPos,
                xpos + _ERRORBAR_LEG_LENGTH, yLowEBPos, clip);
    }

    /** Draw a line from the specified point to the y axis.
     *  If the specified point is below the y axis or outside the
     *  x range, do nothing.  If the <i>clip</i> argument is true,
     *  then do not draw above the y range.
     *  This method should be called only from the event dispatch thread.
     *  It is not synchronized, so its caller should be.
     *  @param graphics The graphics context.
     *  @param xpos The x position.
     *  @param ypos The y position.
     *  @param clip If true, then do not draw outside the range.
     */
    protected void _drawImpulse(Graphics graphics,
            long xpos, long ypos, boolean clip) {
        if (clip) {
            if (ypos < _uly) {
                ypos = _uly;
            } if (ypos > _lry) {
                ypos = _lry;
            }
        }
        if (ypos <= _lry && xpos <= _lrx && xpos >= _ulx) {
            // The y position of the zero line.
            double zeroypos = _lry - (long) ((0-_yMin) * _yscale);
            if (_lry < zeroypos) zeroypos = _lry;
            if (_uly > zeroypos) zeroypos = _uly;
            _setWidth(graphics, 1f);
            graphics.drawLine((int)xpos, (int)ypos, (int)xpos,
                    (int)zeroypos);
        }
    }

    /** Draw a line from the specified starting point to the specified
     *  ending point.  The current color is used.  If the <i>clip</i> argument
     *  is true, then draw only that portion of the line that lies within the
     *  plotting rectangle. This method draws a line one pixel wide.
     *  This method should be called only from the event dispatch thread.
     *  It is not synchronized, so its caller should be.
     *  @param graphics The graphics context.
     *  @param dataset The index of the dataset.
     *  @param startx The starting x position.
     *  @param starty The starting y position.
     *  @param endx The ending x position.
     *  @param endy The ending y position.
     *  @param clip If true, then do not draw outside the range.
     */
    protected void _drawLine(Graphics graphics,
            int dataset, long startx, long starty, long endx, long endy,
            boolean clip) {
        _drawLine(graphics, dataset, startx, starty, endx, endy, clip, 1f);
    }

    /** Draw a line from the specified starting point to the specified
     *  ending point.  The current color is used.  If the <i>clip</i> argument
     *  is true, then draw only that portion of the line that lies within the
     *  plotting rectangle.  The width argument is ignored if the graphics
     *  argument is not an instance of Graphics2D.
     *  This method should be called only from the event dispatch thread.
     *  It is not synchronized, so its caller should be.
     *  @param graphics The graphics context.
     *  @param dataset The index of the dataset.
     *  @param startx The starting x position.
     *  @param starty The starting y position.
     *  @param endx The ending x position.
     *  @param endy The ending y position.
     *  @param clip If true, then do not draw outside the range.
     *  @param width The thickness of the line.
     */
    protected void _drawLine(Graphics graphics,
            int dataset, long startx, long starty, long endx, long endy,
            boolean clip, float width) {

        _setWidth(graphics, width);
        if (clip) {
            // Rule out impossible cases.
            if (!((endx <= _ulx && startx <= _ulx) ||
                    (endx >= _lrx && startx >= _lrx) ||
                    (endy <= _uly && starty <= _uly) ||
                    (endy >= _lry && starty >= _lry))) {
                // If the end point is out of x range, adjust
                // end point to boundary.
                // The integer arithmetic has to be done with longs so as
                // to not loose precision on extremely close zooms.
                if (startx != endx) {
                    if (endx < _ulx) {
                        endy = (int)(endy + ((long)(starty - endy) *
                                (_ulx - endx))/(startx - endx));
                        endx = _ulx;
                    } else if (endx > _lrx) {
                        endy = (int)(endy + ((long)(starty - endy) *
                                (_lrx - endx))/(startx - endx));
                        endx = _lrx;
                    }
                }

                // If end point is out of y range, adjust to boundary.
                // Note that y increases downward
                if (starty != endy) {
                    if (endy < _uly) {
                        endx = (int)(endx + ((long)(startx - endx) *
                                (_uly - endy))/(starty - endy));
                        endy = _uly;
                    } else if (endy > _lry) {
                        endx = (int)(endx + ((long)(startx - endx) *
                                (_lry - endy))/(starty - endy));
                        endy = _lry;
                    }
                }

                // Adjust current point to lie on the boundary.
                if (startx != endx) {
                    if (startx < _ulx) {
                        starty = (int)(starty + ((long)(endy - starty) *
                                (_ulx - startx))/(endx - startx));
                        startx = _ulx;
                    } else if (startx > _lrx) {
                        starty = (int)(starty + ((long)(endy - starty) *
                                (_lrx - startx))/(endx - startx));
                        startx = _lrx;
                    }
                }
                if (starty != endy) {
                    if (starty < _uly) {
                        startx = (int)(startx + ((long)(endx - startx) *
                                (_uly - starty))/(endy - starty));
                        starty = _uly;
                    } else if (starty > _lry) {
                        startx = (int)(startx + ((long)(endx - startx) *
                                (_lry - starty))/(endy - starty));
                        starty = _lry;
                    }
                }
            }

            // Are the new points in range?
            if (endx >= _ulx && endx <= _lrx &&
                    endy >= _uly && endy <= _lry &&
                    startx >= _ulx && startx <= _lrx &&
                    starty >= _uly && starty <= _lry) {
                graphics.drawLine((int)startx, (int)starty,
                        (int)endx, (int)endy);
            }
        } else {
            // draw unconditionally.
            graphics.drawLine((int)startx, (int)starty,
                    (int)endx, (int)endy);
        }
    }

    /** Draw the axes and then plot all points. If the second
     *  argument is true, clear the display first.
     *  This method is called by paintComponent().
     *  To cause it to be called you would normally call repaint(),
     *  which eventually causes paintComponent() to be called.
     *  <p>
     *  Note that this is synchronized so that points are not added
     *  by other threads while the drawing is occurring.  This method
     *  should be called only from the event dispatch thread, consistent
     *  with swing policy.
     *  @param graphics The graphics context.
     *  @param clearfirst If true, clear the plot before proceeding.
     *  @param drawRectangle The Rectangle to draw in.
     */
    protected synchronized void _drawPlot(Graphics graphics,
            boolean clearfirst, Rectangle drawRectangle) {

        // We must call PlotBox._drawPlot() before calling _drawPlotPoint
        // so that _xscale and _yscale are set.
        super._drawPlot(graphics, clearfirst, drawRectangle);

        // Plot the points in reverse order so that the first colors
        // appear on top.
        for (int dataset = _points.size() - 1; dataset >= 0 ; dataset--) {
            Vector data = (Vector)_points.elementAt(dataset);
            for (int pointnum = 0; pointnum < data.size(); pointnum++) {
                _drawPlotPoint(graphics, dataset, pointnum);
            }
        }
        _showing = true;
    }

    /** Put a mark corresponding to the specified dataset at the
     *  specified x and y position. The mark is drawn in the current
     *  color. What kind of mark is drawn depends on the _marks
     *  variable and the dataset argument. If the fourth argument is
     *  true, then check the range and plot only points that
     *  are in range.
     *  This method should be called only from the event dispatch thread.
     *  It is not synchronized, so its caller should be.
     *  @param graphics The graphics context.
     *  @param dataset The index of the dataset.
     *  @param xpos The x position.
     *  @param ypos The y position.
     *  @param clip If true, then do not draw outside the range.
     */
    protected void _drawPoint(Graphics graphics,
            int dataset, long xpos, long ypos,
            boolean clip) {

        // If the point is not out of range, draw it.
        boolean pointinside = ypos <= _lry && ypos >= _uly &&
            xpos <= _lrx && xpos >= _ulx;
        if (!clip || pointinside) {
            int xposi = (int)xpos;
            int yposi = (int)ypos;

            // Check to see whether the dataset has a marks directive
            Format fmt = (Format)_formats.elementAt(dataset);
            int marks = _marks;
            if (!fmt.marksUseDefault) marks = fmt.marks;

            // If the point is out of range, and being drawn, then it is
            // probably a legend point.  When printing in black and white,
            // we want to use a line rather than a point for the legend.
            // (So that line patterns are visible). The only exception is
            // when the marks style uses distinct marks, or if there is
            // no line being drawn.
            // NOTE: It is unfortunate to have to test the class of graphics,
            // but there is no easy way around this that I can think of.
            if (!pointinside && marks != 3 && _isConnected(dataset) &&
                    (graphics instanceof EPSGraphics)) {
                graphics.drawLine(xposi-6, yposi, xposi+6, yposi);
            } else {
                // Color display.  Use normal legend.
                switch (marks) {
                case 0:
                    // If no mark style is given, draw a filled rectangle.
                    // This is used, for example, to draw the legend.
                    graphics.fillRect(xposi-6, yposi-6, 6, 6);
                    break;
                case 1:
                    // points -- use 3-pixel ovals.
                    graphics.fillOval(xposi-1, yposi-1, 3, 3);
                    break;
                case 2:
                    // dots
                    graphics.fillOval(xposi-_radius, yposi-_radius,
                            _diameter, _diameter);
                    break;
                case 3:
                    // various
                    int xpoints[], ypoints[];
                    // Points are only distinguished up to _MAX_MARKS data sets.
                    int mark = dataset % _MAX_MARKS;
                    switch (mark) {
                    case 0:
                        // filled circle
                        graphics.fillOval(xposi-_radius, yposi-_radius,
                                _diameter, _diameter);
                        break;
                    case 1:
                        // cross
                        graphics.drawLine(xposi-_radius, yposi-_radius,
                                xposi+_radius, yposi+_radius);
                        graphics.drawLine(xposi+_radius, yposi-_radius,
                                xposi-_radius, yposi+_radius);
                        break;
                    case 2:
                        // square
                        graphics.drawRect(xposi-_radius, yposi-_radius,
                                _diameter, _diameter);
                        break;
                    case 3:
                        // filled triangle
                        xpoints = new int[4];
                        ypoints = new int[4];
                        xpoints[0] = xposi; ypoints[0] = yposi-_radius;
                        xpoints[1] = xposi+_radius; ypoints[1] = yposi+_radius;
                        xpoints[2] = xposi-_radius; ypoints[2] = yposi+_radius;
                        xpoints[3] = xposi; ypoints[3] = yposi-_radius;
                        graphics.fillPolygon(xpoints, ypoints, 4);
                        break;
                    case 4:
                        // diamond
                        xpoints = new int[5];
                        ypoints = new int[5];
                        xpoints[0] = xposi; ypoints[0] = yposi-_radius;
                        xpoints[1] = xposi+_radius; ypoints[1] = yposi;
                        xpoints[2] = xposi; ypoints[2] = yposi+_radius;
                        xpoints[3] = xposi-_radius; ypoints[3] = yposi;
                        xpoints[4] = xposi; ypoints[4] = yposi-_radius;
                        graphics.drawPolygon(xpoints, ypoints, 5);
                        break;
                    case 5:
                        // circle
                        graphics.drawOval(xposi-_radius, yposi-_radius,
                                _diameter, _diameter);
                        break;
                    case 6:
                        // plus sign
                        graphics.drawLine(xposi, yposi-_radius, xposi,
                                yposi+_radius);
                        graphics.drawLine(xposi-_radius, yposi, xposi+_radius,
                                yposi);
                        break;
                    case 7:
                        // filled square
                        graphics.fillRect(xposi-_radius, yposi-_radius,
                                _diameter, _diameter);
                        break;
                    case 8:
                        // triangle
                        xpoints = new int[4];
                        ypoints = new int[4];
                        xpoints[0] = xposi; ypoints[0] = yposi-_radius;
                        xpoints[1] = xposi+_radius; ypoints[1] = yposi+_radius;
                        xpoints[2] = xposi-_radius; ypoints[2] = yposi+_radius;
                        xpoints[3] = xposi; ypoints[3] = yposi-_radius;
                        graphics.drawPolygon(xpoints, ypoints, 4);
                        break;
                    case 9:
                        // filled diamond
                        xpoints = new int[5];
                        ypoints = new int[5];
                        xpoints[0] = xposi; ypoints[0] = yposi-_radius;
                        xpoints[1] = xposi+_radius; ypoints[1] = yposi;
                        xpoints[2] = xposi; ypoints[2] = yposi+_radius;
                        xpoints[3] = xposi-_radius; ypoints[3] = yposi;
                        xpoints[4] = xposi; ypoints[4] = yposi-_radius;
                        graphics.fillPolygon(xpoints, ypoints, 5);
                        break;
                    }
                    break;
                case 4:
                    // If the mark style is pixels, draw a filled rectangle.
                    graphics.fillRect(xposi, yposi, 1, 1);
                    break;
                default:
                    // none
                }
            }
        }
    }

    /** Parse a line that gives plotting information. Return true if
     *  the line is recognized.  Lines with syntax errors are ignored.
     *  It is not synchronized, so its caller should be.
     *  @param line A command line.
     *  @return True if the line is recognized.
     */
    protected boolean _parseLine(String line) {
        boolean connected = false;
        if (_isConnected(_currentdataset)) {
            connected = true;
        }
        // parse only if the super class does not recognize the line.
        if (super._parseLine(line)) {
            return true;
        } else {
            // We convert the line to lower case so that the command
            // names are case insensitive
            String lcLine = new String(line.toLowerCase());
            if (lcLine.startsWith("marks:")) {
                // If we have seen a dataset directive, then apply the
                // request to the current dataset only.
                String style = (line.substring(6)).trim();
                if (_sawFirstDataSet) {
                    setMarksStyle(style, _currentdataset);
                } else {
                    setMarksStyle(style);
                }
                return true;
            } else if (lcLine.startsWith("numsets:")) {
                // Ignore.  No longer relevant.
                return true;
            } else if (lcLine.startsWith("reusedatasets:")) {
                if (lcLine.indexOf("off", 16) >= 0) {
                    setReuseDatasets(false);
                } else {
                    setReuseDatasets(true);
                }
                return true;
            } else if (lcLine.startsWith("dataset:")) {
                if (_reuseDatasets && lcLine.length() > 0) {
                    String tlegend = (line.substring(8)).trim();
                    _currentdataset = -1;
                    int i;
                    for ( i = 0; i <= _maxDataset; i++) {
                        if (getLegend(i).compareTo(tlegend) == 0) {
                            _currentdataset = i;
                        }
                    }
                    if (_currentdataset != -1) {
                        return true;
                    } else {
                        _currentdataset = _maxDataset;
                    }
                }

                // new data set
                _firstInSet = true;
                _sawFirstDataSet = true;
                _currentdataset++;
                if (lcLine.length() > 0) {
                    String legend = (line.substring(8)).trim();
                    if (legend != null && legend.length() > 0) {
                        addLegend(_currentdataset, legend);
                    }
                }
                _maxDataset = _currentdataset;
                return true;
            } else if (lcLine.startsWith("lines:")) {
                if (_sawFirstDataSet) {
                    // Backward compatbility with xgraph here.
                    // If we see some data sets, then they are drawn
                    // with lines, if we then see a Lines: off
                    // the current dataset and succeeding datasets
                    // will be drawn without lines.

                    // For each of the existing datasets, if
                    // it fmt.connectedUseDefault is true, then
                    // set fmt.connectedUseDefault to false and set
                    // the value of fmt.connected
                    Enumeration formats = _formats.elements();
                    while (formats.hasMoreElements()) {
                        Format format = (Format) formats.nextElement();
                        if (format.connectedUseDefault) {
                            format.connectedUseDefault = false;
                            format.connected = _connected;
                        }
                    }

                }

                if (lcLine.indexOf("off", 6) >= 0) {
                    setConnected(false);
                } else {
                    setConnected(true);
                }
                return true;
            } else if (lcLine.startsWith("impulses:")) {
                // If we have not yet seen a dataset, then this is interpreted
                // as the global default.  Otherwise, it is assumed to apply
                // only to the current dataset.
                if (_sawFirstDataSet) {
                    if (lcLine.indexOf("off", 9) >= 0) {
                        setImpulses(false, _currentdataset);
                    } else {
                        setImpulses(true, _currentdataset);
                    }
                } else {
                    if (lcLine.indexOf("off", 9) >= 0) {
                        setImpulses(false);
                    } else {
                        setImpulses(true);
                    }
                }
                return true;
            } else if (lcLine.startsWith("bars:")) {
                if (lcLine.indexOf("off", 5) >= 0) {
                    setBars(false);
                } else {
                    setBars(true);
                    int comma = line.indexOf(",", 5);
                    String barwidth;
                    String baroffset = null;
                    if (comma > 0) {
                        barwidth = (line.substring(5, comma)).trim();
                        baroffset = (line.substring(comma+1)).trim();
                    } else {
                        barwidth = (line.substring(5)).trim();
                    }
                    try {
                        Double bwidth = new Double(barwidth);
                        double boffset = _barOffset;
                        if (baroffset != null) {
                            boffset = (new Double(baroffset)).
                                doubleValue();
                        }
                        setBars(bwidth.doubleValue(), boffset);
                    } catch (NumberFormatException e) {
                        // ignore if format is bogus.
                    }
                }
                return true;
            } else if (line.startsWith("move:")) {
                // a disconnected point
                connected = false;
                // deal with 'move: 1 2' and 'move:2 2'
                line = line.substring(5, line.length()).trim();
            } else if (line.startsWith("move")) {
                // a disconnected point
                connected = false;
                // deal with 'move 1 2' and 'move2 2'
                line = line.substring(4, line.length()).trim();
            } else if (line.startsWith("draw:")) {
                // a connected point, if connect is enabled.
                line = line.substring(5, line.length()).trim();
            } else if (line.startsWith("draw")) {
                // a connected point, if connect is enabled.
                line = line.substring(4, line.length()).trim();
            }
            line = line.trim();

            // We can't use StreamTokenizer here because it can't
            // process numbers like 1E-01.
            // This code is somewhat optimized for speed, since
            // most data consists of two data points, we want
            // to handle that case as efficiently as possible.

            int fieldsplit = line.indexOf(",");
            if (fieldsplit == -1) {
                fieldsplit = line.indexOf(" ");
            }
            if (fieldsplit == -1) {
                fieldsplit = line.indexOf("\t");  // a tab
            }

            if (fieldsplit > 0) {
                String x = (line.substring(0, fieldsplit)).trim();
                String y = (line.substring(fieldsplit+1)).trim();
                // Any more separators?
                int fieldsplit2 = y.indexOf(",");
                if (fieldsplit2 == -1) {
                    fieldsplit2 = y.indexOf(" ");
                }
                if (fieldsplit2 == -1) {
                    fieldsplit2 = y.indexOf("\t");  // a tab
                }
                if (fieldsplit2 > 0) {
                    line = (y.substring(fieldsplit2+1)).trim();
                    y = (y.substring(0, fieldsplit2)).trim();
                }
                try {
                    Double xpt = new Double(x);
                    Double ypt = new Double(y);
                    if (fieldsplit2 > 0) {
                        // There was one separator after the y value, now
                        // look for another separator.
                        int fieldsplit3 = line.indexOf(",");
                        if (fieldsplit3 == -1) {
                            fieldsplit3 = line.indexOf(" ");
                        }
                        if (fieldsplit3 == -1) {
                            fieldsplit2 = line.indexOf("\t");  // a tab
                        }

                        if (fieldsplit3 > 0) {
                            // We have more numbers, assume that this is
                            // an error bar
                            String yl = (line.substring(0,
                                    fieldsplit3)).trim();
                            String yh = (line.substring(fieldsplit3+1)).trim();
                            Double yLowEB = new Double(yl);
                            Double yHighEB = new Double(yh);
                            connected = _addLegendIfNecessary(connected);
                            addPointWithErrorBars(_currentdataset,
                                    xpt.doubleValue(),
                                    ypt.doubleValue(),
                                    yLowEB.doubleValue(),
                                    yHighEB.doubleValue(),
                                    connected);
                            return true;
                        } else {
                            // It is unlikely that we have a fieldsplit2 >0
                            // but not fieldsplit3 >0, but just in case:

                            connected = _addLegendIfNecessary(connected);
                            addPoint(_currentdataset, xpt.doubleValue(),
                                    ypt.doubleValue(), connected);
                            return true;
                        }
                    } else {
                        // There were no more fields, so this is
                        // a regular pt.
                        connected = _addLegendIfNecessary(connected);
                        addPoint(_currentdataset, xpt.doubleValue(),
                                ypt.doubleValue(), connected);
                        return true;
                    }
                } catch (NumberFormatException e) {
                    // ignore if format is bogus.
                }
            }
        }
        return false;
    }

    /** If the graphics argument is an instance of Graphics2D, then set
     *  the current stroke to the specified width.  Otherwise, do nothing.
     *  @param graphics The graphics object.
     *  @param width The width.
     */
    protected void _setWidth(Graphics graphics, float width) {
        // For historical reasons, the API here only assumes Graphics
        // objects, not Graphics2D.
        if (graphics instanceof Graphics2D) {
            // We cache the two most common cases.
            if (width == 1f) {
                ((Graphics2D)graphics).setStroke(_lineStroke1);
            } else if (width == 2f) {
                ((Graphics2D)graphics).setStroke(_lineStroke2);
            } else {
                ((Graphics2D)graphics).setStroke(new BasicStroke(
                        width, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND));
            }
        }
    }

    ///////////////////////////////////////////////////////////////////
    ////                         protected variables               ////

    /** @serial The current dataset. */
    protected int _currentdataset = -1;

    /** @serial A vector of datasets. */
    protected Vector _points = new Vector();

    /** @serial An indicator of the marks style.  See _parseLine method for
     * interpretation.
     */
    protected int _marks;

    ///////////////////////////////////////////////////////////////////
    ////                         private methods                   ////

    /* Add a legend if necessary, return the value of the connected flag.
     */
    private boolean _addLegendIfNecessary(boolean connected) {
        if ((! _sawFirstDataSet  || _currentdataset < 0) &&
                ! _reuseDatasets) {
            // We did not set a DataSet line, but
            // we did get called with -<digit> args and
            // we did not see reusedatasets: yes
            _sawFirstDataSet = true;
            _currentdataset++;
        }
        if (! _sawFirstDataSet && getLegend(_currentdataset) == null) {
            // We did not see a "DataSet" string yet,
            // nor did we call addLegend().
            _firstInSet = true;
            _sawFirstDataSet = true;
            addLegend(_currentdataset,
                    new String("Set "+ _currentdataset));
        }
        if (_firstInSet && ! _reuseDatasets) {
            connected = false;
            _firstInSet = false;
        }
        return connected;
    }

    /* In the specified data set, add the specified x, y point to the
     * plot.  Data set indices begin with zero.  If the dataset
     * argument is less than zero, throw an IllegalArgumentException
     * (a runtime exception).  If it refers to a data set that does
     * not exist, create the data set.  The fourth argument indicates
     * whether the point should be connected by a line to the previous
     * point.  However, this argument is ignored if setConnected() has
     * been called with a false argument.  In that case, a point is never
     * connected to the previous point.  That argument is also ignored
     * if the point is the first in the specified dataset.
     * The point is drawn on the screen only if is visible.
     * Otherwise, it is drawn the next time paintComponent() is called.
     *
     * This is not synchronized, so the caller should be.  Moreover, this
     * should only be called in the event dispatch thread. It should only
     * be called via deferIfNecessary().
     */
    private void _addPoint(
            int dataset, double x, double y, double yLowEB, double yHighEB,
            boolean connected, boolean errorBar) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;

        _checkDatasetIndex(dataset);

        if (_xlog) {
            if (x <= 0.0) {
                System.err.println("Can't plot non-positive X values "+
                        "when the logarithmic X axis value is specified: " +
                        x);
                return;
            }
            x = Math.log(x)*_LOG10SCALE;
        }
        if (_ylog) {
            if (y <= 0.0) {
                System.err.println("Can't plot non-positive Y values "+
                        "when the logarithmic Y axis value is specified: " +
                        y);
                return;
            }
            y = Math.log(y)*_LOG10SCALE;
            if (errorBar) {
                if (yLowEB <= 0.0 || yHighEB <= 0.0) {
                    System.err.println("Can't plot non-positive Y values "+
                            "when the logarithmic Y axis value is specified: " +
                            y);
                    return;
                }
                yLowEB = Math.log(yLowEB)*_LOG10SCALE;
                yHighEB = Math.log(yHighEB)*_LOG10SCALE;
            }
        }

        Vector pts = (Vector)_points.elementAt(dataset);

        // If X persistence has been set, then delete any old points.
        if (_xPersistence > 0.0) {
            int numToDelete = 0;
            while (numToDelete < pts.size()) {
                PlotPoint old = (PlotPoint)(pts.elementAt(numToDelete));
                if (x - old.originalx <= _xPersistence) break;
                numToDelete++;
            }
            for (int i = 0; i < numToDelete; i++) {
                erasePoint(dataset, 0);
            }
        }

        // Get the new size after deletions.
        int size = pts.size();

        PlotPoint pt = new PlotPoint();
        // Original value of x before wrapping.
        pt.originalx = x;

        // Modify x if wrapping.
        if (_wrap) {
            double width = _wrapHigh - _wrapLow;
            if (x < _wrapLow) {
                x += width*Math.floor(1.0 + (_wrapLow-x)/width);
            } else if (x > _wrapHigh) {
                x -= width*Math.floor(1.0 + (x-_wrapHigh)/width);
                // NOTE: Could quantization errors be a problem here?
                if (x == _wrapLow) x = _wrapHigh;
            }
        }

        // For auto-ranging, keep track of min and max.
        if (x < _xBottom) _xBottom = x;
        if (x > _xTop) _xTop = x;
        if (y < _yBottom) _yBottom = y;
        if (y > _yTop) _yTop = y;

        pt.x = x;
        pt.y = y;
        pt.connected = connected && _isConnected(dataset);

        if (errorBar) {
            if (yLowEB < _yBottom) _yBottom = yLowEB;
            if (yLowEB > _yTop) _yTop = yLowEB;
            if (yHighEB < _yBottom) _yBottom = yHighEB;
            if (yHighEB > _yTop) _yTop = yHighEB;
            pt.yLowEB = yLowEB;
            pt.yHighEB = yHighEB;
            pt.errorBar = true;
        }

        // If this is the first point in the dataset, clear the connected bit.
        if (size == 0) {
            pt.connected = false;
        } else if (_wrap) {
            // Do not connect points if wrapping...
            PlotPoint old = (PlotPoint)(pts.elementAt(size-1));
            if (old.x > x) pt.connected = false;
        }
        pts.addElement(pt);
        // If points persistence has been set, then delete one old point.
        if (_pointsPersistence > 0) {
            if (size > _pointsPersistence) erasePoint(dataset, 0);
        }
        // Draw the point on the screen only if the plot is showing.
        Graphics graphics = getGraphics();
        // Need to check that graphics is not null because plot may have
        // been dismissed.
        if (_showing && graphics != null) {

            if ((_pointsPersistence > 0 || _xPersistence > 0.0)
                    && isDoubleBuffered()) {
                // NOTE: Double buffering has a bug in Java (in at least
                // version 1.3) where there is a one pixel alignment problem
                // that prevents XOR drawing from working correctly.
                // XOR drawing is used for live plots, and if double buffering
                // is turned on, then cruft is left on the screen whenever the
                // fill or zoom functions are used.
                // Here, if it hasn't been done already, we turn off double
                // buffering on this panel and all its parents for which this
                // is possible.  Note that we could do this globally using
                //
                // RepaintManager repaintManager
                //        = RepaintManager.currentManager(this);
                // repaintManager.setDoubleBufferingEnabled(false);
                //
                // However, that turns off double buffering in all windows
                // of the application, which means that other windows that only
                // work properly with double buffering (such as vergil windows)
                // will not work.
                //
                // NOTE: This fix creates another problem...
                // If there are other widgets besides the plotter in the
                // same top-level window, and they implement double
                // buffering (which they will by default), then they
                // need to be opaque or drawing artifacts will appear
                // upon exposure events.  The workaround is simple:
                // Make these other objects opaque, and set their
                // background color appropriately.
                //
                // See:
                // <pre>
                // http://developer.java.sun.com/developer/bugParade/bugs/
                //     4188795.html
                //     4204551.html
                //     4295712.htm
                // </pre>
                //
                // Since we are assured of being in the event dispatch thread,
                // we can simply execute this.
                setDoubleBuffered(false);
                Component parent = getParent();
                while (parent != null) {
                    if (parent instanceof JComponent) {
                        ((JComponent)parent).setDoubleBuffered(false);
                    }
                    parent = parent.getParent();
                }
            }

            // Again, we are in the event thread, so this is safe...
            _drawPlotPoint(graphics, dataset, pts.size() - 1);
        }

        if (_wrap && x == _wrapHigh) {
            // Plot a second point at the low end of the range.
            _addPoint(dataset, _wrapLow, y, yLowEB, yHighEB, false, errorBar);
        }
    }

    /* Clear the plot of all data points.  If the argument is true, then
     * reset all parameters to their initial conditions, including
     * the persistence, plotting format, and axes formats.
     * For the change to take effect, you must call repaint().
     *
     * This is not synchronized, so the caller should be.  Moreover, this
     * should only be called in the event dispatch thread. It should only
     * be called via deferIfNecessary().
     */
    private void _clear(boolean format) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;
        super.clear(format);
        _currentdataset = -1;
        int size = _points.size();
        _points = new Vector();
        _prevx = new Vector();
        _prevy = new Vector();
        _maxDataset = -1;
        _firstInSet = true;
        _sawFirstDataSet = false;
        _xyInvalid = true;
        _filename = null;

        if (format) {
            _showing = false;
            // Reset format controls
            _formats = new Vector();
            _marks = 0;
            _pointsPersistence = 0;
            _xPersistence = 0;
            _bars = false;
            barWidth = 0.5;
            _barOffset = 0.05;
            _connected = true;
            _impulses = false;
            _reuseDatasets = false;
        }
    }

    /** Clear the plot of data points in the specified dataset.
     *  This calls repaint() to request an update of the display.
     *
     * This is not synchronized, so the caller should be.  Moreover, this
     * should only be called in the event dispatch thread. It should only
     * be called via deferIfNecessary().
     */
    private void _clear(int dataset) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;
        _checkDatasetIndex(dataset);
        _xyInvalid = true;
        Vector points = (Vector)_points.elementAt(dataset);
        // Vector.clear() is new in JDK1.2, so we use just
        // create a new Vector here so that we can compile
        // this with JDK1.1 for use in JDK1.1 browsers
        _points.setElementAt(new Vector(), dataset);
        repaint();
    }

    /* Draw the specified point and associated lines, if any.
     * Note that paintComponent() should be called before
     * calling this method so that it calls _drawPlot(), which sets
     * _xscale and _yscale. Note that this does not check the dataset
     * index.  It is up to the caller to do that.
     *
     * Note that this method is not synchronized, so the caller should be.
     * Moreover this method should always be called from the event thread
     * when being used to write to the screen.
     */
    private void _drawPlotPoint(Graphics graphics,
            int dataset, int index) {
        if (_pointsPersistence > 0 || _xPersistence > 0.0) {
            // To allow erasing to work by just redrawing the points.
            if (_background == null) {
                // java.awt.Component.setBackground(color) says that
                // if the color "parameter is null then this component
                // will inherit the  background color of its parent."
                graphics.setXORMode(getBackground());
            } else {
                graphics.setXORMode(_background);
            }
        }
        // Set the color
        if (_usecolor) {
            int color = dataset % _colors.length;
            graphics.setColor(_colors[color]);
        } else {
            graphics.setColor(_foreground);
        }

        Vector pts = (Vector)_points.elementAt(dataset);
        PlotPoint pt = (PlotPoint)pts.elementAt(index);
        // Use long here because these numbers can be quite large
        // (when we are zoomed out a lot).
        long ypos = _lry - (long)((pt.y - _yMin) * _yscale);
        long xpos = _ulx + (long)((pt.x - _xMin) * _xscale);

        // Draw the line to the previous point.
        long prevx = ((Long)_prevx.elementAt(dataset)).longValue();
        long prevy = ((Long)_prevy.elementAt(dataset)).longValue();
        // MIN_VALUE is a flag that there has been no previous x or y.
        if (pt.connected) {
            _drawLine(graphics, dataset, xpos, ypos, prevx, prevy, true, 2f);
        }

        // Save the current point as the "previous" point for future
        // line drawing.
        _prevx.setElementAt(new Long(xpos), dataset);
        _prevy.setElementAt(new Long(ypos), dataset);

        // Draw decorations that may be specified on a per-dataset basis
        Format fmt = (Format)_formats.elementAt(dataset);
        if (fmt.impulsesUseDefault) {
            if (_impulses) _drawImpulse(graphics, xpos, ypos, true);
        } else {
            if (fmt.impulses) _drawImpulse(graphics, xpos, ypos, true);
        }

        // Check to see whether the dataset has a marks directive
        int marks = _marks;
        if (!fmt.marksUseDefault) marks = fmt.marks;
        if (marks != 0) _drawPoint(graphics, dataset, xpos, ypos, true);

        if (_bars) _drawBar(graphics, dataset, xpos, ypos, true);
        if (pt.errorBar)
            _drawErrorBar(graphics, dataset, xpos,
                    _lry - (long)((pt.yLowEB - _yMin) * _yscale),
                    _lry - (long)((pt.yHighEB - _yMin) * _yscale), true);

        // Restore the color, in case the box gets redrawn.
        graphics.setColor(_foreground);
        if (_pointsPersistence > 0 || _xPersistence > 0.0) {
            // Restore paint mode in case axes get redrawn.
            graphics.setPaintMode();
        }
    }

    /* Erase the point at the given index in the given dataset.  If
     * lines are being drawn, also erase the line to the next points
     * (note: not to the previous point).
     *
     * This is not synchronized, so the caller should be.  Moreover, this
     * should only be called in the event dispatch thread. It should only
     * be called via deferIfNecessary().
     */
    private void _erasePoint(int dataset, int index) {
        // Ensure replot of offscreen buffer.
        _plotImage = null;

        _checkDatasetIndex(dataset);
        // Plot has probably been dismissed.  Return.
        Graphics graphics = getGraphics();
        // Need to check that graphics is not null because plot may have
        // been dismissed.
        if (_showing && graphics != null) {
            // Set the color
            if (_pointsPersistence > 0 || _xPersistence > 0.0) {
                // To allow erasing to work by just redrawing the points.
                if (_background == null) {
                    graphics.setXORMode(getBackground());
                } else {
                    graphics.setXORMode(_background);
                }
            }
            if (_usecolor) {
                int color = dataset % _colors.length;
                graphics.setColor(_colors[color]);
            } else {
                graphics.setColor(_foreground);
            }

            Vector pts = (Vector)_points.elementAt(dataset);
            PlotPoint pt = (PlotPoint)pts.elementAt(index);
            long ypos = _lry - (long) ((pt.y - _yMin) * _yscale);
            long xpos = _ulx + (long) ((pt.x - _xMin) * _xscale);

            // Erase line to the next point, if appropriate.
            if (index < pts.size() - 1) {
                PlotPoint nextp = (PlotPoint)pts.elementAt(index+1);
                int nextx = _ulx + (int) ((nextp.x - _xMin) * _xscale);
                int nexty = _lry - (int) ((nextp.y - _yMin) * _yscale);
                // NOTE: I have no idea why I have to give this point backwards.
                if (nextp.connected) _drawLine(graphics, dataset,
                        nextx, nexty,  xpos, ypos, true, 2f);
                nextp.connected = false;
            }

            // Draw decorations that may be specified on a per-dataset basis
            Format fmt = (Format)_formats.elementAt(dataset);
            if (fmt.impulsesUseDefault) {
                if (_impulses) _drawImpulse(graphics, xpos, ypos, true);
            } else {
                if (fmt.impulses) _drawImpulse(graphics, xpos, ypos, true);
            }

            // Check to see whether the dataset has a marks directive
            int marks = _marks;
            if (!fmt.marksUseDefault) marks = fmt.marks;
            if (marks != 0) _drawPoint(graphics, dataset, xpos, ypos, true);

            if (_bars) _drawBar(graphics, dataset, xpos, ypos, true);
            if (pt.errorBar)
                _drawErrorBar(graphics, dataset, xpos,
                        _lry - (long)((pt.yLowEB - _yMin) * _yscale),
                        _lry - (long)((pt.yHighEB - _yMin) * _yscale), true);

            // Restore the color, in case the box gets redrawn.
            graphics.setColor(_foreground);
            if (_pointsPersistence > 0 || _xPersistence > 0.0) {
                // Restore paint mode in case axes get redrawn.
                graphics.setPaintMode();
            }
        }

        // The following is executed whether the plot is showing or not.
        // Remove the point from the model.
        Vector points = (Vector)_points.elementAt(dataset);
        if (points != null) {
            // If this point is at the maximum or minimum x or y boundary,
            // then flag that boundary needs to be recalculated next time
            // fillPlot() is called.
            PlotPoint pt = (PlotPoint)points.elementAt(index);
            if (pt != null) {
                if (pt.x == _xBottom || pt.x == _xTop ||
                        pt.y == _yBottom || pt.y == _yTop) {
                    _xyInvalid = true;
                }

                points.removeElementAt(index);
            }
        }
    }

    /* Rescale so that the data that is currently plotted just fits.
     * This overrides the base class method to ensure that the protected
     * variables _xBottom, _xTop, _yBottom, and _yTop are valid.
     * This method calls repaint(), which causes the display
     * to be updated.
     *
     * This is not synchronized, so the caller should be.  Moreover, this
     * should only be called in the event dispatch thread. It should only
     * be called via deferIfNecessary().
     */
    private void _fillPlot() {
        if (_xyInvalid) {
            // Recalculate the boundaries based on currently visible data
            _xBottom = Double.MAX_VALUE;
            _xTop = - Double.MAX_VALUE;
            _yBottom = Double.MAX_VALUE;
            _yTop = - Double.MAX_VALUE;
            for (int dataset = 0; dataset < _points.size(); dataset++) {
                Vector points = (Vector)_points.elementAt(dataset);
                for (int index = 0; index < points.size(); index++) {
                    PlotPoint pt = (PlotPoint)points.elementAt(index);
                    if (pt.x < _xBottom) _xBottom = pt.x;
                    if (pt.x > _xTop) _xTop = pt.x;
                    if (pt.y < _yBottom) _yBottom = pt.y;
                    if (pt.y > _yTop) _yTop = pt.y;
                }
            }
        }
        _xyInvalid = false;
        // If this is a bar graph, then make sure the Y range includes 0
        if (_bars) {
            if (_yBottom > 0.0) _yBottom = 0.0;
            if (_yTop < 0.0) _yTop = 0.0;
        }
        super.fillPlot();
    }

    // Return true if the specified dataset is connected by default.
    private boolean _isConnected(int dataset) {
        if (dataset < 0) return _connected;
        _checkDatasetIndex(dataset);
        Format fmt = (Format)_formats.elementAt(dataset);
        if (fmt.connectedUseDefault) {
            return _connected;
        } else {
            return fmt.connected;
        }
    }

    /**
     * Returns the bar width in x-axis units.
     *
     * @return the bar width
     */

    protected double getBarWidth() {
        return barWidth;
    }

    ///////////////////////////////////////////////////////////////////
    ////                         private variables                 ////

    /** @serial Number of points to persist for. */
    private int _pointsPersistence = 0;

    /** @serial Persistence in units of the horizontal axis. */
    private double _xPersistence = 0.0;

    /** @serial True if this is a bar plot. */
    private boolean _bars = false;

    /** @serial Width of a bar in x axis units. */
    private double barWidth = 0.5;

    /** @serial Offset per dataset in x axis units. */
    private double _barOffset = 0.05;

    /** @serial True if the points are connected. */
    private boolean _connected = true;

    /** @serial True if this is an impulse plot. */
    private boolean _impulses = false;

    /** @serial The highest data set used. */
    private int _maxDataset = -1;

    /** @serial True if we saw 'reusedatasets: on' in the file. */
    private boolean _reuseDatasets = false;

    /** @serial Is this the first datapoint in a set? */
    private boolean _firstInSet = true;

    /** @serial Have we seen a DataSet line in the current data file? */
    private boolean _sawFirstDataSet = false;

    /** @serial Give the radius of a point for efficiency. */
    private int _radius = 3;

    /** @serial Give the diameter of a point for efficiency. */
    private int _diameter = 6;

    /** @serial Information about the previously plotted point. */
    private Vector _prevx = new Vector(), _prevy = new Vector();

    // Half of the length of the error bar horizontal leg length;
    private static final int _ERRORBAR_LEG_LENGTH = 5;

    // Maximum number of different marks
    // NOTE: There are 11 colors in the base class.  Combined with 10
    // marks, that makes 110 unique signal identities.
    private static final int _MAX_MARKS = 10;

    // A stroke of width 1.
    private static final BasicStroke _lineStroke1 = new BasicStroke(
            1f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND);

    // A stroke of width 2.
    private static final BasicStroke _lineStroke2 = new BasicStroke(
            2f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND);

    // The stroke to use for thin lines in the plot.
    private static final BasicStroke _thinStroke = new BasicStroke(
            1f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND);

    /** @serial Flag indicating validity of _xBottom, _xTop,
     *  _yBottom, and _yTop.
     */
    private boolean _xyInvalid = true;

    /** @serial Last filename seen in command-line arguments. */
    private String _filename = null;

    /** @serial Set by _drawPlot(), and reset by clear(). */
    private boolean _showing = false;

    /** @serial Format information on a per data set basis. */
    private Vector _formats = new Vector();

    ///////////////////////////////////////////////////////////////////
    ////                         inner classes                     ////

    private class Format implements Serializable {
        // Indicate whether the current dataset is connected.
        public boolean connected;

        // Indicate whether the above variable should be ignored.
        public boolean connectedUseDefault = true;

        // Indicate whether a stem plot should be drawn for this data set.
        // This is ignored unless the following variable is set to false.
        public boolean impulses;

        // Indicate whether the above variable should be ignored.
        public boolean impulsesUseDefault = true;

        // Indicate what type of mark to use.
        // This is ignored unless the following variable is set to false.
        public int marks;

        // Indicate whether the above variable should be ignored.
        public boolean marksUseDefault = true;
    }
}
