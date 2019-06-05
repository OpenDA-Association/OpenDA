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

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

//////////////////////////////////////////////////////////////////////////
//// Query
/**
Create a query with various types of entry boxes and controls.  Each type
of entry box has a colon and space appended to the end of its label, to
ensure uniformity.
Here is one example of creating a query with a radio button:
<pre>
    query = new Query();
    getContentPane().add(query);
    String[] options = {"water", "soda", "juice", "none"};
    query.addRadioButtons("radio", "Radio buttons", options, "water");
</pre>

@author  Edward A. Lee, Manda Sutijono, Elaine Cheong
@version $Id: Query.java,v 1.85 2003/09/12 16:04:41 eal Exp $
@since Ptolemy II 0.3
*/
public class Query extends JPanel {

    /** Construct a panel with no entries in it.
     */
    public Query() {
        _grid = new GridBagLayout();
        _constraints = new GridBagConstraints();
        _constraints.fill = GridBagConstraints.HORIZONTAL;

        // If the next line is commented out, then the PtolemyApplet
        // model parameters will have an entry that is less than one
        // character wide unless the window is made to be fairly large.
        _constraints.weightx = 1.0;
        _constraints.anchor = GridBagConstraints.NORTHWEST;
        _entryPanel.setLayout(_grid);
        // It's not clear whether the following has any real significance...
        // _entryPanel.setOpaque(true);

        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

        _entryPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Add a message panel into which a message can be placed using
        // setMessage().
        _messageArea = new JTextArea("");
        _messageArea.setFont(new Font("SansSerif", Font.PLAIN, 12));
        _messageArea.setEditable(false);
        _messageArea.setLineWrap(true);
        _messageArea.setWrapStyleWord(true);

        // It seems like setLineWrap is somewhat broken.  Really,
        // setLineWrap works best with scrollbars.  We have
        // a couple of choices: use scrollbars or hack in something
        // that guesses the number of lines.  Note that to
        // use scrollbars, the tutorial at
        // http://java.sun.com/docs/books/tutorial/uiswing/components/simpletext.html#textarea
        // suggests: "If you put a text area in a scroll pane, be
        // sure to set the scroll pane's preferred size or use a
        // text area constructor that sets the number of rows and
        // columns for the text area."

        _messageArea.setBackground(null);

        _messageArea.setAlignmentX(Component.LEFT_ALIGNMENT);

        _messageScrollPane = new JScrollPane(_messageArea);
        _messageScrollPane.setVerticalScrollBarPolicy(
            JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        // Get rid of the border.
        _messageScrollPane.setBorder(BorderFactory.createEmptyBorder());
        _messageScrollPane.getViewport().setBackground(null);

        // Useful for debugging:
        //_messageScrollPane.setBorder(
        //                    BorderFactory.createLineBorder(Color.pink));

        // We add the _messageScrollPane when we first use it.

        _entryScrollPane = new JScrollPane(_entryPanel);
        // Get rid of the border.
        _entryScrollPane.setBorder(BorderFactory.createEmptyBorder());
        _entryScrollPane.getViewport().setBackground(null);
        _entryScrollPane.setBackground(null);
        add(_entryScrollPane);

        // Setting the background to null allegedly means it inherits the
        // background color from the container.
        _entryPanel.setBackground(null);
    }

    ///////////////////////////////////////////////////////////////////
    ////                         public methods                    ////

    /** Create an on-off check box.
     *  @param name The name used to identify the entry (when calling get).
     *  @param label The label to attach to the entry.
     *  @param defaultValue The default value (true for on).
     */
    public void addCheckBox(String name, String label, boolean defaultValue) {
        JLabel lbl = new JLabel(label + ": ");
        lbl.setBackground(_background);
        JRadioButton checkbox = new JRadioButton();
        checkbox.setBackground(_background);
        checkbox.setOpaque(false);
        checkbox.setSelected(defaultValue);
        _addPair(name, lbl, checkbox, checkbox);
        // Add the listener last so that there is no notification
        // of the first value.
        checkbox.addItemListener(new QueryItemListener(name));
    }

    /** Create an uneditable choice menu.
     *  @param name The name used to identify the entry (when calling get).
     *  @param label The label to attach to the entry.
     *  @param values The list of possible choices.
     *  @param defaultChoice Default choice.
     */
    public void addChoice(
            String name,
            String label,
            String[] values,
            String defaultChoice) {
        addChoice(name, label, values, defaultChoice, false);
    }

    /** Create a choice menu.
     *  @param name The name used to identify the entry (when calling get).
     *  @param label The label to attach to the entry.
     *  @param values The list of possible choices.
     *  @param defaultChoice Default choice.
     *  @param editable True if an arbitrary choice can be entered, in addition
     *  to the choices in values.
     */
    public void addChoice(
            String name,
            String label,
            String[] values,
            String defaultChoice,
            boolean editable) {
        addChoice(name, label, values, defaultChoice, editable, Color.white);
    }

    /** Create a choice menu.
     *  @param name The name used to identify the entry (when calling get).
     *  @param label The label to attach to the entry.
     *  @param values The list of possible choices.
     *  @param defaultChoice Default choice.
     *  @param editable True if an arbitrary choice can be entered, in addition
     *   to the choices in values.
     *  @param background The background color for the editable part.
     */
    public void addChoice(
            String name,
            String label,
            String[] values,
            String defaultChoice,
            boolean editable,
            Color background) {
        JLabel lbl = new JLabel(label + ": ");
        lbl.setBackground(_background);
        JComboBox combobox = new JComboBox(values);
        combobox.setEditable(editable);
        // FIXME: Typical of Swing, the following does not set
        // the background color.  How does one set the background
        // color?
        combobox.setBackground(background);
        combobox.setSelectedItem(defaultChoice);
        _addPair(name, lbl, combobox, combobox);
        // Add the listener last so that there is no notification
        // of the first value.
        combobox.addItemListener(new QueryItemListener(name));
    }

    /** Create a simple one-line text display, a non-editable value that
     *  is set externally using the setDisplay() method.
     *  @param name The name used to identify the entry (when calling get).
     *  @param label The label to attach to the entry.
     *  @param theValue Default string to display.
     */
    public void addDisplay(String name, String label, String theValue) {
        JLabel lbl = new JLabel(label + ": ");
        lbl.setBackground(_background);
        // NOTE: JLabel would be a reasonable choice here, but at
        // least in the current version of swing, JLabel.setText() does
        // not work.
        JTextArea displayField = new JTextArea(theValue, 1, 10);
        displayField.setEditable(false);
        displayField.setBackground(_background);
        _addPair(name, lbl, displayField, displayField);
    }

    /** Create a ColorChooser
     *  @param name The name used to identify the entry (when calling get).
     *  @param label The label to attach to the entry.
     *  @param defaultColor The default color to use.
     */
    public void addColorChooser(
            String name,
            String label,
            String defaultColor) {

        JLabel lbl = new JLabel(label + ": ");
        lbl.setBackground(_background);
        QueryColorChooser colorChooser =
            new QueryColorChooser(name, defaultColor);
        _addPair(name, lbl, colorChooser, colorChooser);
    }

    /** Create a FileChooser
     *  @param name The name used to identify the entry (when calling get).
     *  @param label The label to attach to the entry.
     *  @param defaultName The default file name to use.
     *  @param base The URI with respect to which to give
     *   relative file names, or null to give absolute file name.
     *  @param startingDirectory The directory to open the file chooser in.
     */
    public void addFileChooser(
            String name,
            String label,
            String defaultName,
            URI base,
            File startingDirectory) {
        addFileChooser(
                name,
                label,
                defaultName,
                base,
                startingDirectory,
                Color.white);
    }

    /** Create a FileChooser
     *  @param name The name used to identify the entry (when calling get).
     *  @param label The label to attach to the entry.
     *  @param defaultName The default file name to use.
     *  @param base The URI with respect to which to give
     *   relative file names, or null to give absolute file name.
     *  @param startingDirectory The directory to open the file chooser in.
     *  @param background The background color for the text entry box.
     */
    public void addFileChooser(
            String name,
            String label,
            String defaultName,
            URI base,
            File startingDirectory,
            Color background) {

        JLabel lbl = new JLabel(label + ": ");
        lbl.setBackground(_background);
        QueryFileChooser fileChooser =
            new QueryFileChooser(
                    name,
                    defaultName,
                    base,
                    startingDirectory,
                    background);
        _addPair(name, lbl, fileChooser, fileChooser);
    }

    /** Create a single-line entry box with the specified name, label, and
     *  default value.  To control the width of the box, call setTextWidth()
     *  first.
     *  @param name The name used to identify the entry (when accessing
     *   the entry).
     *  @param label The label to attach to the entry.
     *  @param defaultValue Default value to appear in the entry box.
     */
    public void addLine(String name, String label, String defaultValue) {
        addLine(name, label, defaultValue, Color.white);
    }

    /** Create a single-line entry box with the specified name, label,
     *  default value, and background color.  To control the width of
     *  the box, call setTextWidth() first.
     *  @param name The name used to identify the entry (when accessing
     *   the entry).
     *  @param label The label to attach to the entry.
     *  @param defaultValue Default value to appear in the entry box.
     *  @param background The background color.
     */
    public void addLine(
            String name,
            String label,
            String defaultValue,
            Color background) {
        JLabel lbl = new JLabel(label + ": ");
        lbl.setBackground(_background);
        JTextField entryBox = new JTextField(defaultValue, _width);
        entryBox.setBackground(background);
        _addPair(name, lbl, entryBox, entryBox);

        // Add the listener last so that there is no notification
        // of the first value.
        entryBox.addActionListener(new QueryActionListener(name));

        // Add a listener for loss of focus.  When the entry gains
        // and then loses focus, listeners are notified of an update,
        // but only if the value has changed since the last notification.
        // FIXME: Unfortunately, Java calls this listener some random
        // time after the window has been closed.  It is not even a
        // a queued event when the window is closed.  Thus, we have
        // a subtle bug where if you enter a value in a line, do not
        // hit return, and then click on the X to close the window,
        // the value is restored to the original, and then sometime
        // later, the focus is lost and the entered value becomes
        // the value of the parameter.  I don't know of any workaround.
        entryBox.addFocusListener(new QueryFocusListener(name));
    }

    /*  Create a text area.
     *  @param name The name used to identify the entry (when calling get).
     *  @param label The label to attach to the entry.
     *  @param theValues The value of this text area
     */
    public void addTextArea(String name, String label, String theValue) {
        addTextArea(name, label, theValue, Color.white);
    }

    /*  Create a text area.
     *  @param name The name used to identify the entry (when calling get).
     *  @param label The label to attach to the entry.
     *  @param theValues The value of this text area.
     *  @param background The background color.
     */
    public void addTextArea(
            String name,
            String label,
            String theValue,
            Color background) {
        JLabel lbl = new JLabel(label + ": ");
        lbl.setBackground(_background);
        JTextArea textArea = new JTextArea(theValue, _height, _width);
        textArea.setEditable(true);
        textArea.setBackground(background);
        QueryScrollPane textPane = new QueryScrollPane(textArea);
        _addPair(name, lbl, textPane, textPane);
        textArea.addFocusListener(new QueryFocusListener(name));
    }

    /** Add a listener.  The changed() method of the listener will be
     *  called when any of the entries is changed.  Note that "line"
     *  entries only trigger this call when Return or Enter is pressed, or
     *  when the entry gains and then loses the keyboard focus.
     *  Notice that the currently selected line loses focus when the
     *  panel is destroyed, so notification of any changes that
     *  have been made will be done at that time.  That notification
     *  will occur in the UI thread, and may be later than expected.
     *  Notification due to loss of focus only occurs if the value
     *  of the entry has changed since the last notification.
     *  If the listener has already been added, then do nothing.
     *  @param listener The listener to add.
     */
    public void addQueryListener(QueryListener listener) {
        if (_listeners == null)
            _listeners = new Vector();
        if (_listeners.contains(listener))
            return;
        _listeners.add(listener);
    }

    /** Create a bank of radio buttons.  A radio button provides a list of
     *  choices, only one of which may be chosen at a time.
     *  @param name The name used to identify the entry (when calling get).
     *  @param label The label to attach to the entry.
     *  @param values The list of possible choices.
     *  @param defaultValue Default value.
     */
    public void addRadioButtons(
        String name,
        String label,
        String[] values,
        String defaultValue) {
        JLabel lbl = new JLabel(label + ": ");
        lbl.setBackground(_background);
        FlowLayout flow = new FlowLayout();
        flow.setAlignment(FlowLayout.LEFT);

        // This must be a JPanel, not a Panel, or the scroll bars won't work.
        JPanel buttonPanel = new JPanel(flow);

        ButtonGroup group = new ButtonGroup();
        QueryActionListener listener = new QueryActionListener(name);

        // Regrettably, ButtonGroup provides no method to find out
        // which button is selected, so we have to go through a
        // song and dance here...
        JRadioButton[] buttons = new JRadioButton[values.length];
        for (int i = 0; i < values.length; i++) {
            JRadioButton checkbox = new JRadioButton(values[i]);
            buttons[i] = checkbox;
            checkbox.setBackground(_background);
            // The following (essentially) undocumented method does nothing...
            // checkbox.setContentAreaFilled(true);
            checkbox.setOpaque(false);
            if (values[i].equals(defaultValue)) {
                checkbox.setSelected(true);
            }
            group.add(checkbox);
            buttonPanel.add(checkbox);
            // Add the listener last so that there is no notification
            // of the first value.
            checkbox.addActionListener(listener);
        }
        _addPair(name, lbl, buttonPanel, buttons);
    }

    /** Create a bank of buttons that provides a list of
     *  choices, any subset of which may be chosen at a time.
     *  @param name The name used to identify the entry (when calling get).
     *  @param label The label to attach to the entry.
     *  @param values The list of possible choices.
     *  @param initiallySelected The initially selected choices, or null
     *   to indicate that none are selected.
     */
    public void addSelectButtons(
        String name,
        String label,
        String[] values,
        Set initiallySelected) {
        JLabel lbl = new JLabel(label + ": ");
        lbl.setBackground(_background);
        FlowLayout flow = new FlowLayout();
        flow.setAlignment(FlowLayout.LEFT);

        // This must be a JPanel, not a Panel, or the scroll bars won't work.
        JPanel buttonPanel = new JPanel(flow);

        QueryActionListener listener = new QueryActionListener(name);
        if (initiallySelected == null) {
            initiallySelected = new HashSet();
        }
        JRadioButton[] buttons = new JRadioButton[values.length];
        for (int i = 0; i < values.length; i++) {
            JRadioButton checkbox = new JRadioButton(values[i]);
            buttons[i] = checkbox;
            checkbox.setBackground(_background);
            // The following (essentially) undocumented method does nothing...
            // checkbox.setContentAreaFilled(true);
            checkbox.setOpaque(false);
            if (initiallySelected.contains(values[i])) {
                checkbox.setSelected(true);
            }
            buttonPanel.add(checkbox);
            // Add the listener last so that there is no notification
            // of the first value.
            checkbox.addActionListener(listener);
        }
        _addPair(name, lbl, buttonPanel, buttons);
    }

    /** Create a slider with the specified name, label, default
     *  value, maximum, and minimum.
     *  @param name The name used to identify the slider.
     *  @param label The label to attach to the slider.
     *  @param defaultValue Initial position of slider.
     *  @param maximum Maximum value of slider.
     *  @param minimum Minimum value of slider.
     *  @exception IllegalArgumentException If the desired default value
     *   is not between the minimum and maximum.
     */
    public void addSlider(
        String name,
        String label,
        int defaultValue,
        int minimum,
        int maximum)
        throws IllegalArgumentException {
        JLabel lbl = new JLabel(label + ": ");
        if (minimum > maximum) {
            int temp = minimum;
            minimum = maximum;
            maximum = temp;
        }
        if ((defaultValue > maximum) || (defaultValue < minimum)) {
            throw new IllegalArgumentException(
                "Desired default "
                    + "value \""
                    + defaultValue
                    + "\" does not fall "
                    + "between the minimum and maximum.");
        }
        JSlider slider = new JSlider(minimum, maximum, defaultValue);
        _addPair(name, lbl, slider, slider);
        slider.addChangeListener(new SliderListener(name));
    }

    /** Get the current value in the entry with the given name
     *  and return as a boolean.  If the entry is not a checkbox,
     *  then throw an exception.
     *  @deprecated Use getBooleanValue(String name) instead.
     *  @return The state of the checkbox.
     *  @exception NoSuchElementException If there is no item with the
     *   specified name.  Note that this is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception IllegalArgumentException If the entry is not a
     *   checkbox.  This is a runtime exception, so it
     *   need not be declared explicitly.
     */
    public boolean booleanValue(String name)
        throws NoSuchElementException, IllegalArgumentException {
        return getBooleanValue(name);
    }

    /** Get the current value in the entry with the given name
     *  and return as a double value.  If the entry is not a line,
     *  then throw an exception.  If the value of the entry is not
     *  a double, then throw an exception.
     *  @deprecated Use getDoubleValue(String name) instead.
     *  @return The value currently in the entry as a double.
     *  @exception NoSuchElementException If there is no item with the
     *   specified name.  Note that this is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception NumberFormatException If the value of the entry cannot
     *   be converted to a double.  This is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception IllegalArgumentException If the entry is not a
     *   line.  This is a runtime exception, so it
     *   need not be declared explicitly.
     */
    public double doubleValue(String name)
        throws
            IllegalArgumentException,
            NoSuchElementException,
            NumberFormatException {
        return getDoubleValue(name);
    }

    /** Get the current value in the entry with the given name
     *  and return as a boolean.  If the entry is not a checkbox,
     *  then throw an exception.
     *  @return The state of the checkbox.
     *  @exception NoSuchElementException If there is no item with the
     *   specified name.  Note that this is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception IllegalArgumentException If the entry is not a
     *   checkbox.  This is a runtime exception, so it
     *   need not be declared explicitly.
     */
    public boolean getBooleanValue(String name)
        throws NoSuchElementException, IllegalArgumentException {
        Object result = _entries.get(name);
        if (result == null) {
            throw new NoSuchElementException(
                "No item named \"" + name + "\" in the query box.");
        }
        if (result instanceof JRadioButton) {
            return ((JRadioButton) result).isSelected();
        } else {
            throw new IllegalArgumentException(
                "Item named \""
                    + name
                    + "\" is not a radio button, and hence does not have "
                    + "a boolean value.");
        }
    }

    /** Get the current value in the entry with the given name
     *  and return as a double value.  If the entry is not a line,
     *  then throw an exception.  If the value of the entry is not
     *  a double, then throw an exception.
     *  @return The value currently in the entry as a double.
     *  @exception NoSuchElementException If there is no item with the
     *   specified name.  Note that this is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception NumberFormatException If the value of the entry cannot
     *   be converted to a double.  This is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception IllegalArgumentException If the entry is not a
     *   line.  This is a runtime exception, so it
     *   need not be declared explicitly.
     */
    public double getDoubleValue(String name)
        throws
            IllegalArgumentException,
            NoSuchElementException,
            NumberFormatException {
        Object result = _entries.get(name);
        if (result == null) {
            throw new NoSuchElementException(
                "No item named \"" + name + " \" in the query box.");
        }
        if (result instanceof JTextField) {
            return (new Double(((JTextField) result).getText())).doubleValue();
        } else {
            throw new IllegalArgumentException(
                "Item named \""
                    + name
                    + "\" is not a text line, and hence cannot be converted to "
                    + "a double value.");
        }
    }

    /** Get the current value in the entry with the given name
     *  and return as an integer.  If the entry is not a line,
     *  choice, or slider, then throw an exception.
     *  If it is a choice or radio button, then return the
     *  index of the first selected item.
     *  @return The value currently in the entry as an integer.
     *  @exception NoSuchElementException If there is no item with the
     *   specified name.  Note that this is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception NumberFormatException If the value of the entry cannot
     *   be converted to an integer.  This is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception IllegalArgumentException If the entry is not a
     *   choice, line, or slider.  This is a runtime exception, so it
     *   need not be declared explicitly.
     */
    public int getIntValue(String name)
        throws
            IllegalArgumentException,
            NoSuchElementException,
            NumberFormatException {
        Object result = _entries.get(name);
        if (result == null) {
            throw new NoSuchElementException(
                "No item named \"" + name + " \" in the query box.");
        }
        if (result instanceof JTextField) {
            return (new Integer(((JTextField) result).getText())).intValue();
        } else if (result instanceof JSlider) {
            return ((JSlider) result).getValue();
        } else if (result instanceof JComboBox) {
            return ((JComboBox) result).getSelectedIndex();
        } else if (result instanceof JRadioButton[]) {
            // Regrettably, ButtonGroup gives no way to determine
            // which button is selected, so we have to search...
            JRadioButton[] buttons = (JRadioButton[]) result;
            for (int i = 0; i < buttons.length; i++) {
                if (buttons[i].isSelected()) {
                    return i;
                }
            }
            // In theory, we shouldn't get here, but the compiler
            // is unhappy without a return.
            return -1;
        } else {
            throw new IllegalArgumentException(
                "Item named \""
                    + name
                    + "\" is not a text line or slider, and hence "
                    + "cannot be converted to "
                    + "an integer value.");
        }
    }

    /** Return the preferred height, but set the width to the maximum
     *  possible value.  Currently (JDK 1.3), only BoxLayout pays any
     *  attention to getMaximumSize().
     *
     *  @return The maximum desired size.
     */
    public Dimension getMaximumSize() {
        // Unfortunately, if we don't have a message, then we end up with
        // an empty space that is difficult to control the size of, which
        // requires us to set the maximum size to be the same as
        // the preferred size

        // If you change this, be sure to try applets that have both
        // horizontal and vertical layout.
        Dimension preferred = getPreferredSize();
        preferred.width = Short.MAX_VALUE;
        return preferred;
    }

    /** Get the preferred width in characters to be used for entry
     *  boxes created in using addLine().  The preferred width is set
     *  using setTextWidth().
     *  @return The preferred width of an entry box in characters.
     */
    public int getTextWidth() {
        return _width;
    }

    /** Get the preferred number of lines to be used for entry boxes created
     *  in using addTextArea().  The preferred height is set using
     *  setTextHeight().
     *  @return The preferred height in lines.
     *  @see #addTextArea(String, String, String)
     *  @see #setTextHeight(int)
     */
    public int getTextHeight() {
        return _height;
    }

    /** Get the current value in the entry with the given name,
     *  and return as a String.  All entry types support this.
     *  Note that this method should be called from the event dispatch
     *  thread, since it needs to query to UI widgets for their current
     *  values.  If it is called from another thread, there is no
     *  assurance that the value returned will be the current value.
     *  @return The value currently in the entry as a String.
     *  @exception NoSuchElementException If there is no item with the
     *   specified name.  Note that this is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception IllegalArgumentException If the entry type does not
     *   have a string representation (this should not be thrown).
     */
    public String getStringValue(String name)
        throws NoSuchElementException, IllegalArgumentException {
        Object result = _entries.get(name);
        if (result == null) {
            throw new NoSuchElementException(
                "No item named \"" + name + " \" in the query box.");
        }
        // FIXME: Surely there is a better way to do this...
        // We should define a set of inner classes, one for each entry type.
        // Currently, this has to be updated each time a new entry type
        // is added.
        if (result instanceof JTextField) {
            return ((JTextField) result).getText();
        } else if (result instanceof QueryColorChooser) {
            return ((QueryColorChooser) result).getSelectedColor();
        } else if (result instanceof QueryFileChooser) {
            return ((QueryFileChooser) result).getSelectedFileName();
        } else if (result instanceof JTextArea) {
            return ((JTextArea) result).getText();
        } else if (result instanceof JRadioButton) {
            JRadioButton radioButton = (JRadioButton) result;
            if (radioButton.isSelected()) {
                return "true";
            } else {
                return "false";
            }
        } else if (result instanceof JSlider) {
            return "" + ((JSlider) result).getValue();
        } else if (result instanceof JComboBox) {
            return (String) (((JComboBox) result).getSelectedItem());
        } else if (result instanceof JRadioButton[]) {
            // Regrettably, ButtonGroup gives no way to determine
            // which button is selected, so we have to search...
            JRadioButton[] buttons = (JRadioButton[]) result;
            String toReturn = null;
            for (int i = 0; i < buttons.length; i++) {
                if (buttons[i].isSelected()) {
                    if (toReturn == null)
                        toReturn = buttons[i].getText();
                    else
                        toReturn = toReturn + ", " + buttons[i].getText();
                }
            }
            if (toReturn == null)
                toReturn = "";
            return toReturn;
        } else if (result instanceof QueryScrollPane) {
            return ((QueryScrollPane) result).getText();
        } else {
            throw new IllegalArgumentException(
                "Query class cannot generate"
                    + " a string representation for entries of type "
                    + result.getClass());
        }
    }

    /** Get the current value in the entry with the given name
     *  and return as an integer.  If the entry is not a line,
     *  choice, or slider, then throw an exception.
     *  If it is a choice or radio button, then return the
     *  index of the first selected item.
     *  @deprecated Use getIntValue(String name) instead.
     *  @return The value currently in the entry as an integer.
     *  @exception NoSuchElementException If there is no item with the
     *   specified name.  Note that this is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception NumberFormatException If the value of the entry cannot
     *   be converted to an integer.  This is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception IllegalArgumentException If the entry is not a
     *   choice, line, or slider.  This is a runtime exception, so it
     *   need not be declared explicitly.
     */
    public int intValue(String name)
        throws
            IllegalArgumentException,
            NoSuchElementException,
            NumberFormatException {
        return getIntValue(name);
    }

    /** Notify listeners of the current value of all entries, unless
     *  those entries have not changed since the last notification.
     */
    public void notifyListeners() {
        Iterator names = _entries.keySet().iterator();
        while (names.hasNext()) {
            String name = (String) names.next();
            _notifyListeners(name);
        }
    }

    /** Remove a listener.  If the listener has not been added, then
     *  do nothing.
     *  @param listener The listener to remove.
     */
    public void removeQueryListener(QueryListener listener) {
        if (_listeners == null)
            return;
        _listeners.remove(listener);
    }

    /** Set the value in the entry with the given name.
     *  The second argument must be a string that can be parsed to the
     *  proper type for the given entry, or an exception is thrown.
     *  Note that this does NOT trigger the notification of listeners, and
     *  intended to allow a way to set the query to reflect the current state.
     *  @param name The name used to identify the entry (when calling get).
     *  @param value The value to set the entry to.
     *  @exception NoSuchElementException If there is no item with the
     *   specified name.  Note that this is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception IllegalArgumentException If the value does not parse
     *   to the appropriate type.
     */
    public void set(String name, String value)
        throws NoSuchElementException, IllegalArgumentException {
        Object result = _entries.get(name);
        if (result == null) {
            throw new NoSuchElementException(
                "No item named \"" + name + " \" in the query box.");
        }
        // FIXME: Surely there is a better way to do this...
        // We should define a set of inner classes, one for each entry type.
        // Currently, this has to be updated each time a new entry type
        // is added.
        if (result instanceof JTextField) {
            ((JTextField) result).setText(value);
        } else if (result instanceof JTextArea) {
            ((JTextArea) result).setText(value);
        } else if (result instanceof QueryScrollPane) {
            ((QueryScrollPane) result).setText(value);
        } else if (result instanceof JRadioButton) {
            Boolean flag = new Boolean(value);
            setBoolean(name, flag.booleanValue());
        } else if (result instanceof JSlider) {
            Integer parsed = new Integer(value);
            ((JSlider) result).setValue(parsed.intValue());
        } else if (result instanceof JComboBox) {
            ((JComboBox) result).setSelectedItem(value);
        } else if (result instanceof JRadioButton[]) {
            // First, parse the value, which may be a comma-separated list.
            Set selectedValues = new HashSet();
            StringTokenizer tokenizer = new StringTokenizer(value, ",");
            while (tokenizer.hasMoreTokens()) {
                selectedValues.add(tokenizer.nextToken().trim());
            }
            JRadioButton[] buttons = (JRadioButton[]) result;
            for (int i = 0; i < buttons.length; i++) {
                if (selectedValues.contains(buttons[i].getText())) {
                    buttons[i].setSelected(true);
                } else {
                    buttons[i].setSelected(false);
                }
            }
        } else if (result instanceof QueryColorChooser) {
            ((QueryColorChooser) result).setColor(value);
        } else if (result instanceof QueryFileChooser) {
            ((QueryFileChooser) result).setFileName(value);
        } else {
            throw new IllegalArgumentException(
                "Query class cannot set"
                    + " a string representation for entries of type "
                    + result.getClass());
        }
        // Record the new value as if it was the previously notified
        // value.  Thus, any future change from this value will trigger
        // notification.
        _previous.put(name, value);
    }

    /** Set the value in the entry with the given name and notify listeners.
     *  The second argument must be a string that can be parsed to the
     *  proper type for the given entry, or an exception is thrown.
     *  @param name The name used to identify the entry (when calling get).
     *  @param value The value to set the entry to.
     *  @exception NoSuchElementException If there is no item with the
     *   specified name.  Note that this is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception IllegalArgumentException If the value does not parse
     *   to the appropriate type.
     */
    public void setAndNotify(String name, String value)
        throws NoSuchElementException, IllegalArgumentException {
        set(name, value);
        _notifyListeners(name);
    }

    /** Set the background color for all the widgets.
     *  @param color The background color.
     */
    public void setBackground(Color color) {
        super.setBackground(color);
        _background = color;
        // Set the background of any components that already exist.
        Component[] components = getComponents();
        for (int i = 0; i < components.length; i++) {
            if (!(components[i] instanceof JTextField)) {
                components[i].setBackground(_background);
            }
        }
    }

    /** Set the current value in the entry with the given name.
     *  If the entry is not a checkbox, then throw an exception.
     *  Notify listeners that the value has changed.
     *  @exception NoSuchElementException If there is no item with the
     *   specified name.  Note that this is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception IllegalArgumentException If the entry is not a
     *   checkbox.  This is a runtime exception, so it
     *   need not be declared explicitly.
     */
    public void setBoolean(String name, boolean value)
        throws NoSuchElementException, IllegalArgumentException {
        Object result = _entries.get(name);
        if (result == null) {
            throw new NoSuchElementException(
                "No item named \"" + name + "\" in the query box.");
        }
        if (result instanceof JRadioButton) {
            ((JRadioButton) result).setSelected(value);
        } else {
            throw new IllegalArgumentException(
                "Item named \""
                    + name
                    + "\" is not a radio button, and hence does not have "
                    + "a boolean value.");
        }
        _notifyListeners(name);
    }

    /** Specify the number of columns to use.
     *  The default is one.  If an integer larger than one is specified
     *  here, then the queries will be arranged using the specified number
     *  of columns.  As queries are added, they are put in the first row
     *  until that row is full.  Then they are put in the second row, etc.
     *  @param columns The number of columns.
     */
    public void setColumns(int columns) {
        if (columns <= 0)
            throw new IllegalArgumentException("Query.setColumns() requires a strictly positive argument.");
        _columns = columns;
    }

    /** Set the displayed text of an entry that has been added using
     *  addDisplay.
     *  Notify listeners that the value has changed.
     *  @param name The name of the entry.
     *  @param value The string to display.
     *  @exception NoSuchElementException If there is no entry with the
     *   specified name.  Note that this is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception IllegalArgumentException If the entry is not a
     *   display.  This is a runtime exception, so it
     *   need not be declared explicitly.
     */
    public void setDisplay(String name, String value)
        throws NoSuchElementException, IllegalArgumentException {
        Object result = _entries.get(name);
        if (result == null) {
            throw new NoSuchElementException(
                "No item named \"" + name + " \" in the query box.");
        }
        if (result instanceof JTextArea) {
            JTextArea label = (JTextArea) result;
            label.setText(value);
        } else {
            throw new IllegalArgumentException(
                "Item named \""
                    + name
                    + "\" is not a display, and hence cannot be set using "
                    + "setDisplay().");
        }
        _notifyListeners(name);
    }

    /** For line, display, check box, slider, radio button, or choice
     *  entries made, if the second argument is false, then it will
     *  be disabled.
     *  @param name The name of the entry.
     *  @param value If false, disables the entry.
     */
    public void setEnabled(String name, boolean value) {
        Object result = _entries.get(name);
        if (result == null) {
            throw new NoSuchElementException(
                "No item named \"" + name + " \" in the query box.");
        }
        if (result instanceof JComponent) {
            ((JComponent) result).setEnabled(value);
        } else if (result instanceof JRadioButton[]) {
            JRadioButton[] buttons = (JRadioButton[]) result;
            for (int i = 0; i < buttons.length; i++) {
                buttons[i].setEnabled(value);
            }
        }
    }

    /** Set the displayed text of an item that has been added using
     *  addLine. Notify listeners that the value has changed.
     *  @param name The name of the entry.
     *  @param value The string to display.
     *  @exception NoSuchElementException If there is no item with the
     *   specified name.  Note that this is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception IllegalArgumentException If the entry is not a
     *   display.  This is a runtime exception, so it
     *   need not be declared explicitly.
     */
    public void setLine(String name, String value) {
        Object result = _entries.get(name);
        if (result == null) {
            throw new NoSuchElementException(
                "No item named \"" + name + " \" in the query box.");
        }
        if (result instanceof JTextField) {
            JTextField line = (JTextField) result;
            line.setText(value);
        } else {
            throw new IllegalArgumentException(
                "Item named \""
                    + name
                    + "\" is not a line, and hence cannot be set using "
                    + "setLine().");
        }
        _notifyListeners(name);
    }

    /** Specify a message to be displayed above the query.
     *  @param message The message to display.
     */
    public void setMessage(String message) {
        if (!_messageScrollPaneAdded) {
            _messageScrollPaneAdded = true;
            add(_messageScrollPane, 1);

            // Add a spacer.
            add(Box.createRigidArea(new Dimension(0, 10)), 2);
        }

        _messageArea.setText(message);
        // I'm not sure why we need to add 1 here?
        int lineCount = _messageArea.getLineCount() + 1;
        // Keep the line count to less than 30 lines.  If
        // we have more than 30 lines, we get a scroll bar.
        if (lineCount > 30) {
            lineCount = 30;
        }
        _messageArea.setRows(lineCount);
        _messageArea.setColumns(_width);

        // In case size has changed.
        validate();
    }

    /** Set the position of an item that has been added using
     *  addSlider.  Notify listeners that the value has changed.
     *  @param name The name of the entry.
     *  @param value The value to set the slider position.
     *  @exception NoSuchElementException If there is no item with the
     *   specified name.  Note that this is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception IllegalArgumentException If the entry is not a
     *   slider.  This is a runtime exception, so it
     *   need not be declared explicitly.
     */
    public void setSlider(String name, int value) {
        Object result = _entries.get(name);
        if (result == null) {
            throw new NoSuchElementException(
                "No item named \"" + name + " \" in the query box.");
        }
        if (result instanceof JSlider) {
            JSlider theSlider = (JSlider) result;
            // Set the new slider position.
            theSlider.setValue(value);
        } else {
            throw new IllegalArgumentException(
                "Item named \""
                    + name
                    + "\" is not a slider, and hence cannot be set using "
                    + "setSlider().");
        }
        _notifyListeners(name);
    }

    /** Specify the preferred height to be used for entry boxes created
     *  in using addTextArea().  If this is called multiple times, then
     *  it only affects subsequent calls.
     *  @param characters The preferred height.
     *  @see #addTextArea(String, String, String)
     *  @see #getTextHeight()
     */
    public void setTextHeight(int characters) {
        _height = characters;
    }

    /** Specify the preferred width to be used for entry boxes created
     *  in using addLine().  If this is called multiple times, then
     *  it only affects subsequent calls.
     *  @param characters The preferred width.
     */
    public void setTextWidth(int characters) {
        _width = characters;
    }

    /** Specify a tool tip to appear when the mouse lingers over the label.
     *  @param name The name of the entry.
     *  @param tip The text of the tool tip.
     */
    public void setToolTip(String name, String tip) {
        JLabel label = (JLabel) _labels.get(name);
        if (label != null) {
            label.setToolTipText(tip);
        }
    }

    /** Get the current value in the entry with the given name,
     *  and return as a String.  All entry types support this.
     *  Note that this method should be called from the event dispatch
     *  thread, since it needs to query to UI widgets for their current
     *  values.  If it is called from another thread, there is no
     *  assurance that the value returned will be the current value.
     *  @deprecated Use getStringValue(String name) instead.
     *  @return The value currently in the entry as a String.
     *  @exception NoSuchElementException If there is no item with the
     *   specified name.  Note that this is a runtime exception, so it
     *   need not be declared explicitly.
     *  @exception IllegalArgumentException If the entry type does not
     *   have a string representation (this should not be thrown).
     */
    public String stringValue(String name)
        throws NoSuchElementException, IllegalArgumentException {
        return getStringValue(name);
    }

    ///////////////////////////////////////////////////////////////////
    ////                         public variables                  ////

    /** The default height of entries created with addText(). */
    public static final int DEFAULT_ENTRY_HEIGHT = 10;

    /** The default width of entries created with addLine(). */
    public static final int DEFAULT_ENTRY_WIDTH = 30;

    ///////////////////////////////////////////////////////////////////
    ////                         protected methods                 ////

    /** Add a label and a widget to the panel.
     *  @param name The name of the entry.
     *  @param label The label.
     *  @param widget The interactive entry to the right of the label.
     *  @param entry The object that contains user data.
     */
    protected void _addPair(
        String name,
        JLabel label,
        Component widget,
        Object entry) {
        // Surely there is a better layout manager in swing...
        // Note that Box and BoxLayout do not work because they do not
        // support gridded layout.
        _constraints.gridwidth = 1;
        _constraints.insets = _leftPadding;
        _grid.setConstraints(label, _constraints);
        _entryPanel.add(label);

        _constraints.insets = _noPadding;
        if (_columns > 1 && ((_entries.size() + 1) % _columns != 0)) {
            _constraints.gridwidth = 1;
        } else {
            _constraints.gridwidth = GridBagConstraints.REMAINDER;
        }
        _grid.setConstraints(widget, _constraints);
        _entryPanel.add(widget);

        _entries.put(name, entry);
        _labels.put(name, label);
        _previous.put(name, getStringValue(name));

        Dimension preferredSize = _entryPanel.getPreferredSize();
        // Add some slop to the width to take in to account
        // the width of the vertical scrollbar.
        preferredSize.width += 25;

        // Applets seem to need this, see CT/SigmaDelta
        _widgetsHeight += widget.getPreferredSize().height;
        preferredSize.height = _widgetsHeight;

        Toolkit tk = Toolkit.getDefaultToolkit();
        if (preferredSize.height > tk.getScreenSize().height) {
            // Fudge factor to keep this window smaller than the screen
            // height.  CGSUnitBase and the Code Generator are good tests.
            preferredSize.height = (int) (tk.getScreenSize().height * 0.75);
            _entryScrollPane.setPreferredSize(preferredSize);
        }

        _entryScrollPane.setPreferredSize(preferredSize);
        // Call revalidate for the scrollbar.
        _entryPanel.revalidate();
    }

    ///////////////////////////////////////////////////////////////////
    ////                         protected variables               ////

    /** The background color as set by setBackground().
     *  This defaults to null, which indicates that the background
     *  is the same as the container.
     */
    protected Color _background = null;

    /** Standard constraints for use with _grid. */
    protected GridBagConstraints _constraints;

    /** Layout control. */
    protected GridBagLayout _grid;

    /** List of registered listeners. */
    protected Vector _listeners;

    ///////////////////////////////////////////////////////////////////
    ////                         friendly methods                  ////

    /** Notify all registered listeners that something changed for the
     *  specified entry, if it indeed has changed.  The getStringValue()
     *  method is used to check the current value against the previously
     *  notified value, or the original value if there have been no
     *  notifications.
     *  @param name The entry that may have changed.
     */
    void _notifyListeners(String name) {
        if (_listeners != null) {
            String previous = (String) _previous.get(name);
            String newValue = getStringValue(name);
            if (newValue.equals(previous)) {
                return;
            }

            // Store the new value to prevent repeated notification.
            // This must be done before listeners are notified, because
            // the notified listeners might do something that again triggers
            // notification, and we do not want that notification to occur
            // if the value has not changed.
            _previous.put(name, newValue);

            Enumeration listeners = _listeners.elements();
            while (listeners.hasMoreElements()) {
                QueryListener queryListener =
                    (QueryListener) (listeners.nextElement());
                queryListener.changed(name);
            }
        }
    }

    ///////////////////////////////////////////////////////////////////
    ////                         private variables                 ////

    // The number of columns.
    private int _columns = 1;

    // The hashtable of items in the query.
    private Map _entries = new HashMap();

    // A panel within which the entries are placed.
    private JPanel _entryPanel = new JPanel();

    // A scroll pane that contains the _entryPanel.
    private JScrollPane _entryScrollPane;

    // The number of lines in a text box.
    private int _height = DEFAULT_ENTRY_HEIGHT;

    // The hashtable of labels in the query.
    private Map _labels = new HashMap();

    // Left padding insets.
    private Insets _leftPadding = new Insets(0, 10, 0, 0);

    // Area for messages.
    private JTextArea _messageArea = null;

    // A scroll pane that contains the _messageArea.
    private JScrollPane _messageScrollPane;

    // True if we have added the _messageScrollPane
    private boolean _messageScrollPaneAdded = false;

    // No padding insets.
    private Insets _noPadding = new Insets(0, 0, 0, 0);

    // The hashtable of previous values, indexed by entry name.
    private Map _previous = new HashMap();

    // The sum of the height of the widgets added using _addPair
    // If you adjust this, try the GR/Pendulum demo, which has
    // only one parameter.
    private int _widgetsHeight = 20;

    // The number of horizontal characters in a text box.
    private int _width = DEFAULT_ENTRY_WIDTH;

    ///////////////////////////////////////////////////////////////////
    ////                         inner classes                     ////

    /** Listener for "line" and radio button entries.
     */
    class QueryActionListener implements ActionListener {
        public QueryActionListener(String name) {
            _name = name;
        }
        /** Call all registered QueryListeners. */
        public void actionPerformed(ActionEvent e) {
            _notifyListeners(_name);
        }
        private String _name;
    }

    /** Panel containing an entry box and color chooser.
     */
    class QueryColorChooser extends Box implements ActionListener {
        public QueryColorChooser(
            String name,
            String defaultColor) {

            super(BoxLayout.X_AXIS);
            _defaultColor = defaultColor;
            _entryBox = new JTextField(defaultColor, _width);
            JButton button = new JButton("Choose");
            button.addActionListener(this);
            add(_entryBox);
            add(button);
            // Add the listener last so that there is no notification
            // of the first value.
            _entryBox.addActionListener(new QueryActionListener(name));

            // Add a listener for loss of focus.  When the entry gains
            // and then loses focus, listeners are notified of an update,
            // but only if the value has changed since the last notification.
            // FIXME: Unfortunately, Java calls this listener some random
            // time after the window has been closed.  It is not even a
            // a queued event when the window is closed.  Thus, we have
            // a subtle bug where if you enter a value in a line, do not
            // hit return, and then click on the X to close the window,
            // the value is restored to the original, and then sometime
            // later, the focus is lost and the entered value becomes
            // the value of the parameter.  I don't know of any workaround.
            _entryBox.addFocusListener(new QueryFocusListener(name));

            _name = name;
        }
        public void actionPerformed(ActionEvent e) {
            // Read the current color from the text field.
            String spec = getSelectedColor().trim();
            String[] specArray = spec.split("[{},]");
            float red = 0f;
            float green = 0f;
            float blue = 0f;
            float alpha = 1.0f;
            // If any exceptions occur during the attempt to parse,
            // then just use the default color.
            try {
                int i = 0;
                // Ignore any blank strings that this simple parsing produces.
                while (specArray[i].trim().equals("")) {
                    i++;
                }
                if (specArray.length > i) {
                    red = Float.parseFloat(specArray[i]);
                }
                i++;
                while (specArray[i].trim().equals("")) {
                    i++;
                }
                if (specArray.length > i) {
                    green = Float.parseFloat(specArray[i]);
                }
                i++;
                while (specArray[i].trim().equals("")) {
                    i++;
                }
                if (specArray.length > i) {
                    blue = Float.parseFloat(specArray[i]);
                }
                i++;
                while (specArray[i].trim().equals("")) {
                    i++;
                }
                if (specArray.length > i) {
                    alpha = Float.parseFloat(specArray[i]);
                }
            } catch (Exception ex) {
                // Ignore and use default color.
            }
            Color newColor =
                JColorChooser.showDialog(
                    Query.this,
                    "Choose Background Color",
                    new Color(red, green, blue, alpha));
            if (newColor != null) {
                float[] components = newColor.getRGBComponents(null);
                StringBuffer string = new StringBuffer("{");
                // Use the syntax of arrays.
                for (int j = 0; j < components.length; j++) {
                    string.append(components[j]);
                    if (j < components.length - 1) {
                        string.append(",");
                    } else {
                        string.append("}");
                    }
                }
                _entryBox.setText(string.toString());
                _notifyListeners(_name);
            }
        }
        public String getSelectedColor() {
            return _entryBox.getText();
        }
        public void setColor(String name) {
            _entryBox.setText(name);
        }
        private JTextField _entryBox;
        private String _name;
        private String _defaultColor;
    }

    /** Panel containing an entry box and file chooser.
     */
    class QueryFileChooser extends Box implements ActionListener {
        public QueryFileChooser(
                String name,
                String defaultName,
                URI base,
                File startingDirectory) {
            this(name, defaultName, base, startingDirectory, Color.white);
        }
        public QueryFileChooser(
                String name,
                String defaultName,
                URI base,
                File startingDirectory,
                Color background) {
            super(BoxLayout.X_AXIS);
            _base = base;
            _startingDirectory = startingDirectory;
            _entryBox = new JTextField(defaultName, _width);
            _entryBox.setBackground(background);
            JButton button = new JButton("Browse");
            button.addActionListener(this);
            add(_entryBox);
            add(button);
            // Add the listener last so that there is no notification
            // of the first value.
            _entryBox.addActionListener(new QueryActionListener(name));

            // Add a listener for loss of focus.  When the entry gains
            // and then loses focus, listeners are notified of an update,
            // but only if the value has changed since the last notification.
            // FIXME: Unfortunately, Java calls this listener some random
            // time after the window has been closed.  It is not even a
            // a queued event when the window is closed.  Thus, we have
            // a subtle bug where if you enter a value in a line, do not
            // hit return, and then click on the X to close the window,
            // the value is restored to the original, and then sometime
            // later, the focus is lost and the entered value becomes
            // the value of the parameter.  I don't know of any workaround.
            _entryBox.addFocusListener(new QueryFocusListener(name));

            _name = name;
        }
        public void actionPerformed(ActionEvent e) {
            // NOTE: If the last argument is null, then choose a default dir.
            JFileChooser fileChooser = new JFileChooser(_startingDirectory);
            fileChooser.setApproveButtonText("Select");
            // FIXME: The following doesn't have any effect.
            fileChooser.setApproveButtonMnemonic('S');
            int returnValue = fileChooser.showOpenDialog(Query.this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                if (_base == null) {
                    // Absolute file name.
                    try {
                        _entryBox.setText(
                            fileChooser.getSelectedFile().getCanonicalPath());
                    } catch (IOException ex) {
                        // If we can't get a path, then just use the name.
                        _entryBox.setText(
                            fileChooser.getSelectedFile().getName());
                    }
                } else {
                    // Relative file name.
                    File selectedFile = fileChooser.getSelectedFile();
                    // FIXME: There is a bug here under Windows XP
                    // at least... Sometimes, the drive ID (like c:)
                    // is lower case, and sometimes it's upper case.
                    // When we open a MoML file, it's upper case.
                    // When we do "save as", it's lower case.
                    // This despite the fact that both use the same
                    // file browser to determine the file name.
                    // Beats me... Consequence is that if you save as,
                    // then the following relativize call doesn't work
                    // until you close and reopen the file.
                    try {
                        selectedFile = selectedFile.getCanonicalFile();
                    } catch (IOException ex) {
                        // Ignore, since we can't do much about it anyway.
                    }
                    URI relativeURI = _base.relativize(selectedFile.toURI());
                    _entryBox.setText(relativeURI.toString());
                }
                _notifyListeners(_name);
            }
        }
        public String getSelectedFileName() {
            return _entryBox.getText();
        }
        public void setFileName(String name) {
            _entryBox.setText(name);
        }
        private URI _base;
        private JTextField _entryBox;
        private String _name;
        private File _startingDirectory;
    }

    /** Listener for line entries, for when they lose the focus.
     */
    class QueryFocusListener implements FocusListener {
        public QueryFocusListener(String name) {
            _name = name;
        }
        public void focusGained(FocusEvent e) {
            // Nothing to do.
        }
        public void focusLost(FocusEvent e) {
            // NOTE: Java's lame AWT has no reliable way
            // to take action on window closing, so this focus lost
            // notification is the only reliable way we have of reacting
            // to a closing window.  If the previous
            // notification was an erroneous one and the value has not
            // changed, then no further notification occurs.
            // This could be a problem for some users of this class.
            _notifyListeners(_name);
        }
        private String _name;
    }

    /** Listener for "CheckBox" and "Choice" entries.
     */
    class QueryItemListener implements ItemListener {
        public QueryItemListener(String name) {
            _name = name;
        }

        /** Call all registered QueryListeners. */
        public void itemStateChanged(ItemEvent e) {
            _notifyListeners(_name);
        }
        private String _name;
    }

    /** Inner class to tie textArea to scroll pane */
    class QueryScrollPane extends JScrollPane {
        public JTextArea textArea;
        QueryScrollPane(JTextArea c) {
            super(c);
            textArea = c;
        }
        public String getText() {
            String retval = textArea.getText();
            return retval;
        }
        public void setText(String s) {
            textArea.setText(s);
        }
    }

    /** Listener for changes in slider.
     */
    class SliderListener implements ChangeListener {
        public SliderListener(String name) {
            _name = name;
        }
        /** Call all registered QueryListeners. */
        public void stateChanged(ChangeEvent event) {
            _notifyListeners(_name);
        }
        private String _name;
    }
}
