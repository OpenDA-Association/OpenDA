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
package org.openda.utils;
import java.awt.FlowLayout;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;

import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import org.openda.blackbox.config.BBUtils;

public final class UsernamePassword {

   private String username;
   private String password;
   private File   credentials;

   /**
    * Constructor without default username and password. Tries to obtain both a username and a password.
    *
    * @param server
    *           Server name (used in the settings filename).
    */
   public UsernamePassword(String server) {
      this(server, null, null);
   }

   /**
    * Constructor with default username and password. Tries to obtain a username and a password if any of these is missing.
    *
    * @param server
    *           Server name (used in the settings filename).
    * @param username
    *           Prefilled username (leave empty if none available).
    * @param password
    *           Prefilled password (leave empty if none available).
    */
   public UsernamePassword(String server, String username, String password) {
      this.username = username;
      this.password = password;

      // Check whether username and password are filled
      if (this.username != null && this.password != null &&
               !(this.username.isEmpty() || this.password.isEmpty())) return;

      // Try to read user name and password from file.
      this.credentials = BBUtils.getFileOnOpenDaTempDir("credentials." + server);
      readFromFile();

      // Check whether username and password are filled
      if (this.username != null && this.password != null &&
               !(this.username.isEmpty() || this.password.isEmpty())) return;

      // Ask for user name and password if these could not be read from file.
      Map<String, String> env = System.getenv();
      if ((env.get("TERM") == null || env.get("TERM").equalsIgnoreCase("xterm"))) {
         readFromGUI();
      }
      else {
         readFromTerminal();
      }

      // Store user name and password entered in the file
      writeToFile();
   }

   /**
    * @return The user name entered.
    */
   public String getUsername() {
      return this.username;
   }

   /**
    * @return The password entered.
    */
   public String getPassword() {
      return this.password;
   }

   /**
    * Ask a file for the username and password.
    */
   private void readFromFile() {
      try {
         FileReader fr = new FileReader(this.credentials);
         BufferedReader br = new BufferedReader(fr);
         this.username = br.readLine();
         this.password = br.readLine();
         br.close();
         fr.close();
      }
      catch (Exception e) {
         this.username = "";
         this.password = "";
      }
   }

   /**
    * Write the username and password to the file.
    */
   private void writeToFile() {
      try {
         FileWriter fw = new FileWriter(this.credentials);
         fw.write(this.username + '\n');
         fw.write(this.password + '\n');
         fw.close();
      }
      catch (Exception e) {
         // Empty
      }
   }

   /**
    * Ask using the terminal for the username and password.
    */
   private void readFromTerminal() {
      while (this.username == null || this.username.isEmpty()) {
         this.username = getString("Username: ");
      }
      while (this.password == null || this.password.isEmpty()) {
         this.password = getString("Password: ");
      }
   }

   /**
    * Ask a single line from the terminal.
    *
    * @param caption
    *           The caption of the line.
    * @return The value entered.
    */
   private String getString(String caption) {
      System.out.print(caption);

      // Get buffer with input
      byte[] buffer = new byte[64];
      int len;
      try {
         len = System.in.read(buffer);
      }
      catch (IOException e) {
         len = 0;
      }
      finally {
         // System.out.print('\n');
      }

      // Convert input buffer to string
      StringBuilder input = new StringBuilder();
      for (int i = 0; i < len; i++) {
         if (buffer[i] > 31) {
            input.append((char) (buffer[i]));
         }
      }

      return input.toString();
   }

   /**
    * Ask using a GUI for the username and password.
    */
   private void readFromGUI() {
      // Create fields for user name.
      final JTextField usernameField = new JTextField(20);
      usernameField.setText(this.username);
      final JLabel usernameLabel = new JLabel("Username: ");
      usernameLabel.setLabelFor(usernameField);
      final JPanel usernamePane = new JPanel(new FlowLayout(FlowLayout.TRAILING));
      usernamePane.add(usernameLabel);
      usernamePane.add(usernameField);

      // Create fields for password.
      final JPasswordField passwordField = new JPasswordField(20);
      passwordField.setText(this.password);
      final JLabel passwordLabel = new JLabel("Password: ");
      passwordLabel.setLabelFor(passwordField);
      final JPanel passwordPane = new JPanel(new FlowLayout(FlowLayout.TRAILING));
      passwordPane.add(passwordLabel);
      passwordPane.add(passwordField);

      // Create panel
      final JPanel main = new JPanel();
      main.setLayout(new BoxLayout(main, BoxLayout.PAGE_AXIS));
      main.add(usernamePane);
      main.add(passwordPane);

      // Create and handle dialog
      final JOptionPane jop = new JOptionPane(main, JOptionPane.QUESTION_MESSAGE, JOptionPane.OK_CANCEL_OPTION);
      final JDialog dialog = jop.createDialog("User name and password");
      dialog.addComponentListener(new ComponentAdapter() {
         @Override
         public void componentShown(ComponentEvent e) {
            SwingUtilities.invokeLater(new Runnable() {
               @Override
               public void run() {
                  if (usernameField.getText().isEmpty())
                  {
                     usernameField.requestFocusInWindow();
                  }
                  else
                  {
                     passwordField.requestFocusInWindow();
                  }
               }
            });
         }
      });
      dialog.setVisible(true);
      final Integer result = (Integer) jop.getValue();
      dialog.dispose();
      if (result.intValue() == JOptionPane.OK_OPTION) {
         this.username = usernameField.getText();

         final char[] pwd = passwordField.getPassword();
         this.password = new String(pwd);
      }
   }
}
