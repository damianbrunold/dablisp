/*  dablisp - a kind of common lisp subset written in java
    Copyright 2008 Damian Brunold

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
package dablisp.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.FocusTraversalPolicy;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;

import javax.swing.*;

import dablisp.Lisp;
import dablisp.LispException;
import dablisp.Obj;
import dablisp.Strm;

public class LispConsoleFrame extends JFrame {

    private static final long serialVersionUID = 1L;

    private static final String EOL = System.getProperty("line.separator");

    public LispConsoleFrame() {
        super("dablisp console");
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        setLayout(new BorderLayout());
        JPanel pane = new JPanel();
        add(pane, BorderLayout.CENTER);
        pane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
        pane.setLayout(new BorderLayout());
        final JTextArea output = new JTextArea();
        final JTextField input = new JTextField();
        JScrollPane sp = new JScrollPane(output, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        output.setBorder(BorderFactory.createEmptyBorder(0, 10, 10, 10));
        pane.add(sp, BorderLayout.CENTER);
        pane.add(input, BorderLayout.SOUTH);
        //output.setEditable(false);
        output.setLineWrap(true);
        output.setWrapStyleWord(false);
        input.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ByteArrayOutputStream baos = new ByteArrayOutputStream();
                PrintStream p = new PrintStream(baos);
                Strm strm = new Strm(p);
                Lisp.setErrorOutput(strm);
                Lisp.setStandardOutput(strm);
                Lisp.setTraceOutput(strm);
                try {
                    if (!input.getText().isEmpty()) {
                        addToOutput("> " + input.getText());
                        Obj result = Lisp.eval(input.getText());
                        p.flush();
                        addToOutput(baos.toString());
                        if (result.isMvals()) {
                            for (int i = 0; i < result.asMvals().size(); i++) {
                                addToOutput(result.asMvals().val(i).toString());
                            }
                        } else {
                            addToOutput(result.toString());
                        }
                    }
                } catch (LispException ex) {
                    addToOutput(ex);
                }
                output.setCaretPosition(output.getText().length());
                input.setText("");
            }
            private void addToOutput(String result) {
                if (result != null && !result.isEmpty()) {
                    output.setText(output.getText() + EOL + result);
                }
            }
            private void addToOutput(LispException e) {
                String s = output.getText();
                s += EOL + e.tag + ": " + e.value;
                String b = getBacktrace(e);
                if (!b.isEmpty()) {
                    s += EOL + EOL + b;
                }
                output.setText(s);
            }
            private String getBacktrace(LispException e) {
                StringWriter s = new StringWriter();
                PrintWriter p = new PrintWriter(s);
                e.printBacktrace(p);
                p.flush();
                return s.toString();
            }
        });
        setFocusTraversalPolicy(new FocusTraversalPolicy() {
            @Override
            public Component getComponentAfter(Container container, Component component) {
                return component == input ? output : input;
            }
            @Override
            public Component getComponentBefore(Container container, Component component) {
                return component == input ? output : input;
            }
            @Override
            public Component getDefaultComponent(Container container) {
                return input;
            }
            @Override
            public Component getFirstComponent(Container container) {
                return input;
            }
            @Override
            public Component getLastComponent(Container container) {
                return output;
            }
        });
        setSize(800, 600);
        setLocationRelativeTo(null);
    }

    public static void main(String... args) {
        new LispConsoleFrame().setVisible(true);
    }

}
