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
package dablisp.fn;

import dablisp.*;

public class FnCharpred extends Fn {

    private String pred;

    public FnCharpred(String pred) {
        this.pred = pred;
    }

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        if (pred.equals("/=") || pred.equals("not-equal")) {
            checkArgCountMin(argc, 2);
            Char[] chars = new Char[argc];
            for (int i = 0; i < argc; i++) chars[i] = stack.pop().asChar();
            for (int i = 0; i < argc - 1; i++) {
                for (int j = i + 1; j < argc; j++) {
                    if (!pred(chars[i], chars[j])) {
                        stack.push(Symbol.NIL);
                        return;
                    }
                }
            }
            stack.push(Symbol.T);
        } else {
            checkArgCountMin(argc, 2);
            Char ch = stack.pop().asChar();
            Obj result = Symbol.T;
            for (int i = 1; i < argc; i++) {
                Char ch2 = stack.pop().asChar();
                if (result == Symbol.T && !pred(ch, ch2)) {
                    result = Symbol.NIL;
                }
                ch = ch2;
            }
            stack.push(result);
        }
    }

    private boolean pred(Char ch1, Char ch2) {
        if (pred.equals("=")) {
            return ch1.getValue() == ch2.getValue();
        } else if (pred.equals("/=")) {
            return ch1.getValue() != ch2.getValue();
        } else if (pred.equals("<")) {
            return ch1.getValue() < ch2.getValue();
        } else if (pred.equals("<=")) {
            return ch1.getValue() <= ch2.getValue();
        } else if (pred.equals(">")) {
            return ch1.getValue() > ch2.getValue();
        } else if (pred.equals(">=")) {
            return ch1.getValue() >= ch2.getValue();
        } else if (pred.equals("equal")) {
            return Character.toLowerCase(ch1.getValue()) == Character.toLowerCase(ch2.getValue());
        } else if (pred.equals("not-equal")) {
            return Character.toLowerCase(ch1.getValue()) != Character.toLowerCase(ch2.getValue());
        } else if (pred.equals("lessp")) {
            return Character.toLowerCase(ch1.getValue()) < Character.toLowerCase(ch2.getValue());
        } else if (pred.equals("not-greaterp")) {
            return Character.toLowerCase(ch1.getValue()) <= Character.toLowerCase(ch2.getValue());
        } else if (pred.equals("greaterp")) {
            return Character.toLowerCase(ch1.getValue()) > Character.toLowerCase(ch2.getValue());
        } else if (pred.equals("not-lessp")) {
            return Character.toLowerCase(ch1.getValue()) >= Character.toLowerCase(ch2.getValue());
        } else {
            throw new LispException(Symbol.GENERALCONDITION, "unknown pred " + pred);
        }
    }

    @Override
    public String getName() {
        return "char" + (Character.isLetter(pred.charAt(0)) ? "-" : "") + pred;
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " &rest chars) - function -  returns true if the CHARS are ordered according to " + pred;
    }

}
