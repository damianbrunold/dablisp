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

import java.io.IOException;

import dablisp.Char;
import dablisp.Env;
import dablisp.Fn;
import dablisp.Lisp;
import dablisp.LispException;
import dablisp.Obj;
import dablisp.Stack;
import dablisp.Strm;
import dablisp.Symbol;

public class FnReadchar extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCountRange(argc, 0, 4);
        Strm strm = Lisp.getStandardInput();
        Obj eoferrorp = Symbol.T;
        Obj eofvalue = Symbol.NIL;
        // Obj recursivep = Symbol.NIL;
        if (argc >= 1)
            strm = stack.pop().asStrm();
        if (argc >= 2)
            eoferrorp = stack.pop();
        if (argc >= 3)
            eofvalue = stack.pop();
        // if (len >= 4) recursivep = params.fourth();
        int ch = -1;
        try {
            ch = strm.getIn().read();
        } catch (IOException e) {
            throw new LispException(Symbol.IOCONDITION, "could not read char: " + e.getMessage());
        }
        if (ch == -1) {
            if (eoferrorp != Symbol.NIL) {
                throw new LispException(Symbol.IOCONDITION, "eof");
            }
            stack.push(eofvalue);
        } else {
            stack.push(new Char((char) ch));
        }
    }

    @Override
    public String getName() {
        return "read-char";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " &optional stream eof-error-p eof-value recursive-p) - function -  reads a character from stream or standard-input";
    }

}
