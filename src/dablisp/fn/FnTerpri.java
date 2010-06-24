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

import dablisp.Env;
import dablisp.Fn;
import dablisp.Lisp;
import dablisp.Stack;
import dablisp.Strm;
import dablisp.Symbol;

public class FnTerpri extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCountRange(argc, 0, 1);
        Strm out = Lisp.getStandardOutput();
        if (argc == 1) {
            out = stack.pop().asStrm();
        }
        out.print();
        stack.push(Symbol.NIL);
    }

    @Override
    public String getName() {
        return "terpri";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " &optional stream) - function -  writes a line terminator to stream or standard output";
    }

}
