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
import dablisp.Obj;
import dablisp.Stack;
import dablisp.Strm;

public class FnPrin1 extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCountRange(argc, 1, 2);
        Obj val = stack.pop();
        Strm out = Lisp.getStandardOutput();
        if (argc == 2) {
            out = stack.pop().asStrm();
        }
        out.prin1(val);
        stack.push(val);
    }

    @Override
    public String getName() {
        return "prin1";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " obj &optional stream) - function -  prints the obj to the stream or standard output";
    }

}
