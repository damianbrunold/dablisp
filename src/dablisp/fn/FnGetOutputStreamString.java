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

public class FnGetOutputStreamString extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCount(argc, 1);
        StringOutputStrm strm = (StringOutputStrm) stack.pop().asStrm();
        Vector result = new Vector(strm.getOutput());
        strm.reset();
        stack.push(result);
    }

    @Override
    public String getName() {
        return "get-output-stream-string";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " strm) - function -  returns the string contents of the string output stream STRM";
    }

}
