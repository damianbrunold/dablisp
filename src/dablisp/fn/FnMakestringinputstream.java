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

public class FnMakestringinputstream extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCountRange(argc, 1, 3);
        Vector str = stack.pop().asStr();
        int start = 0;
        String val = str.getStrValue();
        int end = val.length();
        if (argc >= 2) start = (int) stack.pop().asIntNum().intValue();
        if (argc >= 3) end = (int) stack.pop().asIntNum().intValue();
        String s = val.substring(start, end);
        stack.push(new Strm(new ReadStream(s)));
    }

    @Override
    public String getName() {
        return "make-string-input-stream";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " str &optional start end) - function -  returns a string input stream based on str";
    }

}
