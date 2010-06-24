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
import dablisp.Mvals;
import dablisp.Stack;
import dablisp.Strm;
import dablisp.Symbol;

public class FnRoom extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCountRange(argc, 0, 1);
        Symbol x = Symbol.intern(":default");
        if (argc == 1) {
            x = stack.pop().asSymbol();
        }
        Runtime rt = Runtime.getRuntime();
        Strm out = Lisp.getStandardOutput();
        out.print(String.format("mem avail: %d%%", 100L * (rt.freeMemory() + (rt.maxMemory() - rt.totalMemory())) / rt.maxMemory()));
        if (x != Symbol.NIL) {
            out.print(String.format("memory: free %d, total %d, max %d", rt.freeMemory(), rt.totalMemory(), rt.maxMemory()));
        }
        stack.push(new Mvals());
    }

    @Override
    public String getName() {
        return "room";
    }

    @Override
    public String getDocstring() {
        return "(" + getName()
                + " &optional x) - function -  prints information about internal state to standard out. Use for x one of t, :default and nil.";
    }

}
