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

public class FnLogop extends Fn {

    private String op;

    public FnLogop(String op) {
        this.op = op;
    }

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCountMin(argc, 1);
        long n = stack.pop().asIntNum().intValue();
        for (int i = 1; i < argc; i++) {
            IntNum m = stack.pop().asIntNum();
            if (op.equals("ior")) {
                n = n | m.intValue();
            } else if (op.equals("and")) {
                n = n & m.intValue();
            } else if (op.equals("xor")) {
                n = n ^ m.intValue();
            }
        }
        stack.push(IntNum.fromLong(n));
    }

    @Override
    public String getName() {
        return "log" + op;
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " n &rest args) - function -  performs the operation " + op + " on n and all args";
    }

}
