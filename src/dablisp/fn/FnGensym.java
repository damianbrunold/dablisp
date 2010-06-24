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
import dablisp.Stack;
import dablisp.Symbol;

public class FnGensym extends Fn {

    private static int gensymcount = 1;

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCountRange(argc, 0, 1);
        if (argc == 0) stack.push(new Symbol("#:g" + gensymcount++));
        else stack.push(new Symbol(stack.pop().asStr().getStrValue() + gensymcount++));
    }

    @Override
    public String getName() {
        return "gensym";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " &optional prefix) - function -  returns a new unique uninterned symbol";
    }

}
