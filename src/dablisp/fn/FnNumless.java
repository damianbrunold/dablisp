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
import dablisp.Number;
import dablisp.Stack;
import dablisp.Symbol;

public class FnNumless extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        if (argc == 0) {
            stack.push(Symbol.T);
            return;
        }
        Number current = stack.pop().asNumber();
        Symbol result = Symbol.T;
        for (int i = 1; i < argc; i++) {
            Number next = stack.pop().asNumber();
            if (result == Symbol.T && !current.less(next)) {
                result = Symbol.NIL;
            }
            current = next;
        }
        stack.push(result);
    }

    @Override
    public String getName() {
        return "<";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " &rest args) - function -  returns true if the args are ordered by <";
    }

}
