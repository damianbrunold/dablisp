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
import dablisp.Obj;
import dablisp.Stack;
import dablisp.Symbol;

public class FnNumnoteq extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        if (argc == 0) {
            stack.push(Symbol.T);
            return;
        }
        Obj c = stack.popList(argc);
        while (c != Symbol.NIL) {
            if (existsEqlOne(c.first().asNumber(), c.rest())) {
                stack.push(Symbol.NIL);
                return;
            }
            c = c.rest();
        }
        stack.push(Symbol.T);
    }

    public boolean existsEqlOne(Number c, Obj rest) {
        while (rest != Symbol.NIL) {
            if (c.eql(rest.first().asNumber())) {
                return true;
            }
            rest = rest.rest();
        }
        return false;
    }

    @Override
    public String getName() {
        return "/=";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " &rest args) - function -  returns true if no two args are equal";
    }

}
