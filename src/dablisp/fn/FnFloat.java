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
import dablisp.FloatNum;
import dablisp.Fn;
import dablisp.Number;
import dablisp.Stack;

public class FnFloat extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCount(argc, 1);
        Number n = stack.pop().asNumber();
        if (n instanceof FloatNum) stack.push(n);
        else stack.push(new FloatNum(n.asIntNum()));
    }

    @Override
    public String getName() {
        return "float";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " number) - function -  converts the number into a float";
    }

}