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

public class FnDisassemble extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        Fn fn = stack.pop().asFn();
        if (fn.isPrimitive()) {
            Lisp.getStandardOutput().print();
            Lisp.getStandardOutput().print("(primitive function)");
        } else {
            FnCompiledDef func = (FnCompiledDef) fn;
            Lisp.getStandardOutput().print();
            Lisp.getStandardOutput().print(func.code);
        }
        stack.push(new Mvals());
    }

    @Override
    public String getName() {
        return "disassemble";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " fn) - function -  disassemble";
    }

}
