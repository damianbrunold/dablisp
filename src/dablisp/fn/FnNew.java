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

import java.lang.reflect.Constructor;

import dablisp.Env;
import dablisp.Fn;
import dablisp.LispException;
import dablisp.Native;
import dablisp.NativeAccess;
import dablisp.Obj;
import dablisp.Stack;
import dablisp.Symbol;

public class FnNew extends Fn {

    @Override
    @SuppressWarnings("unchecked")
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCountMin(argc, 1);
        Symbol fn = stack.pop().asSymbol();
        Obj args = stack.popList(argc - 1);
        try {
            Class c = Class.forName(fn.getName());
            if (args == Symbol.NIL) {
                stack.push(new Native(c.newInstance()));
                return;
            }
            Constructor ctor = c.getConstructor(NativeAccess.getTypes(args));
            stack.push(new Native(ctor.newInstance(NativeAccess.getValues(args))));
        } catch (Exception e) {
            throw new LispException(Symbol.INVOKECONDITION, "cannot construct " + fn, e);
        }
    }

    @Override
    public String getName() {
        return "new";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " symbol &rest args) - function -  constructs a native object by calling the constructor symbol with the args";
    }

}
