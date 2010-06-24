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

import java.lang.reflect.Method;

import dablisp.*;

public class FnInvoke extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCountMin(argc, 2);
        Native obj = stack.pop().asNative();
        Symbol fn = stack.pop().asSymbol();
        Obj args = stack.popList(argc - 2);
        Obj result = Symbol.NIL;
        Object o = obj.getNativeObject();
        try {
            Method method = o.getClass().getMethod(fn.getName(), NativeAccess.getTypes(args));
            result = new Native(method.invoke(o, NativeAccess.getValues(args)));
        } catch (Exception e) {
            throw new LispException(Symbol.INVOKECONDITION, "cannot call method " + fn, e);
        }
        stack.push(result);
    }

    @Override
    public String getName() {
        return "invoke";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " native-obj symbol &rest args) - function -  invokes the method symbol on the native object native-obj with args.";
    }

}
