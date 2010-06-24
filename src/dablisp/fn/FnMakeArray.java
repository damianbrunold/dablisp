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

public class FnMakeArray extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCount(argc, 4);
        Obj dims = stack.pop();
        Symbol elementType = stack.pop().asSymbol();
        Obj initialElement = stack.pop();
        Obj fillPointer = stack.pop();
        if (dims == Symbol.NIL) {
            stack.push(new MultiDimArray(elementType, initialElement, new int[0]));
        } else if (dims.isIntNum()) {
            stack.push(new Vector(elementType, initialElement, fillPointer, (int) dims.asIntNum().intValue()));
        } else if (dims.length() == 1) {
            stack.push(new Vector(elementType, initialElement, fillPointer, (int) dims.first().asIntNum().intValue()));
        } else {
            int[] dimensions = new int[dims.length()];
            Cons p = dims.asCons();
            for (int i = 0; i < dimensions.length; i++) {
                dimensions[i] = (int) p.nth(i).asIntNum().intValue();
            }
            stack.push(new MultiDimArray(elementType, initialElement, dimensions));
        }
    }

    @Override
    public String getName() {
        return "%make-array";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " dimensions element-type initial-value fill-pointer) - function -  creates an array";
    }

}
