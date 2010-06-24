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

public class FnAset extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCountMin(argc, 3);
        Array a = stack.pop().asArray();
        if (a.isVector()) {
            Obj val = stack.pop();
            int idx = (int) stack.pop().asIntNum().intValue();
            a.asVector().set(val, idx);
        } else {
            Obj val = stack.pop();
            int[] coord = new int[a.dimensions()];
            for (int i = 0; i < a.dimensions(); i++) {
                coord[i] = (int) stack.pop().asIntNum().intValue();
            }
            a.set(val, coord);
        }
        stack.push(a);
    }

    @Override
    public String getName() {
        return "aset";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " array value &rest subscripts) - function -  Accesses the ARRAY using the SUBSCRIPTS and sets the VALUE";
    }

}
