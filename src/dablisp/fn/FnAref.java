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

public class FnAref extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCountMin(argc, 2);
        Array a = stack.pop().asArray();
        if (a.isVector()) {
            try {
                stack.push(a.asVector().get((int) stack.pop().asIntNum().intValue()));
            } catch (ArrayIndexOutOfBoundsException e) {
                throw new LispException(Symbol.intern("index-too-large"), "index too large");
            }
        } else {
            int[] coord = new int[a.dimensions()];
            for (int i = 0; i < a.dimensions(); i++) {
                coord[i] = (int) stack.pop().asIntNum().intValue();
            }
            try {
                stack.push(a.get(coord));
            } catch (ArrayIndexOutOfBoundsException e) {
                throw new LispException(Symbol.intern("index-too-large"), "index too large");
            }
        }
    }

    @Override
    public String getName() {
        return "aref";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " array &rest subscripts) - function -  Accesses the ARRAY using the SUBSCRIPTS and returns the found element";
    }

}
