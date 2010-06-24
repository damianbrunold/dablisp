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

public class FnCharacter extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCount(argc, 1);
        Obj o = stack.pop();
        if (o.isChar()) {
            stack.push(o);
            return;
        } else if (o.isNumber() && o instanceof IntNum) {
            stack.push(new Char((char) o.asIntNum().intValue()));
            return;
        } else if (o.isStr() && o.asStr().getStrValue().length() == 1) {
            stack.push(new Char(o.asStr().getStrValue().charAt(0)));
            return;
        } else if (o.isSymbol() && o.asSymbol().getName().length() == 1) {
            stack.push(new Char(o.asSymbol().getName().charAt(0)));
            return;
        }
        throw new LispException(Symbol.GENERALCONDITION, "cannot coerce " + o + " to a character");
    }

    @Override
    public String getName() {
        return "character";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " obj) - function -  converts the obj into a character if possible";
    }

}
