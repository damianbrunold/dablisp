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

public class FnTraces extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCountRange(argc, 1, 2);
        Symbol action = stack.pop().asSymbol();
        if (action == Symbol.intern("add")) {
            Obj o = stack.pop();
            while (o != Symbol.NIL) {
                Lisp.traces.add(o.first().asSymbol().getName());
                o = o.rest();
            }
        } else if (action == Symbol.intern("remove")) {
            Obj o = stack.pop();
            while (o != Symbol.NIL) {
                Lisp.traces.remove(o.first().asSymbol().getName());
                o = o.rest();
            }
        } else if (action == Symbol.intern("removeall")) {
            Lisp.traces.clear();
        } else if (action == Symbol.intern("show")) {
            for (String s : Lisp.traces) {
                Lisp.getStandardOutput().print(s);
            }
        }
        stack.push(new Mvals());
    }

    @Override
    public String getName() {
        return "traces";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " action &optional symbols) - function -  action 'add adds the symbol to the traces, action 'remove removes it, action 'removeall removes all";
    }

}
