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

import java.util.Set;
import java.util.TreeSet;

import dablisp.*;

public class FnApropos extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCount(argc, 1);
        Vector str = stack.pop().asStr();
        Set<Symbol> symbols = new TreeSet<Symbol>();
        symbols.addAll(fenv.symbols());
        symbols.addAll(env.symbols());
        symbols.addAll(Lisp.vm.denv.symbols());
        for (Symbol s : symbols) {
            if (s.getName().contains(str.getStrValue())) {
                Lisp.getStandardOutput().print(s);
            }
        }
    	stack.push(new Mvals());
    }

    @Override
    public String getName() {
        return "apropos";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + " str) - function -  prints symbols whose name contains STR to standard output";
    }

}
