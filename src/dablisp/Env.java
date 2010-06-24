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
package dablisp;

import java.util.*;

public class Env {

    private Env parent;
    private Map<Symbol, Obj> vars = new HashMap<Symbol, Obj>();

    public static Env createRootEnv() {
        return extend(null);
    }

    public static Env extend(Env env) {
        return new Env(env);
    }

    private Env(Env parent) {
        this.parent = parent;
    }

    public Env discardEnv() {
        if (parent != null) return parent;
        return this;
    }

    public boolean isRootEnv() {
        return parent == null;
    }

    public void bind(Symbol symbol, Obj value) {
        vars.put(symbol, value);
    }

    public void set(Symbol symbol, Obj value) {
        if (vars.containsKey(symbol)) {
            vars.put(symbol, value);
            return;
        }
        if (parent != null) {
            parent.set(symbol, value);
            return;
        }
        throw new LispException(Symbol.UNBOUNDCONDITION, symbol);
    }

    public Obj get(Symbol symbol) {
        Obj val = vars.get(symbol);
        if (val != null) {
            return val;
        }
        if (parent != null) {
            return parent.get(symbol);
        }
        throw new LispException(Symbol.UNBOUNDCONDITION, symbol);
    }

    public boolean boundp(Symbol symbol) {
        if (vars.containsKey(symbol)) {
            return true;
        }
        if (parent != null) {
            return parent.boundp(symbol);
        }
        return false;
    }

    public Set<Symbol> symbols() {
        Set<Symbol> symbols = new TreeSet<Symbol>();
        collectSymbols(symbols);
        return symbols;
    }

    private void collectSymbols(Set<Symbol> symbols) {
        symbols.addAll(vars.keySet());
        if (parent != null) parent.collectSymbols(symbols);
    }

}
