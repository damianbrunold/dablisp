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

public class DEnv
{
    private int pointer = 0;
    private List<Binding> bindings = new ArrayList<Binding>();
    private LinkedList<Integer> marks = new LinkedList<Integer>();

    public int getLevel() {
        return marks.size();
    }

    public void setLevel(int level) {
        while (marks.size() > level) {
            restore();
        }
    }

    public void mark() {
        marks.push(pointer);
    }

    public void restore() {
        int ptr = marks.pop();
        for (int i = ptr; i < this.pointer; i++) {
            bindings.remove(ptr);
        }
        this.pointer = ptr;
    }

    public Binding binding(Symbol symbol) {
        for (int i = pointer - 1; i >= 0; i--) {
            Binding b = bindings.get(i);
            if (b.symbol == symbol) {
                return b;
            }
        }
        return null;
    }

    public boolean boundp(Symbol symbol) {
        return binding(symbol) != null;
    }

    public Obj get(Symbol symbol) {
        Binding b = binding(symbol);
        if (b == null) {
            throw new LispException(Symbol.UNBOUNDCONDITION, symbol);
        }
        return b.value;
    }

    public void bind(Symbol symbol, Obj value) {
        if (bindings.size() != pointer) throw new RuntimeException("bindings.size != pointer");
        bindings.add(new Binding(symbol, value));
        pointer++;
    }

    public void set(Symbol symbol, Obj value) {
        Binding b = binding(symbol);
        if (b == null) {
            throw new LispException(Symbol.UNBOUNDCONDITION, symbol);
        }
        b.value = value;
    }

    public Set<Symbol> symbols() {
        Set<Symbol> symbols = new TreeSet<Symbol>();
        for (int i = 0; i < pointer; i++) {
            symbols.add(bindings.get(i).symbol);
        }
        return symbols;
    }

    @Override
    public String toString() {
        StringBuilder b = new StringBuilder();
        b.append("denv: ");
        for (int i = pointer - 1; i >= 0; i--) {
            b.append(bindings.get(i)).append(" ");
        }
        return b.toString().trim();
    }

}

