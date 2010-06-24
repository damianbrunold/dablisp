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

public class Binding {

    public Symbol symbol;
    public Obj value;

    public Binding(Symbol symbol) {
        this(symbol, null);
    }

    public Binding(Symbol symbol, Obj value) {
        this.symbol = symbol;
        this.value = value;
    }

    public boolean isBound() {
        return value != null;
    }

    public void unbind() {
        value = null;
    }

    public void bind(Obj value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return symbol + "=" + value;
    }

}
