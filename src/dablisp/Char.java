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

public class Char extends Atom {

    private char ch;

    public Char(char ch) {
        this.ch = ch;
    }

    public char getValue() {
        return ch;
    }

    @Override
    public String toString() {
        if (ch == '\n') return "#\\Newline";
        if (ch == '\r') return "#\\Return";
        if (ch == ' ') return "#\\Space";
        if (ch == '\t') return "#\\Tab";
        return "#\\" + ch;
    }

    @Override
    public String displayValue() {
        return Character.toString(ch);
    }

    @Override
    public boolean isChar() { return true; }

    @Override
    public Char asChar() { return this; }

    @Override
    public Obj types() { return new Cons(Symbol.CHARACTER, super.types()); }

    @Override
    public IntNum sxhash() { return IntNum.fromLong(ch); }

}
