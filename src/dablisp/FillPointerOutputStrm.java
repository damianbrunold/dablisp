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

public class FillPointerOutputStrm extends Strm {

    private Vector str;

    public FillPointerOutputStrm(Vector str) {
        this.str = str;
    }

    @Override
    public String toString() {
        return "#<fill-pointer-output-strm>";
    }

    @Override
    public String displayValue() {
        return toString();
    }

    @Override
    public boolean isStrm() { return true; }

    @Override
    public Strm asStrm() { return this; }

    @Override
    public void print(Object val) {
        str.append(System.getProperty("line.separator"));
        str.append(val.toString());
    }

    @Override
    public void prin1(Obj val) {
        str.append(val.toString());
    }

    @Override
    public void princ(Obj val) {
        str.append(val.displayValue());
    }

    @Override
    public void print() {
        str.append(System.getProperty("line.separator"));
    }

    @Override
    public Obj types() { return new Cons(Symbol.intern("fill-pointer-output-stream"), new Cons(Symbol.STREAM, super.types())); }

    @Override
    public IntNum sxhash() { return IntNum.ZERO; }

}
