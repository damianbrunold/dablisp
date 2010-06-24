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

public abstract class Number extends Atom {

    public abstract long intValue();
    public abstract double doubleValue();

    public abstract boolean isInteger();
    public abstract boolean isFloating();
    public abstract boolean isIntegral();

    public abstract Number add(Number n);
    public abstract Number subtract(Number n);
    public abstract Number multiply(Number n);
    public abstract Number divide(Number n);

    public abstract boolean eql(Number n);
    public abstract boolean noteql(Number n);

    public abstract boolean less(Number n);
    public abstract boolean lesseql(Number n);
    public abstract boolean greater(Number n);
    public abstract boolean greatereql(Number n);

    public abstract IntNum floor();
    public abstract IntNum ceiling();
    public abstract IntNum truncate();
    public abstract IntNum round();

    @Override
    public boolean isNumber() { return true; }

    @Override
    public Number asNumber() { return this; }

    @Override
    public Obj types() { return new Cons(Symbol.REAL, new Cons(Symbol.NUMBER, super.types())); }

}
