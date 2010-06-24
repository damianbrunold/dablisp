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

public class IntNum extends Number {

    public static final IntNum ZERO = new IntNum(0);
    public static final IntNum ONE = new IntNum(1);

    private long val;

    public static IntNum fromLong(long val) {
        if (val == 0) return ZERO;
        else if (val == 1) return ONE;
        else return new IntNum(val);
    }

    private IntNum(long val) {
        this.val = val;
    }

    public long getValue() {
        return val;
    }

    @Override
    public Number add(Number n) {
        if (n.isFloating()) return n.add(this);
        return new IntNum(val + n.intValue());
    }

    @Override
    public Number subtract(Number n) {
        if (n.isFloating()) return new FloatNum(this).subtract(n);
        return new IntNum(val - n.intValue());
    }

    @Override
    public Number multiply(Number n) {
        if (n.isFloating()) return n.multiply(this);
        return new IntNum(val * n.intValue());
    }

    @Override
    public Number divide(Number n) {
        if (n.isFloating()) return new FloatNum(this).divide(n);
        if (n.intValue() == 0) throw new LispException(Symbol.DIVZEROCONDITION, "division by zero");
        long r = val / n.intValue();
        if (r * n.intValue() == val) return new IntNum(r);
        else return new FloatNum(this).divide(n);
    }

    @Override
    public boolean isFloating() {
        return false;
    }

    @Override
    public boolean isInteger() {
        return true;
    }

    @Override
    public boolean isIntegral() {
        return true;
    }

    @Override
    public double doubleValue() {
        return val;
    }

    @Override
    public long intValue() {
        return val;
    }

    @Override
    public String toString() {
        return Long.toString(val);
    }

    @Override
    public String displayValue() {
        return Long.toString(val);
    }

    @Override
    public boolean isIntNum() { return true; }

    @Override
    public IntNum asIntNum() { return this; }

    @Override
    public boolean eql(Number n) {
        if (n.isFloating()) return n.eql(this);
        return n.intValue() == val;
    }

    @Override
    public boolean noteql(Number n) {
    	return !eql(n);
    }

    @Override
    public boolean less(Number n) {
    	if (n.isFloating()) return n.less(this);
    	return val < n.intValue();
    }

    @Override
    public boolean lesseql(Number n) {
    	return less(n) || eql(n);
    }

    @Override
    public boolean greater(Number n) {
    	return !lesseql(n);
    }

    @Override
    public boolean greatereql(Number n) {
    	return !less(n);
    }

    @Override
    public IntNum floor() {
        return this;
    }

    @Override
    public IntNum ceiling() {
        return this;
    }

    @Override
    public IntNum truncate() {
        return this;
    }

    @Override
    public IntNum round() {
        return this;
    }

    @Override
    public Obj types() { return new Cons(Symbol.INTEGER, super.types()); }

    @Override
    public IntNum sxhash() { return new IntNum(val); }

}
