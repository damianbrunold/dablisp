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

public class FloatNum extends Number {

    private double val;

    public FloatNum(double val) {
        this.val = val;
    }

    public FloatNum(IntNum val) {
        this.val = val.doubleValue();
    }

    public double getValue() {
        return val;
    }

    @Override
    public Number add(Number n) {
        double result = val + n.doubleValue();
        if (isIntRepresentable(result)) return IntNum.fromLong((long)result);
        return new FloatNum(result);
    }

    @Override
    public Number subtract(Number n) {
        double result = val - n.doubleValue();
        if (isIntRepresentable(result)) return IntNum.fromLong((long)result);
        return new FloatNum(result);
    }

    @Override
    public Number multiply(Number n) {
        double result = val * n.doubleValue();
        if (isIntRepresentable(result)) return IntNum.fromLong((long)result);
        return new FloatNum(result);
    }

    @Override
    public Number divide(Number n) {
        if (n.doubleValue() == 0.0) throw new LispException(Symbol.DIVZEROCONDITION, "division by zero");
        double result = val / n.doubleValue();
        if (isIntRepresentable(result)) return IntNum.fromLong((long)result);
        return new FloatNum(result);
    }

    @Override
    public boolean isFloating() {
        return true;
    }

    @Override
    public boolean isInteger() {
        return false;
    }

    @Override
    public boolean isIntegral() {
        return Math.floor(val) == val;
    }

    @Override
    public double doubleValue() {
        return val;
    }

    @Override
    public long intValue() {
        return (long) val;
    }

    private boolean isIntRepresentable(double result) {
        return Math.floor(result) == result && result < Long.MAX_VALUE;
    }

    @Override
    public String toString() {
        return Double.toString(val);
    }

    @Override
    public String displayValue() {
        return Double.toString(val);
    }

    @Override
    public boolean isFloatNum() { return true; }

    @Override
    public FloatNum asFloatNum() { return this; }

    @Override
    public boolean eql(Number n) {
        return n.doubleValue() == doubleValue();
    }

    @Override
    public boolean noteql(Number n) {
    	return !eql(n);
    }

    @Override
    public boolean less(Number n) {
    	return doubleValue() < n.doubleValue();
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
        return IntNum.fromLong((int) Math.floor(val));
    }

    @Override
    public IntNum ceiling() {
        return IntNum.fromLong((int) Math.ceil(val));
    }

    @Override
    public IntNum truncate() {
        return IntNum.fromLong((int) val);
    }

    @Override
    public IntNum round() {
        return IntNum.fromLong((int) Math.rint(val));
    }

    @Override
    public Obj types() { return new Cons(Symbol.FLOAT, super.types()); }

    @Override
    public IntNum sxhash() { return IntNum.fromLong(Double.valueOf(val).hashCode()); }

}
