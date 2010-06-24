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

public class Vector extends Array {

    private int size;
    private Obj[] data;
    private int fillpointer = -1;

    public Vector(Symbol elementType, Obj initialElement, int size) {
        this(elementType, initialElement, Symbol.NIL, size);
    }

    public Vector(Symbol elementType, Obj initialElement, Obj fillPointer, int size) {
        super(elementType);
        if (elementType == Symbol.CHAR && initialElement == Symbol.NIL) initialElement = new Char(' ');
        this.size = size;
        data = new Obj[size];
        for (int i = 0; i < size; i++) data[i] = initialElement;
        if (fillPointer == Symbol.NIL) {
            fillpointer = -1;
        } else if (fillPointer == Symbol.T) {
            fillpointer = size;
        } else {
            fillpointer = (int) fillPointer.asIntNum().intValue();
        }
    }

    public Vector(String value) {
        super(Symbol.CHAR);
        this.size = value.length();
        this.fillpointer = -1;
        data = new Obj[size];
        for (int i = 0; i < size; i++) {
            data[i] = new Char(value.charAt(i));
        }
    }

    public boolean hasFillPointer() {
        return fillpointer != -1;
    }

    public IntNum fillpointer() {
        if (fillpointer == -1) throw new LispException(Symbol.GENERALCONDITION, "vector does not have a fill-pointer");
        return IntNum.fromLong(fillpointer);
    }

    public void setFillPointer(int fillpointer) {
        if (fillpointer < 0 || fillpointer > size) throw new LispException(Symbol.GENERALCONDITION, "fill-pointer out of bounds");
        this.fillpointer = fillpointer;
    }

    public void append(Vector v) {
        int newsize = length() + v.length();
        Obj[] newdata = new Obj[newsize];
        for (int i = 0; i < length(); i++) newdata[i] = data[i];
        for (int i = length(); i < newsize; i++) newdata[i] = v.get(i - length());
        size = newsize;
        if (fillpointer != -1) fillpointer = size;
        data = newdata;
    }

    public void append(String s) {
        int newsize = length() + s.length();
        Obj[] newdata = new Obj[newsize];
        for (int i = 0; i < length(); i++) newdata[i] = data[i];
        for (int i = length(); i < newsize; i++) newdata[i] = new Char(s.charAt(i - length()));
        size = newsize;
        if (fillpointer != -1) fillpointer = size;
        data = newdata;
    }

    @Override
    public int dimensions() {
        return 1;
    }

    @Override
    public int length() {
        return fillpointer == -1 ? size : fillpointer;
    }

    @Override
    public int length(int dimension) {
        if (dimension != 0) throw new LispException(Symbol.GENERALCONDITION, "vector has only one dimension");
        return size;
    }

    @Override
    public Obj get(int... coord) {
        if (coord.length != 1) throw new LispException(Symbol.GENERALCONDITION, "vector has only one dimension");
        return data[coord[0]];
    }

    @Override
    public void set(Obj val, int... coord)  {
        if (coord.length != 1) throw new LispException(Symbol.GENERALCONDITION, "vector has only one dimension");
        if (elementType == Symbol.CHAR) {
            if (!val.isChar()) throw new LispException(Symbol.GENERALCONDITION, "string can only contain chars");
        }
        data[coord[0]] = val;
    }

    public Obj get(int index) {
        return data[index];
    }

    public void set(Obj val, int index) {
        if (elementType == Symbol.CHAR) {
            if (!val.isChar()) throw new LispException(Symbol.GENERALCONDITION, "string can only contain chars");
        }
        data[index] = val;
    }

    @Override
    public IntNum sxhash() {
        int code = 0;
        for (int i = 0; i < size; i++) code ^= data[i].sxhash().intValue();
        return IntNum.fromLong(code);
    }

    @Override
    public String toString() {
        StringBuilder b = new StringBuilder();
        if (elementType == Symbol.CHAR) {
            b.append('"');
            for (int i = 0; i < length(); i++) {
                if (data[i].asChar().getValue() == '"') {
                    b.append('\\');
                }
                b.append(data[i].asChar().getValue());
            }
            b.append('"');
        } else {
            b.append("#(");
            for (int i = 0; i <  length() - 1; i++) {
                b.append(data[i]);
                b.append(" ");
            }
            if (length() != 0) b.append(data[length() - 1]);
            b.append(")");
        }
        return b.toString();
    }

    @Override
    public String displayValue() {
        if (elementType == Symbol.CHAR) {
            return getStrValue();
        } else {
            StringBuilder b = new StringBuilder();
            b.append("#(");
            for (int i = 0; i <  length() - 1; i++) {
                b.append(data[i].displayValue());
                b.append(" ");
            }
            if (length() != 0) b.append(data[length() - 1]);
            b.append(")");
            return b.toString();
        }
    }

    public String getStrValue() {
        if (elementType != Symbol.CHAR) throw new LispException(Symbol.GENERALCONDITION, "vector is not a string");
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < length(); i++) {
            b.append(data[i].asChar().getValue());
        }
        return b.toString();
    }

    @Override
    public Vector asVector() { return this; }

    @Override
    public boolean isVector() { return true; }

    @Override
    public boolean isStr() { return elementType == Symbol.CHAR; }

    @Override
    public Vector asStr() { if (isStr()) return this; else throw new LispException(Symbol.TYPECONDITION, "obj " + this + " is not a str"); }

    @Override
    public Obj types() {
        if (elementType == Symbol.CHAR) {
            return new Cons(Symbol.STRING, new Cons(Symbol.VECTOR, super.types()));
        } else if (elementType == Symbol.T && fillpointer == -1) {
            return new Cons(Symbol.SIMPLEVECTOR, new Cons(Symbol.VECTOR, super.types()));
        } else {
            return new Cons(Symbol.VECTOR, super.types());
        }
    }

}
