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

public class MultiDimArray extends Array {

    private int[] dimensions;
    private Obj[] data;
    private int[] offsets;

    public MultiDimArray(Symbol elementType, Obj initialElement, int... dimensions) {
        super(elementType);
        this.dimensions = dimensions;
        this.offsets = new int[dimensions.length];
        int offset = 0;
        int size = 1;
        for (int i = dimensions.length - 1; i >= 0; i--) {
            offsets[i] = offset;
            offset += dimensions[i];
            size *= dimensions[i];
        }
        data = new Obj[size];
        for (int i = 0; i < size; i++) data[i] = initialElement;
    }

    @Override
    public int dimensions() {
        return dimensions.length;
    }

    @Override
    public int length(int dimension) {
        return dimensions[dimension];
    }

    private int index(int... coord) {
        int idx = 0;
        for (int i = 0; i < coord.length; i++) {
            idx += coord[i] * offsets[i];
        }
        return idx;
    }

    @Override
    public Obj get(int... coord) {
        return data[index(coord)];
    }

    @Override
    public void set(Obj val, int... coord)  {
        data[index(coord)] = val;
    }

    @Override
    public Obj types() {
        return new Cons(Symbol.ARRAY, super.types());
    }

    @Override
    public IntNum sxhash() {
        return IntNum.ZERO;
    }

    private void subarray(StringBuilder b, int index, int level, boolean displayable) {
        if (dimensions.length == 0) {
            b.append(data[0]);
        } else if (level == dimensions.length - 1) {
            b.append('(');
            for (int i = 0; i < dimensions[level] - 1; i++) {
                if (displayable) {
                    b.append(data[index + i].displayValue());
                } else {
                    b.append(data[index + i]);
                }
                b.append(' ');
            }
            b.append(data[dimensions[level] - 1]);
            b.append(')');
        } else {
            b.append('(');
            for (int i = 0; i < dimensions[level] - 1; i++) {
                subarray(b, index + offsets[level] * i, level + 1, displayable);
                b.append(' ');
            }
            subarray(b, index + offsets[level] * (dimensions[level] - 1), level + 1, displayable);
            b.append(')');
        }
    }

    @Override
    public String toString() {
        StringBuilder b = new StringBuilder();
        b.append("#");
        b.append(dimensions());
        b.append("A");
        subarray(b, 0, 0, false);
        return b.toString();
    }

    @Override
    public String displayValue() {
        StringBuilder b = new StringBuilder();
        b.append("#");
        b.append(dimensions());
        b.append("A");
        subarray(b, 0, 0, true);
        b.append("");
        return b.toString();
    }

}
