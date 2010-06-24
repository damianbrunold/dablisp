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

public class Stack {

    private static final int INITIAL_STACKSIZE = 1024;

    int pointer = 0;
    Obj[] data = new Obj[INITIAL_STACKSIZE];

    public int getPointer() {
        return pointer;
    }

    public void setPointer(int pointer) {
        for (int i = pointer; i < this.pointer; i++) {
            data[i] = null;
        }
        this.pointer = pointer;
    }

    public void clear() {
        setPointer(0);
    }

    public void push(Obj obj) {
        if (pointer == data.length) {
            grow();
        }
        data[pointer++] = obj;
    }

    public Obj pop() {
        --pointer;
        Obj result = data[pointer];
        data[pointer] = null;
        return result;
    }

    public int popInt() {
        return (int) pop().asIntNum().intValue();
    }

    public void pop(int n) {
        pointer -= n;
        for (int i = pointer; i < pointer + n; i++) {
            data[i] = null;
        }
    }

    public Obj popList(int n) {
        if (n == 0) return Symbol.NIL;
        Cons r = new Cons(Symbol.NIL, Symbol.NIL);
        Cons c = r;
        for (int i = 0; i < n; i++) {
            Cons d = new Cons(pop(), Symbol.NIL);
            c.setCdr(d);
            c = d;
        }
        return r.rest().asCons();
    }

    public void remove(int idx, int n) {
        for (int i = idx - 1; i >= 0; i--) {
            data[pointer - 1 - (i + n)] = data[pointer - 1 - i];
        }
        for (int i = 0; i < n; i++) {
            data[pointer - 1 - i] = null;
        }
        pointer -= n;
    }

    public Obj peek() {
        return data[pointer - 1];
    }

    public int peekInt() {
        return (int) peek().asIntNum().intValue();
    }

    public Obj peek(int offset) {
        return data[pointer - 1 - offset];
    }

    public Obj get(int pointer) {
        return data[pointer];
    }

    public void replace(Obj val) {
        data[pointer - 1] = val;
    }

    private void grow() {
        Obj[] newdata = new Obj[data.length * 2];
        System.arraycopy(data, 0, newdata, 0, data.length);
        data = newdata;
    }

    public int size() {
        return pointer;
    }

    @Override
    public String toString() {
        StringBuilder b = new StringBuilder();
        for (int i = pointer - 1; i >= 0; i--) {
            b.append(data[i]).append(" ");
        }
        if (pointer > 0) b.deleteCharAt(b.length() - 1);
        return b.toString();
    }
}


