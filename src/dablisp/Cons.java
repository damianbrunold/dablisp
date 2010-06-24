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

import java.util.ArrayList;
import java.util.List;

public class Cons extends Obj {

    private Obj car;
    private Obj cdr;

    public Cons(Obj car, Obj cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    public Obj getCar() {
        return car;
    }

    public Obj getCdr() {
        return cdr;
    }

    public void setCar(Obj car) {
        this.car = car;
    }

    public void setCdr(Obj cdr) {
        this.cdr = cdr;
    }

    @Override
    public String displayValue() {
        StringBuilder builder = new StringBuilder();
        if (isQuotedForm(Symbol.QUOTE)) {
            builder.append('\'');
            builder.append(cdr.asCons().getCar().displayValue());
        } else if (isQuotedForm(Symbol.BACKQUOTE)) {
            builder.append('`');
            builder.append(cdr.asCons().getCar().displayValue());
        } else if (isQuotedForm(Symbol.COMMA)) {
            builder.append(',');
            builder.append(cdr.asCons().getCar().displayValue());
        } else if (isQuotedForm(Symbol.COMMAATSIGN)) {
            builder.append(",@");
            builder.append(cdr.asCons().getCar().displayValue());
        } else if (isQuotedForm(Symbol.COMMADOT)) {
            builder.append(",.");
            builder.append(cdr.asCons().getCar().displayValue());
        } else if (isQuotedForm(Symbol.FUNCTION)) {
            builder.append("#'");
            builder.append(cdr.asCons().getCar().displayValue());
        } else {
            strDisplay(builder, false);
        }
        return builder.toString();
    }

    private boolean isQuotedForm(Symbol quote) {
        return car == quote && cdr.isCons() && cdr.asCons().getCdr() == Symbol.NIL;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        if (isQuotedForm(Symbol.QUOTE)) {
            builder.append('\'');
            builder.append(cdr.asCons().getCar());
        } else if (isQuotedForm(Symbol.BACKQUOTE)) {
            builder.append('`');
            builder.append(cdr.asCons().getCar());
        } else if (isQuotedForm(Symbol.COMMA)) {
            builder.append(',');
            builder.append(cdr.asCons().getCar());
        } else if (isQuotedForm(Symbol.COMMAATSIGN)) {
            builder.append(",@");
            builder.append(cdr.asCons().getCar());
        } else if (isQuotedForm(Symbol.COMMADOT)) {
            builder.append(",.");
            builder.append(cdr.asCons().getCar());
        } else if (isQuotedForm(Symbol.FUNCTION)) {
            builder.append("#'");
            builder.append(cdr.asCons().getCar());
        } else {
            str(builder, false);
        }
        return builder.toString();
    }

    private void str(StringBuilder builder, boolean suppressParen) {
        if (!suppressParen) {
            builder.append("(");
        }
        builder.append(car);
        if (cdr == Symbol.NIL) {
            builder.append(")");
        } else if (cdr.isCons()) {
            builder.append(" ");
            cdr.asCons().str(builder, true);
        } else {
            builder.append(" . ");
            builder.append(cdr);
            builder.append(")");
        }
    }

    private void strDisplay(StringBuilder builder, boolean suppressParen) {
        if (!suppressParen) {
            builder.append("(");
        }
        builder.append(car);
        if (cdr == Symbol.NIL) {
            builder.append(")");
        } else if (cdr.isCons()) {
            builder.append(" ");
            cdr.asCons().strDisplay(builder, true);
        } else {
            builder.append(" . ");
            builder.append(cdr.displayValue());
            builder.append(")");
        }
    }

    @Override
    public boolean isAtom() { return false; }

    @Override
    public boolean isCons() { return true; }

    @Override
    public Cons asCons() { return this; }

    public boolean hasNth(int n) {
        Obj c = this;
        while (n > 0) {
            if (c.rest() == Symbol.NIL) return false;
            c = c.rest();
            n--;
        }
        return true;
    }

    public Obj nth(int n) {
        Obj c = this;
        while (n > 0) {
            c = c.rest();
            n--;
        }
        return c.first();
    }

    public Cons nthCons(int n) {
        Cons c = this;
        while (n > 0) {
            c = c.rest().asCons();
            n--;
        }
        return c;
    }

    @Override
    public Obj first() { return car; }
    @Override
    public Obj rest() { return cdr; }

    @Override
    public Obj second() { return nth(1); }
    @Override
    public Obj third() { return nth(2); }
    @Override
    public Obj fourth() { return nth(3); }
    public Obj fifth() { return nth(4); }
    public Obj sixth() { return nth(5); }
    public Obj seventh() { return nth(6); }
    public Obj eighth() { return nth(7); }
    public Obj ninth() { return nth(8); }
    public Obj tenth() { return nth(9); }

    @Override
    public int length() {
        int len = 0;
        Obj o = this;
        while (o != Symbol.NIL) {
            len++;
            o = o.rest();
        }
        return len;
    }

    @Override
    public Obj copy() {
        return new Cons(car.copy(), cdr.copy());
    }

    @Override
    public Obj types() { return new Cons(Symbol.CONS, new Cons(Symbol.LIST, super.types())); }

    public List<Obj> asJavaList() {
        List<Obj> list = new ArrayList<Obj>();
        Obj o = this;
        while (o != Symbol.NIL) {
            list.add(o.first());
            o = o.rest();
        }
        return list;
    }

    public static Obj fromJavaList(List<Obj> list) {
        if (list.isEmpty()) return Symbol.NIL;
        Obj r = new Cons(list.get(0), Symbol.NIL);
        Obj current = r;
        for (int i = 1; i < list.size(); i++) {
            Cons next = new Cons(list.get(i), Symbol.NIL);
            current.asCons().setCdr(next);
            current = next;
        }
        return r;
    }

    public Vector asString() {
        return asVector(Symbol.CHAR);
    }

    public Vector asVector(Symbol elementType) {
        Vector v = new Vector(elementType, Symbol.NIL, length());
        Obj o = this;
        int idx = 0;
        while (o != Symbol.NIL) {
            v.set(o.first(), idx++);
            o = o.rest();
        }
        return v;
    }

    public static Obj fromVector(Vector s) {
        if (s.length() == 0) return Symbol.NIL;
        Obj r = new Cons(s.get(0), Symbol.NIL);
        Obj current = r;
        for (int i = 1; i < s.length(); i++) {
            Cons next = new Cons(s.get(i), Symbol.NIL);
            current.asCons().setCdr(next);
            current = next;
        }
        return r;
    }

    @Override
    public IntNum sxhash() {
        int result = 0;
        if (car != null) result ^= car.hashCode();
        if (cdr != null) result ^= cdr.hashCode();
        return IntNum.fromLong(result);
    }

}
