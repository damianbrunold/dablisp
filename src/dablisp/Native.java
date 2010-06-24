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

public class Native extends Atom {

    private Object nativeObject;

    public Native(Object nativeObject) {
        this.nativeObject = nativeObject;
    }

    public Object getNativeObject() {
        return nativeObject;
    }

    @Override
    public String toString() {
    	if (nativeObject == null) return "#<native null>";
    	String desc = nativeObject.getClass().getName() + '@' + Integer.toHexString(nativeObject.hashCode());
    	return "#<native " + desc + ">";
    }

    @Override
    public String displayValue() {
        return toString();
    }

    @Override
    public boolean isNative() { return true; }

    @Override
    public Native asNative() { return this; }

    @Override
    public Obj types() {
        if (nativeObject != null) {
            return new Cons(Symbol.intern(nativeObject.getClass().getName()),
                    new Cons(Symbol.NATIVE,
                     super.types()));
        } else {
            return new Cons(Symbol.NATIVE, super.types());
        }
    }

    @Override
    public IntNum sxhash() { return nativeObject != null ? IntNum.fromLong(nativeObject.hashCode()) : IntNum.ZERO; }

}
