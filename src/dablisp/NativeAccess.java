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

public class NativeAccess {

    public static Class<?>[] getTypes(Obj args) {
        List<Class<?>> result = new ArrayList<Class<?>>();
        while (args != Symbol.NIL) {
            Obj c = args.first();
            result.add(classForType(getValue(c), getType(c)));
            args = args.rest();
        }
        return result.toArray(new Class[0]);
    }

    public static Object[] getValues(Obj args) {
        List<Object> result = new ArrayList<Object>();
        while (args != Symbol.NIL) {
            Obj c = args.first();
            result.add(coerceToNative(getValue(c), getType(c)));
            args = args.rest();
        }
        return result.toArray();
    }

    public static Symbol getType(Obj c) {
        if (c.isCons()) {
            return c.first().asSymbol();
        } else {
            return getObjType(c);
        }
    }

    public static Obj getValue(Obj c) {
        if (c.isCons()) {
            return c.second();
        } else {
            return c;
        }
    }

    public static Symbol getObjType(Obj c) {
        Symbol type;
        if (c.isChar()) {
            type = Symbol.CHAR;
        } else if (c.isStr()) {
            type = Symbol.STRING;
        } else if (c.isIntNum()) {
            type = Symbol.INTEGER;
        } else if (c.isFloatNum()) {
            type = Symbol.DOUBLE;
        } else if (c == Symbol.T || c == Symbol.NIL) {
            type = Symbol.BOOLEAN;
        } else if (c.isNative()) {
            type = Symbol.NATIVE;
        } else {
            type = Symbol.OBJECT;
        }
        return type;
    }

    public static Class<?> classForType(Obj val, Symbol type) {
        if (type == Symbol.CHAR) {
            return Character.TYPE;
        } else if (type == Symbol.STRING) {
            return String.class;
        } else if (type == Symbol.INTEGER) {
            return Integer.TYPE;
        } else if (type == Symbol.SHORT) {
            return Short.TYPE;
        } else if (type == Symbol.LONG) {
            return Long.TYPE;
        } else if (type == Symbol.BYTE) {
            return Byte.TYPE;
        } else if (type == Symbol.DOUBLE) {
            return Double.TYPE;
        } else if (type == Symbol.FLOAT) {
            return Float.TYPE;
        } else if (type == Symbol.BOOLEAN) {
            return Boolean.TYPE;
        } else if (type == Symbol.NATIVE) {
            return val.asNative().getNativeObject().getClass();
        } else {
            return Object.class;
        }
    }

    public static Object coerceToNative(Obj obj, Symbol type) {
        if (type == Symbol.CHAR) {
            return obj.asChar().getValue();
        } else if (type == Symbol.STRING) {
            return obj.asStr().getStrValue();
        } else if (type == Symbol.INTEGER) {
            return obj.asIntNum().intValue();
        } else if (type == Symbol.SHORT) {
            return (short) obj.asIntNum().intValue();
        } else if (type == Symbol.LONG) {
            return (long) obj.asIntNum().intValue();
        } else if (type == Symbol.BYTE) {
            return (byte) obj.asIntNum().intValue();
        } else if (type == Symbol.DOUBLE) {
            return obj.asFloatNum().getValue();
        } else if (type == Symbol.FLOAT) {
            return (float) obj.asFloatNum().getValue();
        } else if (type == Symbol.BOOLEAN) {
            return obj == Symbol.T ? true : false;
        } else {
            return obj.asNative().getNativeObject();
        }
    }

    public static Obj coerceFromNative(Native obj) {
        Class<?> c = obj.getNativeObject().getClass();
        if (c == Character.class) {
            return new Char(((Character) obj.getNativeObject()).charValue());
        } else if (c == String.class) {
            return new Vector((String) obj.getNativeObject());
        } else if (c == Integer.class) {
            return IntNum.fromLong(((Integer) obj.getNativeObject()).intValue());
        } else if (c == Short.class) {
            return IntNum.fromLong(((Short) obj.getNativeObject()).shortValue());
        } else if (c == Long.class) {
            return IntNum.fromLong(((Long) obj.getNativeObject()).longValue());
        } else if (c == Byte.class) {
            return IntNum.fromLong(((Byte) obj.getNativeObject()).byteValue());
        } else if (c == Double.class) {
            return new FloatNum(((Double) obj.getNativeObject()).doubleValue());
        } else if (c == Float.class) {
            return new FloatNum(((Float) obj.getNativeObject()).floatValue());
        } else if (c == Boolean.class) {
            return ((Boolean) obj.getNativeObject()).booleanValue() ? Symbol.T : Symbol.NIL;
        } else {
            return obj.asNative();
        }
    }

}
