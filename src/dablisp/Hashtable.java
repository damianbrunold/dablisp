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

import java.util.HashMap;
import java.util.Map;

public class Hashtable extends Atom {

    private Env env;
    private Env fenv;
    private Fn test;
    private Map<Key, Obj> map = new HashMap<Key, Obj>();

    private class Key {
        public Obj key;
        public Key(Obj key) {
            this.key = key;
        }
        @Override
        public boolean equals(Object o) {
            if (o instanceof Key) {
                Stack stack = new Stack();
                stack.push(((Key) o).key);
                stack.push(key);
                test.applyfn(2, stack, env, fenv);
                return stack.pop() != Symbol.NIL;
            }
            return false;
        }
        @Override
        public int hashCode() {
            return (int) key.sxhash().intValue();
        }
    }

    public Hashtable(Fn test, Env env, Env fenv) {
        this.test = test;
        this.env = env;
        this.fenv = fenv;
    }

    public Obj gethash(Obj key, Obj def) {
        Key k = new Key(key);
        if (map.containsKey(k)) {
            return new Mvals(map.get(k), Symbol.T);
        } else {
            return new Mvals(def, Symbol.NIL);
        }
    }

    public void sethash(Obj key, Obj value) {
        map.put(new Key(key), value);
    }

    public void remhash(Obj key) {
        map.remove(new Key(key));
    }

    @Override
    public String toString() {
        return "#<hashtable>";
    }

    @Override
    public String displayValue() {
        return toString();
    }

    @Override
    public boolean isHashtable() { return true; }

    @Override
    public Hashtable asHashtable() { return this; }

    @Override
    public Obj types() { return new Cons(Symbol.HASHTABLE, super.types()); }

    @Override
    public IntNum sxhash() { return IntNum.ZERO; }

}
