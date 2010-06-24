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

public class SymbolTable {

    private static Map<String, Symbol> symbols = new HashMap<String, Symbol>();

    public static Symbol intern(String s) {
        if (!symbols.containsKey(s)) {
            symbols.put(s, new Symbol(s));
        }
        return symbols.get(s);
    }

}
