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
package dablisp.fn;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import dablisp.*;

public class FnLispImplementationVersion extends Fn {

    @Override
    public void apply(int argc, Stack stack, Env env, Env fenv) {
        checkArgCount(argc, 0);
        int year = 0;
        int week = 0;
        int build = 0;
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(getClass().getResourceAsStream("/lisp-version.txt")));
            try {
                String line = in.readLine();
                if (line != null) {
                    String[] p = line.trim().split("\\.");
                    if (p != null && p.length == 3) {
                        year = Integer.parseInt(p[0]);
                        week = Integer.parseInt(p[1]);
                        build = Integer.parseInt(p[2]);
                    }
                }
            } finally {
                in.close();
            }
        } catch (NullPointerException e) {
            // ignore
        } catch (IOException e) {
            // ignore
        }
        stack.push(new Cons(IntNum.fromLong(year),
                    new Cons(IntNum.fromLong(week),
                      new Cons(IntNum.fromLong(build),
                        Symbol.NIL))));
    }

    @Override
    public String getName() {
        return "lisp-implementation-version";
    }

    @Override
    public String getDocstring() {
        return "(" + getName() + ") - function -  returns the lisp implementation version as a list of three numbers (year week build). If no version can be determined the list (0 0 0) is returned.";
    }

}
