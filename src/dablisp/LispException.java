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

import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.LinkedList;

public class LispException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public Obj tag;
    public Obj value;
    public LinkedList<Obj> backtrace = new LinkedList<Obj>();

    public LispException(Obj tag, Obj value) {
        super(value.displayValue());
        this.tag = tag;
        this.value = value;
    }

    public LispException(Obj tag, String value) {
        super(value);
        this.tag = tag;
        this.value = new Vector(value);
    }

    public LispException(Obj tag, Obj value, Throwable cause) {
        super(value.displayValue(), cause);
        this.tag = tag;
        this.value = value;
    }

    public LispException(Obj tag, String value, Throwable cause) {
        super(value, cause);
        this.tag = tag;
        this.value = new Vector(value);
    }

    public void addBacktrace(Obj form) {
        backtrace.addLast(form);
    }

    public void printBacktrace(PrintStream strm) {
        for (Obj obj : backtrace) {
            strm.println();
            strm.print(obj);
        }
    }

    public void printBacktrace(PrintWriter strm) {
        for (Obj obj : backtrace) {
            strm.println();
            strm.print(obj);
        }
    }

    @Override
    public String toString() {
        return tag + ": " + value;
    }

}
