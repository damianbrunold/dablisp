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

public class Catcher extends Handler {

    public Obj tag;
    public int target;
    public int stackpointer;

    public Catcher(Obj tag, int target, int stackpointer) {
        this.tag = tag;
        this.target = target;
        this.stackpointer = stackpointer;
    }

    @Override
    public boolean isCatcher() { return true; }

    @Override
    public Catcher asCatcher() { return this; }

    @Override
    public String toString() {
        return "#<catcher " + tag + " target: " + target + " sp: " + stackpointer + ">";
    }

}
