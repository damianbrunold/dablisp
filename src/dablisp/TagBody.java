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

public class TagBody extends Handler {

    public int stackpointer = -1;
    private Map<String, Integer> tags = new HashMap<String, Integer>();

    public int getTarget(Obj tag) {
        if (!tags.containsKey(tag.displayValue())) return -1;
        return tags.get(tag.displayValue());
    }

    public void addTarget(Obj tag, int targetLabel) {
        tags.put(tag.displayValue(), targetLabel);
    }

    @Override
    public boolean isTagBody() { return true; }

    @Override
    public TagBody asTagBody() { return this; }

    @Override
    public String toString() {
        StringBuilder b = new StringBuilder();
        b.append("#<tagbody");
        for (String s : tags.keySet()) {
            b.append(" ");
            b.append(s).append("=").append(tags.get(s));
        }
        b.append(">");
        return b.toString();
    }

    public void resolveAddresses(int[] labels) {
        for (String s : tags.keySet()) {
            tags.put(s, labels[tags.get(s)]);
        }
    }

    public void decAddressesAbove(int i) {
        for (String s : tags.keySet()) {
            int ref = tags.get(s);
            if (ref > i) tags.put(s, ref - 1);
        }
    }

    public boolean references(int i) {
        for (String s : tags.keySet()) {
            int ref = tags.get(s);
            if (ref == i) return true;
        }
        return false;
    }

}
