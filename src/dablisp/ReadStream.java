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

import java.io.*;

public class ReadStream {

    PushbackReader reader;

    public ReadStream(InputStream strm) {
        this(new PushbackReader(new InputStreamReader(strm)));
    }

    public ReadStream(String data) {
        this(new PushbackReader(new StringReader(data)));
    }

    public ReadStream(PushbackReader reader) {
        this.reader = reader;
    }

    public int read() throws IOException {
        return reader.read();
    }

    private boolean eol(int c) throws IOException {
        if (c == '\n') return true;
        if (c == '\r') {
            int c2 = read();
            if (c2 == '\n') return true;
            if (c2 != -1) unread((char) c2);
        }
        return false;
    }

    public String readLine() throws IOException {
        int c = read();
        if (c == -1) return null;
        StringBuilder b = new StringBuilder();
        while (c != -1 && !eol(c)) {
            b.append((char) c);
            c = read();
        }
        return b.toString();
    }

    public void unread(char ch) throws IOException {
        reader.unread(ch);
    }

    public void close() throws IOException {
        reader.close();
    }

}
