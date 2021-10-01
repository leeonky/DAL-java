package com.github.leeonky.dal.token;

import static com.github.leeonky.dal.util.IfThenFactory.when;

@Deprecated
public class SourceCode {
    private final char[] charBuffer;
    private final String sourceCode;
    private int offset = 0;

    public SourceCode(String sourceCode) {
        this.sourceCode = sourceCode;
        charBuffer = sourceCode.toCharArray();
    }

    public String getSourceCode() {
        return sourceCode;
    }

    public int getPosition() {
        return offset;
    }

    public SourceCode skipBlank() {
        while (offset < charBuffer.length && Character.isWhitespace(currentChar()))
            offset++;
        return this;
    }

    public char currentChar() {
        return getChar(offset);
    }

    private char getChar(int offset) {
        if (offset >= charBuffer.length)
            throw new NoMoreSourceCodeException(getPosition());
        return charBuffer[offset];
    }

    public char takeCurrentChar() {
        return getChar(offset++);
    }

    public boolean notEnd() {
        return offset < charBuffer.length;
    }

    public void seek(int p) {
        offset += p;
    }

    public boolean skip(String prefix) {
        return when(startsWith(prefix)).then(() -> seek(prefix.length()));
    }

    public boolean startsWith(String prefix) {
        return sourceCode.startsWith(prefix, offset);
    }
}
