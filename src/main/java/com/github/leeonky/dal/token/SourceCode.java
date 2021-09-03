package com.github.leeonky.dal.token;

import static com.github.leeonky.dal.util.IfThenFactory.when;

//TODO clean method
public class SourceCode {
    private final char[] charBuffer;
    private final String sourceCode;
    private int offset = 0;

    public SourceCode(String sourceCode) {
        this.sourceCode = sourceCode;
        charBuffer = sourceCode.toCharArray();
    }

    public int getPosition() {
        return offset;
    }

    public boolean startsWith(String prefix) {
        return sourceCode.startsWith(prefix, offset);
    }

    public SourceCode trimLeft() {
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

    public boolean hasContent() {
        return trimLeft().notEnd();
    }

    public boolean notEnd() {
        return offset < charBuffer.length;
    }

    public void seek(int p) {
        offset += p;
    }

    public boolean startsWithThenSeek(String prefix) {
        return when(startsWith(prefix)).then(() -> seek(prefix.length()));
    }
}
