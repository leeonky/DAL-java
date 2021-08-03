package com.github.leeonky.dal.token;

public class SourceCode {
    private int offset = 0;
    private char[] charBuffer;
    private String sourceCode;

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
            throw new NoMoreSourceCodeException();
        return charBuffer[offset];
    }

    public char takeCurrentChar() {
        return getChar(offset++);
    }

    public boolean hasContent() {
        trimLeft();
        return notEnd();
    }

    public boolean notEnd() {
        return offset < charBuffer.length;
    }

    public void seek(int p) {
        offset += p;
    }
}
