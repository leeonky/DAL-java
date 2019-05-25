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

    @Override
    public String toString() {
        return sourceCode.substring(offset);
    }

    public boolean startsWith(String prefix) {
        return sourceCode.startsWith(prefix, offset);
    }

    public char charAt(int position) {
        return sourceCode.charAt(offset + position);
    }

    public SourceCode trimLeft() {
        while (offset < charBuffer.length && Character.isWhitespace(getChar()))
            offset++;
        return this;
    }

    public char getChar() {
        return charBuffer[offset];
    }

    public char takeChar() {
        return charBuffer[offset++];
    }

    public SourceCode substring(int begin) {
        offset += begin;
        return this;
    }

    public boolean hasContent() {
        trimLeft();
        return offset < charBuffer.length;
    }
}
