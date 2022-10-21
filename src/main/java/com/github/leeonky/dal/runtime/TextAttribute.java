package com.github.leeonky.dal.runtime;

public class TextAttribute {
    private final String newLine, endOfLine, continuation;

    TextAttribute(String newLine, String endOfLine, String continuation) {
        this.newLine = newLine;
        this.endOfLine = endOfLine;
        this.continuation = continuation;
    }

    public String newLine() {
        return newLine;
    }

    public TextAttribute newLine(String newLine) {
        return new TextAttribute(newLine, endOfLine, continuation);
    }

    public String endOfLine() {
        return endOfLine;
    }

    public TextAttribute endOfLine(String endOfLine) {
        return new TextAttribute(newLine, endOfLine, continuation);
    }

    public TextAttribute continuation(String continuation) {
        return new TextAttribute(newLine, endOfLine, continuation);
    }

    public String continuation() {
        return continuation;
    }
}
