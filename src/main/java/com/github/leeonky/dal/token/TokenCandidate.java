package com.github.leeonky.dal.token;

import java.util.HashSet;
import java.util.Set;

public abstract class TokenCandidate {
    private final int startPosition;
    private final StringBuilder stringBuilder = new StringBuilder();
    private final Set<Character> split;
    private boolean finished = false;

    public TokenCandidate(SourceCode sourceCode) {
        this(sourceCode, new HashSet<>());
    }

    public TokenCandidate(SourceCode sourceCode, Set<Character> split) {
        this.split = split;
        startPosition = sourceCode.getPosition();
        char c = sourceCode.takeCurrentChar();
        if (!needDiscardBeginChar())
            stringBuilder.append(c);
    }

    protected boolean append(int c) {
        stringBuilder.append((char) c);
        return false;
    }

    protected String content() {
        return stringBuilder.toString();
    }

    protected abstract Token toToken();

    protected boolean needDiscardBeginChar() {
        return false;
    }

    protected int getStartPosition() {
        return startPosition;
    }

    protected boolean isUnexpectedChar(char c) {
        return Character.isWhitespace(c) || c == '[' || split.contains(c);
    }

    public Token fetchToken(SourceCode sourceCode) {
        while (sourceCode.notEnd() && needIgnoreBegin(sourceCode.currentChar()))
            sourceCode.takeCurrentChar();
        while (sourceCode.notEnd() && !isDiscardedSuffix(sourceCode) && !isUnexpectedChar(sourceCode.currentChar()))
            takeEscapedChar(sourceCode);
        return toToken();
    }

    protected boolean needIgnoreBegin(char c) {
        return false;
    }

    private void takeEscapedChar(SourceCode sourceCode) {
        while (append(sourceCode.takeCurrentChar()) && sourceCode.notEnd()) ;
    }

    protected String discardedSuffix() {
        return null;
    }

    protected boolean isFinished() {
        return finished;
    }

    private boolean isDiscardedSuffix(SourceCode sourceCode) {
        String suffix = discardedSuffix();
        if (suffix != null) {
            if (sourceCode.startsWith(suffix)) {
                sourceCode.seek(suffix.length());
                finished = true;
                return true;
            }
        }
        return false;
    }
}
