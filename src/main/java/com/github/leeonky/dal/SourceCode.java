package com.github.leeonky.dal;

import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenCandidate;

public class SourceCode {
    private int offset = 0;
    private char[] charBuffer;
    private String sourceCode;

    public SourceCode(String sourceCode) {
        this.sourceCode = sourceCode;
        charBuffer = sourceCode.toCharArray();
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
        while (offset < charBuffer.length && Character.isWhitespace(charBuffer[offset]))
            offset++;
        return this;
    }

    public SourceCode substring(int begin) {
        offset += begin;
        return this;
    }

    public boolean isEnd() {
        trimLeft();
        return offset == charBuffer.length;
    }

    public Token getToken() {
        if (trimLeft().isEnd())
            throw new IllegalStateException("No more token");
        TokenCandidate tokenCandidate = TokenCandidate.createTokenCandidate(charBuffer[offset], offset++);
        while (!isEnd() && !tokenCandidate.isExcludedSplitChar(charBuffer[offset])) {
            char c = charBuffer[offset++];
            if (tokenCandidate.isDiscardedLastChar(c))
                break;
            tokenCandidate.append(c);
            if (tokenCandidate.isIncludedLastChar(c))
                break;
        }
        return tokenCandidate.toToken();
    }

}
