package com.github.leeonky.dal.token;

public abstract class QuotationStringParser extends TextParser {

    private final char finishedChar;
    private int startEndCount = 0;

    protected QuotationStringParser(char finishedChar) {
        this.finishedChar = finishedChar;
    }

    @Override
    protected boolean isEscapeChar(char c) {
        return c == '\\';
    }

    @Override
    protected boolean isFinishedChar(char c) {
        return c == finishedChar && ++startEndCount > 1;
    }

    @Override
    protected String getContent(StringBuffer stringBuffer) {
        return stringBuffer.subSequence(1, stringBuffer.length() - 1).toString();
    }
}
