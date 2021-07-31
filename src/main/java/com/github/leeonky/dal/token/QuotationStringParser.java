package com.github.leeonky.dal.token;

abstract class QuotationStringParser extends TokenParser {

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
        return c == finishedChar;
    }

    @Override
    protected boolean trimFirstChar() {
        return true;
    }
}

class SingleQuotationStringParser extends QuotationStringParser {

    SingleQuotationStringParser() {
        super('\'');
    }

    @Override
    protected String escape(char c) {
        switch (c) {
            case '\'':
            case '\\':
                return String.valueOf(c);
            default:
                return "\\" + c;
        }
    }
}

class DoubleQuotationStringParser extends QuotationStringParser {

    protected DoubleQuotationStringParser() {
        super('"');
    }

    @Override
    protected String escape(char c) {
        switch (c) {
            case '\\':
            case '"':
                return String.valueOf(c);
            case 't':
                return "\t";
            case 'n':
                return "\n";
            default:
                return "\\" + c;
        }
    }
}

class RegexParser extends QuotationStringParser {

    protected RegexParser() {
        super('/');
    }

    @Override
    protected String escape(char c) {
        switch (c) {
            case '\\':
            case '/':
                return String.valueOf(c);
            default:
                return "\\" + c;
        }
    }
}
