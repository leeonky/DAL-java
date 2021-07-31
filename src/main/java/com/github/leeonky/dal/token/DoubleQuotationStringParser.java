package com.github.leeonky.dal.token;

public class DoubleQuotationStringParser extends QuotationStringParser {

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
