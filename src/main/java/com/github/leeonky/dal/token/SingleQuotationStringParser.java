package com.github.leeonky.dal.token;

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
