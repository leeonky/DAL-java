package com.github.leeonky.dal.token;

public class DoubleQuotationStringTokenFactory extends QuotationTokenFactory {
    public DoubleQuotationStringTokenFactory() {
        super('"', "string should end with `\"`");
    }

    @Override
    protected TextParser createParser() {
        return new DoubleQuotationStringParser();
    }
}
