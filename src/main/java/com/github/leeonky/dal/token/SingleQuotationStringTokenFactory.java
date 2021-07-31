package com.github.leeonky.dal.token;

public class SingleQuotationStringTokenFactory extends QuotationTokenFactory {

    public SingleQuotationStringTokenFactory() {
        super('\'', "string should end with `'`");
    }

    @Override
    protected TextParser createParser() {
        return new SingleQuotationStringParser();
    }
}
