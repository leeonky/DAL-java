package com.github.leeonky.dal.token;

public interface TokenFactory {
    Token fetchToken(SourceCode sourceCode);
}

class SingleQuotationStringTokenFactory extends QuotationTokenFactory {

    public SingleQuotationStringTokenFactory() {
        super('\'', "string should end with `'`");
    }

    @Override
    protected TextParser createParser() {
        return new SingleQuotationStringParser();
    }
}

class DoubleQuotationStringTokenFactory extends QuotationTokenFactory {
    public DoubleQuotationStringTokenFactory() {
        super('"', "string should end with `\"`");
    }

    @Override
    protected TextParser createParser() {
        return new DoubleQuotationStringParser();
    }
}
