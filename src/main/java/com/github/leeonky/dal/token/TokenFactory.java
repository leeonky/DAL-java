package com.github.leeonky.dal.token;

public interface TokenFactory {
    Token fetchToken(SourceCode sourceCode, Token previous);
}

class SingleQuotationStringTokenFactory extends QuotationTokenFactory {

    public SingleQuotationStringTokenFactory() {
        super('\'', "string should end with `'`");
    }

    @Override
    protected TokenParser createParser() {
        return new SingleQuotationStringParser();
    }

    @Override
    protected Token createToken(String value) {
        return Token.constValueToken(value);
    }
}

class DoubleQuotationStringTokenFactory extends QuotationTokenFactory {
    public DoubleQuotationStringTokenFactory() {
        super('"', "string should end with `\"`");
    }

    @Override
    protected TokenParser createParser() {
        return new DoubleQuotationStringParser();
    }

    @Override
    protected Token createToken(String value) {
        return Token.constValueToken(value);
    }
}

class RegexTokenFactory extends QuotationTokenFactory {

    public RegexTokenFactory() {
        super('/', "regex should end with `/`");
    }

    @Override
    protected TokenParser createParser() {
        return new RegexParser();
    }

    @Override
    protected Token createToken(String value) {
        return Token.regexToken(value);
    }

    @Override
    public Token fetchToken(SourceCode sourceCode, Token previous) {
        if (previous != null && previous.isOperatorMatches())
            return super.fetchToken(sourceCode, previous);
        return null;
    }
}
