package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;

public abstract class QuotationTokenFactory implements TokenFactory {

    private final String errorMessage;
    private final char startChar;

    public QuotationTokenFactory(char startChar, String errorMessage) {
        this.startChar = startChar;
        this.errorMessage = errorMessage;
    }

    @Override
    public Token fetchToken(SourceCode sourceCode, Token previous) {
        if (sourceCode.notEnd() && sourceCode.currentChar() == startChar)
            return parseConstValueToken(sourceCode);
        return null;
    }

    private Token parseConstValueToken(SourceCode sourceCode) {
        int startPosition = sourceCode.getPosition();
        TokenParser parser = createParser();
        int codeLength = 0;
        while (sourceCode.notEnd() && parser.feed(sourceCode.takeCurrentChar()))
            codeLength++;
        if (!parser.isFinished())
            throw new SyntaxException(startPosition + codeLength, errorMessage);
        return createToken(parser.value());
    }

    protected abstract Token createToken(String value);

    protected abstract TokenParser createParser();
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
