package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;

public class SingleQuotationStringTokenFactory implements TokenFactory {

    @Override
    public Token fetchToken(SourceCode sourceCode) {
        if (sourceCode.notEnd() && sourceCode.getChar() == '\'')
            return parseConstValueToken(sourceCode);
        return null;
    }

    private Token parseConstValueToken(SourceCode sourceCode) {
        int startPosition = sourceCode.getPosition();
        SingleQuotationStringParser parser = new SingleQuotationStringParser();
        int codeLength = 0;
        while (sourceCode.notEnd() && parser.feed(sourceCode.takeChar()))
            codeLength++;
        if (!parser.isFinished())
            throw new SyntaxException(startPosition + codeLength, "string should end with `'`");
        return Token.constValueToken(parser.value());
    }
}
