package com.github.leeonky.dal.token;

abstract class OneCharTokenFactory implements TokenFactory {

    private final char target;

    public OneCharTokenFactory(char target) {
        this.target = target;
    }

    @Override
    public Token fetchToken(SourceCode sourceCode, Token previous) {
        if (sourceCode.notEnd() && sourceCode.currentChar() == target) {
            sourceCode.takeCurrentChar();
            return createToken();
        }
        return null;
    }

    protected abstract Token createToken();
}

class BeginBracketTokenFactory extends OneCharTokenFactory {

    public BeginBracketTokenFactory() {
        super('(');
    }

    @Override
    protected Token createToken() {
        return Token.beginBracketToken();
    }
}

class EndBracketTokenFactory extends OneCharTokenFactory {

    public EndBracketTokenFactory() {
        super(')');
    }

    @Override
    protected Token createToken() {
        return Token.endBracketToken();
    }
}
