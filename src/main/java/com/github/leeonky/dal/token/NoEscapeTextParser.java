package com.github.leeonky.dal.token;

abstract class NoEscapeTextParser extends TokenParser {
    @Override
    protected String escape(char c) {
        throw new IllegalStateException();
    }

    @Override
    protected boolean isEscapeChar(char c) {
        return false;
    }
}

class NumberParser extends NoEscapeTextParser {
    private boolean canFinish = false;

    @Override
    protected boolean trimFirstChar() {
        return false;
    }

    @Override
    protected boolean isFinishedChar(char c) {
        canFinish = true;
        //canFinish = (canFinish || c is contain) TODO
        return Character.isWhitespace(c) || Scanner.CHAR_SPLIT.contains(c);
    }

    @Override
    public boolean canFinish() {
        return canFinish;
    }
}
