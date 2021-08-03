package com.github.leeonky.dal.token;

abstract class FreeTextParser extends TokenParser {
    @Override
    protected String escape(char c) {
        throw new IllegalStateException();
    }

    @Override
    protected boolean isEscapeChar(char c) {
        return false;
    }

    @Override
    protected boolean trimFirstChar() {
        return false;
    }
}

class PropertyIndexParser extends FreeTextParser {

    @Override
    protected boolean isFinishedChar(char c) {
        return c == ']';
    }
}
