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

//TODO extract super class
class OperatorParser extends FreeTextParser {
    private boolean canFinish = false;

    @Override
    protected boolean isFinishedChar(char c) {
        canFinish = true;
        return !Scanner.OPERATOR_CHAR.contains(c) || (isOnlyMatchesOperator() && c == '/');
    }

    private boolean isOnlyMatchesOperator() {
        return content.size() == 1 && content.get(0).equals('~');
    }

    @Override
    public boolean canFinish() {
        return canFinish;
    }
}

class PropertyIndexParser extends FreeTextParser {

    @Override
    protected boolean isFinishedChar(char c) {
        return c == ']';
    }
}
