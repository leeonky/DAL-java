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

    @Override
    protected boolean trimFirstChar() {
        return false;
    }
}

//TODO extract super class
class NumberParser extends NoEscapeTextParser {
    private boolean canFinish = false;

    @Override
    protected boolean isFinishedChar(char c) {
        canFinish = true;
        //canFinish = (canFinish || c is contain) TODO
        return Scanner.TOKEN_SPLIT.contains(c);
    }

    @Override
    public boolean canFinish() {
        return canFinish;
    }
}

//TODO extract super class
class OperatorParser extends NoEscapeTextParser {
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

class PropertyIndexParser extends NoEscapeTextParser {

    @Override
    protected boolean isFinishedChar(char c) {
        return c == ']';
    }
}
