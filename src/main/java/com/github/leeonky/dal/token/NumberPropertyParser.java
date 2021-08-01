package com.github.leeonky.dal.token;

abstract class NumberPropertyParser extends TokenParser {
    @Override
    protected String escape(char c) {
        throw new IllegalStateException();
    }

    @Override
    protected boolean isEscapeChar(char c) {
        return false;
    }
}

class NumberParser extends NumberPropertyParser {
    private boolean canFinish = false;

    @Override
    protected boolean trimFirstChar() {
        return false;
    }

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

class OperatorParser extends NumberPropertyParser {
    private boolean canFinish = false;
    private String operator = "";

    @Override
    protected boolean isFinishedChar(char c) {
        canFinish = true;
        return !Scanner.OPERATOR_CHAR.contains(c) || (operator.equals("~") && c == '/');
    }

    @Override
    protected boolean trimFirstChar() {
        return false;
    }

    @Override
    public boolean canFinish() {
        return canFinish;
    }

    @Override
    protected String content() {
        return operator;
    }

    @Override
    protected boolean isEmpty() {
        return operator.isEmpty();
    }

    @Override
    protected void append(char c) {
        operator += c;
    }
}
