package com.github.leeonky.dal.token;

abstract class EscapeFlag {
    private final char triggerChar;
    boolean isEscape = false;

    public EscapeFlag(char triggerChar) {
        this.triggerChar = triggerChar;
    }

    boolean consume(int c) {
        if (isEscape) {
            triggerAction(c);
            isEscape = false;
        } else {
            if (c == triggerChar)
                return isEscape = true;
            defaultAction(c);
        }
        return isEscape;
    }

    protected abstract void defaultAction(int c);

    protected abstract void triggerAction(int c);
}