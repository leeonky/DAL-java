package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntexException;

public class ConstIndexTokenCandidate extends TokenCandidate {
    public ConstIndexTokenCandidate(char c, int position) {
        super(c, position);
    }

    public static boolean isBegin(char c) {
        return c == '[';
    }

    @Override
    public Token toToken() {
        int value;
        try {
            value = Integer.valueOf(content());
        } catch (NumberFormatException e) {
            throw new SyntexException(getPosition() + 1, "only support const int array index");
        }
        return Token.constIndexToken(value);
    }

    @Override
    public boolean isDiscardedLastChar(char c) {
        return c == ']';
    }

    @Override
    public boolean isDiscardFirstChar() {
        return true;
    }
}
