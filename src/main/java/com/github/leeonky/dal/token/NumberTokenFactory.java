package com.github.leeonky.dal.token;

import java.math.BigDecimal;

public class NumberTokenFactory extends NumberPropertyTokenFactory {

    @Override
    protected Token createToken(String value) {
        return Token.constValueToken(getNumber(value));
    }

    @Override
    protected TokenParser createParser() {
        return new NumberParser();
    }

    private Number getNumber(String value) {
        try {
            return BigDecimal.valueOf(Long.decode(value));
        } catch (NumberFormatException ignore) {
            return new BigDecimal(value);
        }
    }
}
