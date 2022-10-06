package com.github.leeonky.dal.runtime;

public abstract class Checker {
    public abstract boolean failed(ExpectActual expectActual);

    public abstract String message(ExpectActual expectActual);

    public void verify(ExpectActual expectActual, int position) {
        if (failed(expectActual))
            throw new AssertionFailure(message(expectActual), position);
    }

    public static class EqualsChecker extends Checker {
        @Override
        public boolean failed(ExpectActual expectActual) {
            return expectActual.objectNotEquals();
        }

        @Override
        public String message(ExpectActual expectActual) {
            return expectActual.shouldEqualTo();
        }
    }
}
