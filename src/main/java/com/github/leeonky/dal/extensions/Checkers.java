package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.*;

import static com.github.leeonky.dal.runtime.Order.BUILD_IN;
import static java.util.Optional.of;

@Order(BUILD_IN)
public class Checkers implements Extension {
    private static final CheckerFactory ILLEGAL_TYPE_TO_CONVERT_MATCH = (d1, d2) -> of(new Checker() {
        @Override
        public boolean failed(CheckingContext checkingContext) {
            return true;
        }

        @Override
        public String message(CheckingContext checkingContext) {
            return checkingContext.cannotCompare();
        }
    });

    @Override
    public void extend(DAL dal) {
        dal.getRuntimeContextBuilder().checkerSetForMatching()
                .register(CharSequence.class, Number.class, ILLEGAL_TYPE_TO_CONVERT_MATCH)
                .register(CharSequence.class, Boolean.class, ILLEGAL_TYPE_TO_CONVERT_MATCH)
                .register(Number.class, CharSequence.class, ILLEGAL_TYPE_TO_CONVERT_MATCH)
                .register(Boolean.class, CharSequence.class, ILLEGAL_TYPE_TO_CONVERT_MATCH);
    }
}
