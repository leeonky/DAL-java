package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.Order;
import com.github.leeonky.dal.runtime.checker.CheckerFactory;
import com.github.leeonky.dal.runtime.checker.CheckingContext;

import static com.github.leeonky.dal.runtime.Order.BUILD_IN;
import static com.github.leeonky.dal.runtime.checker.Checker.forceFailed;
import static java.util.Optional.of;

@Order(BUILD_IN)
public class Checkers implements Extension {
    private static final CheckerFactory FALSE_FAILED = (d1, d2) -> of(forceFailed(CheckingContext::cannotCompare));

    @Override
    public void extend(DAL dal) {
        dal.getRuntimeContextBuilder().checkerSetForMatching()
                .register(CharSequence.class, Number.class, FALSE_FAILED)
                .register(CharSequence.class, Boolean.class, FALSE_FAILED)
                .register(Number.class, CharSequence.class, FALSE_FAILED)
                .register(Boolean.class, CharSequence.class, FALSE_FAILED);
    }
}
