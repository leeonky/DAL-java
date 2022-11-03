package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.CheckingContext;
import com.github.leeonky.dal.runtime.ConditionalChecker;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.Order;

import java.util.Optional;

import static com.github.leeonky.dal.runtime.Order.BUILD_IN;

@Order(BUILD_IN)
public class Checkers implements Extension {

    @Override
    public void extend(DAL dal) {
        dal.getRuntimeContextBuilder().registerMatchesChecker((expected, actual) -> {
            if (expected.getInstance() instanceof CharSequence
                    && (actual.getInstance() instanceof Number || actual.getInstance() instanceof Boolean))
                return Optional.of(new ConditionalChecker() {
                    @Override
                    public boolean failed(CheckingContext checkingContext) {
                        return true;
                    }

                    @Override
                    public String message(CheckingContext checkingContext) {
                        return checkingContext.cannotCompare();
                    }
                });
            if (expected.getInstance() instanceof Number && actual.getInstance() instanceof String)
                return Optional.of(new ConditionalChecker() {
                    @Override
                    public boolean failed(CheckingContext checkingContext) {
                        return true;
                    }

                    @Override
                    public String message(CheckingContext checkingContext) {
                        return checkingContext.cannotCompare();
                    }
                });
            if (expected.getInstance() instanceof Boolean && actual.getInstance() instanceof String)
                return Optional.of(new ConditionalChecker() {
                    @Override
                    public boolean failed(CheckingContext checkingContext) {
                        return true;
                    }

                    @Override
                    public String message(CheckingContext checkingContext) {
                        return checkingContext.cannotCompare();
                    }
                });
            return Optional.empty();
        });
    }
}
