package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.Optional;
import java.util.function.BiPredicate;
import java.util.function.Predicate;

import static java.util.Optional.of;

public interface Assertion {
    boolean verifyStructure(Verification verification, DALRuntimeContext context);

    boolean verifyContent(Verification verification, DALRuntimeContext context);

    default boolean verify(DALRuntimeContext runtimeContext, Verification verification) {
        return verification.structure() ? verifyStructure(verification, runtimeContext)
                : verifyContent(verification, runtimeContext);
    }

    static Assertion assertion(BiPredicate<Verification, DALRuntimeContext> verifyStructure,
                               BiPredicate<Verification, DALRuntimeContext> verifyContent) {
        return new Assertion() {
            @Override
            public boolean verifyStructure(Verification verification, DALRuntimeContext context) {
                return verifyStructure.test(verification, context);
            }

            @Override
            public boolean verifyContent(Verification verification, DALRuntimeContext context) {
                return verifyContent.test(verification, context);
            }
        };
    }

    interface IfFactory<CONDITION> {
        boolean matches(CONDITION condition);

        static <CONDITION> IfFactory<CONDITION> when(Predicate<CONDITION> predicate) {
            return predicate::test;
        }

        default <VALUE> OptionalFactory<CONDITION, VALUE> then(VALUE value) {
            return condition -> matches(condition) ? of(value) : Optional.empty();
        }
    }

    interface ElseIfFactory<CONDITION, VALUE> {
        boolean matches(CONDITION condition);

        Optional<VALUE> previousIf(CONDITION condition);

        default OptionalFactory<CONDITION, VALUE> then(VALUE value) {
            return condition -> {
                Optional<VALUE> previous = previousIf(condition);
                return previous.isPresent() ? previous : matches(condition) ? of(value) : Optional.empty();
            };
        }
    }

    interface OptionalFactory<CONDITION, VALUE> {
        Optional<VALUE> get(CONDITION condition);

        default Factory<CONDITION, VALUE> orElse(VALUE value) {
            return condition -> get(condition).orElse(value);
        }

        default ElseIfFactory<CONDITION, VALUE> when(Predicate<CONDITION> predicate) {
            return new ElseIfFactory<CONDITION, VALUE>() {
                @Override
                public boolean matches(CONDITION condition) {
                    return predicate.test(condition);
                }

                @Override
                public Optional<VALUE> previousIf(CONDITION condition) {
                    return get(condition);
                }
            };
        }
    }

    interface Factory<CONDITION, VALUE> {
        VALUE create(CONDITION condition);
    }
}
