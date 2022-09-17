package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.function.BiPredicate;
import java.util.function.Predicate;

public interface Assertion {
    boolean matches(Verification verification);

    boolean verifyStructure(Verification verification, DALRuntimeContext context);

    boolean verifyContent(Verification verification, DALRuntimeContext context);

    default boolean verify(DALRuntimeContext runtimeContext, Verification verification) {
        return verification.structure() ? verifyStructure(verification, runtimeContext)
                : verifyContent(verification, runtimeContext);
    }

    interface PredicateAble {
        Predicate<Verification> predicate();

        static PredicateAble when(Predicate<Verification> predicate) {
            return () -> predicate;
        }

        default Assertion then(BiPredicate<Verification, DALRuntimeContext> verifyStructure,
                               BiPredicate<Verification, DALRuntimeContext> verifyContent) {
            return new Assertion() {
                @Override
                public boolean matches(Verification verification) {
                    return predicate().test(verification);
                }

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
    }
}
