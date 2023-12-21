package com.github.leeonky.dal;

import com.github.leeonky.dal.runtime.DALCollection;
import com.github.leeonky.dal.runtime.DALCollectionFactory;
import com.github.leeonky.dal.runtime.Result;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.checker.Checker;
import com.github.leeonky.dal.runtime.checker.CheckingContext;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.Assertions.expect;
import static java.util.Optional.of;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

class DALRuntimeContextTest {

    @Test
    void invoke_static_extended_method_through_object_implicit_data() {
        DAL dal = new DAL();
        RuntimeContextBuilder runtimeContextBuilder = dal.getRuntimeContextBuilder();
        runtimeContextBuilder.registerStaticMethodExtension(StaticMethods.class);
        runtimeContextBuilder.registerImplicitData(Obj.class, obj -> "obj-string");

        org.assertj.core.api.Assertions.assertThat((Object) dal.evaluate(new Obj(), "objString")).isEqualTo("obj-string");
    }

    @Nested
    class IsList {
        private final RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder();

        @Test
        void return_false_when_checking_return_false() {
            runtimeContextBuilder.registerDALCollectionFactory(DynamicList.class, new DALCollectionFactory<DynamicList, Object>() {
                @Override
                public DALCollection<Object> create(DynamicList instance) {
                    return null;
                }

                @Override
                public boolean isList(DynamicList instance) {
                    return instance.isList();
                }
            });
            RuntimeContextBuilder.DALRuntimeContext context = runtimeContextBuilder.build(null);

            assertThat(context.isRegisteredList(new DynamicList().setList(true))).isTrue();
            assertThat(context.isRegisteredList(new DynamicList().setList(false))).isFalse();
        }
    }

    @Getter
    @Setter
    @Accessors(chain = true)
    public static class DynamicList {
        private boolean list;
    }

    public static class Obj {

    }

    public static class StaticMethods {

        public static String objString(String string) {
            return string;
        }
    }

    @Nested
    class CustomizedChecker {
        DAL dal = new DAL().extend();
        Checker checker = spy(new Checker() {
            @Override
            public boolean failed(CheckingContext checkingContext) {
                return false;
            }

            @Override
            public String message(CheckingContext checkingContext) {
                return null;
            }
        });

        @BeforeEach
        void registerLiteralAndChecker() {
            Target target = new Target();
            dal.getRuntimeContextBuilder().registerUserDefinedLiterals(token ->
                    token.equals("target") ? Result.of(target) : Result.empty());
        }

        @Nested
        class Match {

            @BeforeEach
            void registerChecker() {
                dal.getRuntimeContextBuilder().checkerSetForMatching().register(Target.class, (d1, d2) -> of(checker));
            }

            @Test
            void pass() {
                when(checker.failed(any())).thenReturn(false);

                expect(null).use(dal).should(": target");

                verify(checker).failed(any());
            }

            @Test
            void should_raise_error_and_throw_with_right_message() {
                when(checker.failed(any())).thenReturn(true);
                when(checker.message(any())).thenReturn("customer-error!");

                assertThatThrownBy(() -> expect(null).use(dal).should(": target"))
                        .hasMessageContaining("customer-error!");
            }
        }

        @Nested
        class Equal {

            @Nested
            class SameExpectedAndActualType {

                @BeforeEach
                void registerChecker() {
                    dal.getRuntimeContextBuilder().checkerSetForEqualing().register(Target.class, Actual.class, (expect, actual) -> of(checker));
                }

                @Test
                void pass() {
                    when(checker.failed(any())).thenReturn(false);

                    expect(new Actual()).use(dal).should("= target");

                    verify(checker).failed(any());
                }

                @Test
                void should_raise_error_and_throw_with_right_message() {
                    when(checker.failed(any())).thenReturn(true);
                    when(checker.message(any())).thenReturn("customer-error!");

                    assertThatThrownBy(() -> expect(new Actual()).use(dal).should("= target"))
                            .hasMessageContaining("customer-error!");
                }
            }
        }
    }

    public static class Target {
    }

    public static class Actual {
    }
}