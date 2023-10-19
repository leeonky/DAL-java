package com.github.leeonky.dal;

import com.github.leeonky.dal.extensions.DALExtension;
import com.github.leeonky.util.Suppressor;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class DALTest {
    public static String staticMethod(String str) {
        return str.toUpperCase();
    }

    @Test
    void support_extend() {
        DALExtension.extensionForTest = dal ->
                dal.getRuntimeContextBuilder().registerStaticMethodExtension(DALTest.class);

        assertThat(new DAL().extend().<String>evaluate("input", "staticMethod")).isEqualTo("INPUT");
    }

    public boolean isCalled = false;

    @Test
    void test_extension_hook() {
        DAL dal = new DAL();
        isCalled = false;

        dal.getRuntimeContextBuilder().registerErrorHook((i, code, e) -> {
            ((DALTest) Suppressor.get(i::get)).isCalled = true;
            assertThat(i).isSameAs(DALTest.this);
            assertEquals("Error", e.getMessage());
            assertEquals("throwError", code);
        });

        assertThrows(Throwable.class, () -> dal.evaluate(this, "throwError"));
        assertThat(isCalled).isTrue();
    }

    @Test
    void test_extension_hook_in_evaluate_all() {
        DAL dal = new DAL();
        isCalled = false;

        dal.getRuntimeContextBuilder().registerErrorHook((i, code, e) -> {
            ((DALTest) Suppressor.get(i::get)).isCalled = true;
            assertThat(i).isSameAs(DALTest.this);
            assertEquals("Error", e.getMessage());
            assertEquals("throwError", code);
        });

        assertThrows(Throwable.class, () -> dal.evaluateAll(this, "throwError"));
        assertThat(isCalled).isTrue();
    }

    public void throwError() {
        throw new RuntimeException("Error");
    }
}