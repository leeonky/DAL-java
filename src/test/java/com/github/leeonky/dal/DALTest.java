package com.github.leeonky.dal;

import com.github.leeonky.dal.extensions.DALExtension;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class DALTest {

    @Test
    void support_extend() {
        DALExtension.extensionForTest = dal ->
                dal.getRuntimeContextBuilder().registerSchema("ExtensionSchema", (a, c) -> true);

        assertThat(new DAL().extend().<String>evaluate("input", "is ExtensionSchema")).isEqualTo("input");
    }

    public boolean isCalled = false;

    @Test
    void test_extension_hook() {
        DAL dal = new DAL();
        isCalled = false;

        dal.getRuntimeContextBuilder().registerErrorHook((i, code, e) -> {
            ((DALTest) i).isCalled = true;
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
            ((DALTest) i).isCalled = true;
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