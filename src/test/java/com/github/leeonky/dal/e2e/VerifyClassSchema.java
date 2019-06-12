package com.github.leeonky.dal.e2e;

import lombok.Setter;
import lombok.experimental.Accessors;
import org.junit.jupiter.api.Test;

class VerifyClassSchema extends VerifyBase {

    @Test
    void should_check_get_class_for_class_schema() {
        dataAssert.getCompilingContextBuilder().registerClassSchema(Bean.class);

        assertPass(new Bean().setF1("str").setF2("http://www.hello.com"), "is Bean");
    }

    @Test
    void should_not_pass_when_object_has_more_fields() {
        dataAssert.getCompilingContextBuilder().registerClassSchema(Bean.class);

        assertFailed(new AnotherBean(), "is Bean");
    }

    @Test
    void should_not_pass_when_object_is_null() {
        dataAssert.getCompilingContextBuilder().registerClassSchema(Bean.class);

        assertFailed(null, "is Bean");
    }

    @Setter
    @Accessors(chain = true)
    public static class Bean {
        public String f1, f2;
    }

    @Setter
    @Accessors(chain = true)
    public static class AnotherBean {
        public String f1, f2;
    }
}
