package com.github.leeonky.dal.e2e;

import lombok.Setter;
import lombok.experimental.Accessors;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URL;
import java.util.HashMap;
import java.util.Map;

class VerifyClassSchema extends TestBase {

    @BeforeEach
    void register() {
        Map<String, String> fieldTypes = new HashMap<>();
        fieldTypes.put("f1", "String");
        fieldTypes.put("f2", "URL");
        dataAssert.getCompilingContextBuilder()
                .registerStringValueFormat(URL.class)
                .registerSchema("Bean", fieldTypes);
    }

    @Test
    void should_support_verify_java_class_fields() {
        assertPass(new Bean().setF1("str").setF2("http://www.hello.com"), "is Bean");
    }

    @Test
    void should_not_pass_when_object_has_more_fields() {
        assertFailed(new BeanWithMoreProperties(), "is Bean");
    }

    @Setter
    @Accessors(chain = true)
    public static class Bean {
        public String f1, f2;
    }

    @Setter
    @Accessors(chain = true)
    public static class BeanWithLessProperties {
        public String f1;
    }

    @Setter
    @Accessors(chain = true)
    public static class BeanWithMoreProperties {
        public String f1, f2, f3;
    }
}
