package com.github.leeonky.dal.e2e;

import org.junit.jupiter.api.Test;

import java.net.URL;

class VerifyStringValue extends VerifyBase {

    @Test
    void verify_string_type() {
        dataAssert.getCompilingContextBuilder().registerStringValueFormat(String.class);
        assertPass("", "is String");
    }

    @Test
    void verify_url() {
        dataAssert.getCompilingContextBuilder().registerStringValueFormat(URL.class);

        assertPass("http://www.baidu.com", "is URL");

        assertPass("http://www.baidu.com", "is URL which .protocol = 'http' and .host = 'www.baidu.com'");
    }
}
