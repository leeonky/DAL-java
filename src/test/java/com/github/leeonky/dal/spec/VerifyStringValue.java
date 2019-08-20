package com.github.leeonky.dal.spec;

import org.junit.jupiter.api.Test;

class VerifyStringValue extends Base {

    @Test
    void verify_string_type() {
        assertPass("", "is String");
    }

    @Test
    void verify_url() {
        assertPass("http://www.baidu.com", "is URL");

        assertPass("http://www.baidu.com", "is URL which .protocol = 'http' and .host = 'www.baidu.com'");
    }

    @Test
    void verify_instant() {
        assertPass("2001-12-10T10:00:11Z", "is Instant");
    }
}
