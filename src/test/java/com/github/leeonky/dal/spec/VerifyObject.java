package com.github.leeonky.dal.spec;

import java.util.HashMap;

class VerifyObject extends Base {

    //    @Test
    void support_verify_empty_object() {
        assertPass(new HashMap<>(), "= {}");
        assertFailed(new HashMap<String, Object>() {{
            put("key", 1);
        }}, "= {}");
    }
}
