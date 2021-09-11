package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.FieldAlias;
import com.github.leeonky.dal.type.FieldAliases;
import com.github.leeonky.dal.type.Partial;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashMap;

import static java.util.Collections.singleton;

class VerifySchemaFieldAlias extends Base {

    @BeforeEach
    void registerSchema() {
        dataAssert.getRuntimeContextBuilder()
                .registerSchema(Order.class)
                .registerSchema(User.class)
        ;
    }

    @Test
    void should_work_for_use_alias_in_complex_expression() {
        assertPass(new HashMap<String, Object>() {{
            put("id", "001");
            put("lines", singleton(new HashMap<String, Object>() {{
                put("amount", "100");
            }}));
            put("user", new HashMap<String, Object>() {{
                put("age", 30);
            }});
        }}, "is Order which .aliasOfId = '001' and .user.aliasOfAge = 30 and .firstLine.amount = '100'");
    }

    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id"),
            @FieldAlias(alias = "aliasOfAliasId", field = "aliasOfId"),
            @FieldAlias(alias = "firstLine", field = "lines[0]"),
            @FieldAlias(alias = "userName", field = "user.name"),
    })
    public static class Order {
        @AllowNull
        public User user;
    }

    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfAge", field = "age"),
    })
    public static class User {
    }
}
