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
    void support_define_property_alias() {
        assertPass(new HashMap<String, Object>() {{
            put("id", "001");
        }}, "is Order which .aliasOfId = '001'");
    }

    @Test
    void recursive_alias() {
        assertPass(new HashMap<String, Object>() {{
            put("id", "001");
        }}, "is Order which .aliasOfAliasId = '001'");
    }

    @Test
    void access_array_in_alias() {
        assertPass(new HashMap<String, Object>() {{
            put("lines", singleton(new HashMap<String, Object>() {{
                put("amount", "100");
            }}));
        }}, "is Order which .firstLine.amount = '100'");
    }

    @Test
    void access_property_chain_in_alias() {
        assertPass(new HashMap<String, Object>() {{
            put("user", new HashMap<String, Object>() {{
                put("name", 1);
            }});
        }}, "is Order which .userName = 1");
    }

    @Test
    void use_alias_in_sub_property() {
        assertPass(new HashMap<String, Object>() {{
            put("user", new HashMap<String, Object>() {{
                put("age", 30);
            }});
        }}, "is Order which .user.aliasOfAge = 30");
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
