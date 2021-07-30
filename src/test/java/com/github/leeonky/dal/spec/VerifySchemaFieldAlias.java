package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.type.FieldAlias;
import com.github.leeonky.dal.type.FieldAliases;
import com.github.leeonky.dal.type.Partial;
import org.junit.jupiter.api.Test;

import java.util.HashMap;

class VerifySchemaFieldAlias extends Base {

    @Test
    void support_define_field_alias() {
        dataAssert.getRuntimeContextBuilder().registerSchema(Order.class);

        assertPass(new HashMap<String, Object>() {{
            put("total", 1);
        }}, "is Order which .TotalCost = 1");
    }

    @Test
    void support_define_field_chain_alias() {
        dataAssert.getRuntimeContextBuilder().registerSchema(Order.class);

        assertPass(new HashMap<String, Object>() {{
            put("product", new HashMap<String, Object>() {{
                put("cost", 1);
            }});
        }}, "is Order which .ProductCost = 1");
    }

    @Test
    void nested_field_chain_alias() {
        dataAssert.getRuntimeContextBuilder()
                .registerSchema(Order.class)
                .registerSchema(Report.class)
        ;

        assertPass(new HashMap<String, Object>() {{
            put("order", new HashMap<String, Object>() {{
                put("product", new HashMap<String, Object>() {{
                    put("cost", 1);
                }});
            }});
        }}, "is Report which .order.ProductCost = 1");
    }

    @Partial
    @FieldAliases({
            @FieldAlias(alias = "ProductCost", field = "product.cost"),
            @FieldAlias(alias = "TotalCost", field = "total")
    })
    public static class Order {
    }

    public static class Report {
        public Order order;
    }

    //TODO alias same with field
}
