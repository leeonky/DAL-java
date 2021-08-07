package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.type.AllowNull;
import com.github.leeonky.dal.type.FieldAlias;
import com.github.leeonky.dal.type.FieldAliases;
import com.github.leeonky.dal.type.Partial;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashMap;

class VerifySchemaFieldAlias extends Base {

    @BeforeEach
    void registerSchema() {
        dataAssert.getRuntimeContextBuilder()
                .registerSchema(Order.class)
                .registerSchema(Product.class)
                .registerSchema(Report.class)
                .registerSchema(Sku.class)
        ;
    }

    @Test
    void support_define_field_alias() {
        assertPass(new HashMap<String, Object>() {{
            put("total", 1);
        }}, "is Order which .TotalCost = 1");
    }

    @Test
    void support_define_field_chain_alias() {
        assertPass(new HashMap<String, Object>() {{
            put("product", new HashMap<String, Object>() {{
                put("cost", 1);
            }});
        }}, "is Order which .ProductCost = 1");
    }

    @Test
    void support_field_alias_style_chain() {
        assertPass(new HashMap<String, Object>() {{
            put("order", new HashMap<String, Object>() {{
                put("product", new HashMap<String, Object>() {{
                    put("cost", 1);
                }});
            }});
        }}, "is Report which .order.ProductCost = 1");
    }

    @Test
    void support_alias_alias_style_chain() {
        assertPass(new HashMap<String, Object>() {{
            put("product", new HashMap<String, Object>() {{
                put("sku", new HashMap<String, Object>() {{
                    put("count", 10);
                }});
            }});
        }}, "is Order which .ProductSku.SkuCount = 10");
    }

    @Partial
    @FieldAliases({
            @FieldAlias(alias = "ProductCost", field = "product.cost"),
            @FieldAlias(alias = "TotalCost", field = "total"),
            @FieldAlias(alias = "ProductSku", field = "product.sku"),
    })
    public static class Order {
        @AllowNull
        public Product product;
    }

    @Partial
    public static class Product {
        @AllowNull
        public Sku sku;
    }

    @Partial
    @FieldAliases({
            @FieldAlias(alias = "SkuCount", field = "count"),
    })
    public static class Sku {
    }

    public static class Report {
        public Order order;
    }

    //TODO alias same with field *****
    //TODO alias to list element
}
