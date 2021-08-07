package com.github.leeonky.dal;

import com.github.leeonky.dal.type.FieldAlias;
import com.github.leeonky.dal.type.FieldAliases;
import com.github.leeonky.util.BeanClass;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.LinkedList;
import java.util.List;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;

class SchemaTypeTestBk {
    private void assertTransform(Class<?> schema, List<String> list, String... chains) {
        assertThat((new SchemaType(BeanClass.create(schema)).transformToFieldChain(new LinkedList<>(list))))
                .containsExactly(chains);
    }

    @FieldAliases({
            @FieldAlias(alias = "ProductCost", field = "product.count"),
            @FieldAlias(alias = "DuplicatedProductCost", field = "ProductCost"),
    })
    public static class Order {
    }

    public static class Report {
        public Order order;
    }

    @FieldAliases({
            @FieldAlias(alias = "ReportOrder", field = "report.order"),
    })
    public static class User {
        public Report report;
    }

    @Nested
    class Transform {

        @Test
        void simple_transform() {
            assertTransform(Order.class, asList("ProductCost"), "product.count");
        }

        @Test
        void field_alias_chain() {
            assertTransform(Report.class, asList("order", "ProductCost"), "order", "product.count");
        }

        @Test
        void alias_field_chain() {
            assertTransform(Order.class, asList("ProductCost", "currency"), "product.count", "currency");
            assertThat((new SchemaType(BeanClass.create(Order.class)).transformToFieldChain(new LinkedList<>(asList("ProductCost", "currency")))))
                    .containsExactly("product.count", "currency");
        }

        @Test
        void alias_alias_chain() {
            assertTransform(User.class, asList("ReportOrder", "ProductCost"), "report.order", "product.count");
        }
    }
}