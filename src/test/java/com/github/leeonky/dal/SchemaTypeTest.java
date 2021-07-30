package com.github.leeonky.dal;

import com.github.leeonky.dal.type.FieldAlias;
import com.github.leeonky.dal.type.FieldAliases;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.LinkedList;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;

class SchemaTypeTest {
    @FieldAliases({
            @FieldAlias(alias = "ProductCost", field = "product.count"),
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
            assertThat((new SchemaType(Order.class).transformToFieldChain(new LinkedList<>(asList("ProductCost")))))
                    .containsExactly("product.count");
        }

        @Test
        void field_alias_chain() {
            assertThat((new SchemaType(Report.class).transformToFieldChain(new LinkedList<>(asList("order", "ProductCost")))))
                    .containsExactly("order", "product.count");
        }

        @Test
        void alias_field_chain() {
            assertThat((new SchemaType(Order.class).transformToFieldChain(new LinkedList<>(asList("ProductCost", "currency")))))
                    .containsExactly("product.count", "currency");
        }

        @Test
        void alias_alias_chain() {
            assertThat((new SchemaType(User.class).transformToFieldChain(new LinkedList<>(asList("ReportOrder", "ProductCost")))))
                    .containsExactly("report.order", "product.count");
        }
    }
}