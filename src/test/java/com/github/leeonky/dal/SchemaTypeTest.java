package com.github.leeonky.dal;

import com.github.leeonky.dal.type.FieldAlias;
import com.github.leeonky.dal.type.FieldAliases;
import com.github.leeonky.util.BeanClass;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class SchemaTypeTest {

    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id"),
            @FieldAlias(alias = "aliasOfAliasId", field = "aliasOfId"),
            @FieldAlias(alias = "aliasOfUserName", field = "user.name"),
            @FieldAlias(alias = "firstLine", field = "lines[0]")
    })
    public static class Order {
        public User user;
        public List<OrderLine> lines;
    }

    @FieldAliases({
            @FieldAlias(alias = "aliasOfName", field = "name"),
    })
    public static class User {
    }

    @FieldAliases({
            @FieldAlias(alias = "aliasOfProduct", field = "product"),
    })
    public static class OrderLine {
    }

    @Nested
    class AccessProperty {
        SchemaType schemaOrder = SchemaType.create(BeanClass.create(Order.class));

        @Test
        void access_property_directly() {
            SchemaType subSchema = schemaOrder.access("property");

            assertThat(subSchema.getPropertyChainBefore(schemaOrder)).containsExactly("property");
        }


        @Test
        void access_property_from_single_property() {
            SchemaType subSchema = schemaOrder.access("aliasOfId");

            assertThat(subSchema.getPropertyChainBefore(schemaOrder)).containsExactly("id");
        }

        @Test
        void the_second_node_of_chain_is_alias() {
            SchemaType subSchema = schemaOrder.access("user").access("aliasOfName");

            assertThat(subSchema.getPropertyChainBefore(schemaOrder)).containsExactly("user", "name");
        }

        @Test
        void alias_is_property_chain() {
            SchemaType subSchema = schemaOrder.access("aliasOfUserName");

            assertThat(subSchema.getPropertyChainBefore(schemaOrder)).containsExactly("user", "name");
        }

        @Test
        void alias_is_another_alias() {
            SchemaType subSchema = schemaOrder.access("aliasOfAliasId");

            assertThat(subSchema.getPropertyChainBefore(schemaOrder)).containsExactly("id");
        }

        @Test
        void property_is_list_index() {
            SchemaType subSchema = schemaOrder.access("lines").access(0);

            assertThat(subSchema.getPropertyChainBefore(schemaOrder)).containsExactly("lines", 0);
        }

        @Test
        void list_element_has_alias() {
            SchemaType subSchema = schemaOrder.access("lines").access(0).access("aliasOfProduct");

            assertThat(subSchema.getPropertyChainBefore(schemaOrder)).containsExactly("lines", 0, "product");
        }

        @Test
        void alias_is_list_element() {
            SchemaType subSchema = schemaOrder.access("firstLine");

            assertThat(subSchema.getPropertyChainBefore(schemaOrder)).containsExactly("lines", 0);
        }
    }
}