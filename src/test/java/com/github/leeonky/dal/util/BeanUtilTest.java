package com.github.leeonky.dal.util;

import lombok.Setter;
import lombok.experimental.Accessors;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class BeanUtilTest {

    @Setter
    @Accessors(chain = true)
    public static class Bean {
        public int field;
    }

    public static class BeanWithGetter {
        public String getField() {
            return null;
        }

        public void getA() {
        }

        public int getB(int i) {
            return 0;
        }
    }

    public static class BeanWithBoolGetter {
        public boolean isBool() {
            return false;
        }

        public boolean isBool2(int j) {
            return false;
        }

        public Boolean isBool3() {
            return false;
        }

        public String isBool4() {
            return null;
        }

        public void isBool5() {
        }
    }

    public static class BeanWithBooleanGetter {
        public Boolean getBoolean() {
            return false;
        }

        public Boolean getBool3(String s) {
            return false;
        }
    }

    @Setter
    @Accessors(chain = true)
    public static class BeanWithFieldAndGetter {
        public int field;

        public int getField() {
            return 100;
        }
    }

    @Nested
    class FindPropertyNames {

        @Test
        void should_return_all_public_fields() {
            assertThat(BeanUtil.findPropertyReaderNames(Bean.class)).containsOnly("field");
        }

        @Test
        void property_names_should_contains_standard_getter() {
            assertThat(BeanUtil.findPropertyReaderNames(BeanWithGetter.class)).containsOnly("field");
        }

        @Test
        void property_names_should_contains_standard_bool_getter() {
            assertThat(BeanUtil.findPropertyReaderNames(BeanWithBoolGetter.class)).containsOnly("bool");
        }

        @Test
        void property_names_should_contains_standard_boolean_getter() {
            assertThat(BeanUtil.findPropertyReaderNames(BeanWithBooleanGetter.class)).containsOnly("boolean");
        }
    }

    @Nested
    class GetPropertyValue {

        @Test
        void should_get_field_value() throws Exception {
            assertThat(BeanUtil.getPropertyValue(new Bean().setField(10), "field")).isEqualTo(10);
        }

        @Test
        void should_get_value_from_getter_which_override_field() throws Exception {
            assertThat(BeanUtil.getPropertyValue(new BeanWithFieldAndGetter().setField(10), "field")).isEqualTo(100);
        }

        @Test
        void should_raise_error_when_getter_is_not_standard() {
            assertThrows(Exception.class, () -> BeanUtil.getPropertyValue(new BeanWithGetter(), "a"));
        }
    }
}