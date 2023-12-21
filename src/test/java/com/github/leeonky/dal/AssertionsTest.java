package com.github.leeonky.dal;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.SchemaAssertionFailure;
import com.github.leeonky.dal.type.FieldAlias;
import com.github.leeonky.dal.type.FieldAliases;
import com.github.leeonky.dal.type.Partial;
import com.github.leeonky.dal.type.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.Assertions.expect;
import static com.github.leeonky.dal.Assertions.expectRun;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

public class AssertionsTest {

    @Test
    void given_dal_factory() {
        Assertions.setDalFactory(DAL::getInstance);

        expect(1).should("=1");
    }

    @Test
    void disable_dump_input() {
        try {
            Assertions.dumpInput(false);

            expect(1).should("=2");
        } catch (AssertionError error) {
            assertThat(error.getMessage()).doesNotContain("The root value was");
        } finally {
            Assertions.dumpInput(true);
        }
    }

    @Nested
    class GivenRootSchema {

        @Test
        void verify_input_by_root_schema() {
            assertThatThrownBy(() -> expect(new Bean().setValue(100)).is(BeanSchema.class))
                    .hasMessageContaining("Failed")
                    .hasMessageContaining("The root value was")
                    .hasMessageContaining("value: java.lang.Integer <100>");
        }

        @Test
        void raise_error_when_input_code_throw_error() {
            assertThatThrownBy(() -> expectRun(() -> {
                throw new RuntimeException("Error");
            }).is(BeanSchema.class))
                    .hasMessageContaining("Input code got exception: java.lang.RuntimeException")
                    .hasMessageContaining("Error");
        }

        @Test
        void use_root_schema_in_dal_verification() {
            expect(new Bean().setValue(1)).is(BeanSchema.class).should("aliasOfValue= 1");
        }

        @Test
        void use_root_schema_as_list_in_dal_verification() {
            expect(new Bean[]{new Bean().setValue(1)}).is(BeanSchema[].class).should("[0].aliasOfValue= 1");
        }

        @Test
        void specify_schema_by_name() {
            DAL dal = new DAL().extend();
            dal.getRuntimeContextBuilder().registerSchema(BeanSchema.class);

            expect(new Bean().setValue(1)).use(dal).is("BeanSchema").should("aliasOfValue= 1");
        }

        @Test
        void specify_schema_list_by_name() {
            DAL dal = new DAL().extend();
            dal.getRuntimeContextBuilder().registerSchema(BeanSchema.class);

            expect(new Bean[]{new Bean().setValue(1)}).use(dal).is("[BeanSchema]").should("[0].aliasOfValue= 1");
        }
    }

    @Getter
    @Setter
    @Accessors(chain = true)
    public class Bean {
        private int value;
    }

    @Partial
    @FieldAliases({
            @FieldAlias(field = "value", alias = "aliasOfValue")
    })
    public static class BeanSchema implements Schema {

        @Override
        public void verify(Data data) throws SchemaAssertionFailure {
            Bean bean = (Bean) data.instance();
            if (bean.getValue() != 1)
                throw new SchemaAssertionFailure("Failed");
        }
    }

//    List schema
//    use string schema
//    use string schema in list
}