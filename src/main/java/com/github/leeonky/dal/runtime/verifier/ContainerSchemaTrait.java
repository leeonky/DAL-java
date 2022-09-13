package com.github.leeonky.dal.runtime.verifier;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.util.BeanClass;

import java.util.List;
import java.util.Map;

import static com.github.leeonky.dal.runtime.verifier.SchemaVerifier.errorLog;
import static java.lang.String.format;
import static java.util.Collections.emptyMap;

public interface ContainerSchemaTrait {
    BeanClass<?> getElementType(BeanClass<?> type);

    default Map<?, Object> getExpectValues(Object expect) {
        return emptyMap();
    }

    List<?> getActualProperties(Data actual);

    default boolean verifySize(String subPrefix, int actualSize, int expectSize) {
        return true;
    }

    interface ContainerSizeSchemaTrait extends ContainerSchemaTrait {
        @Override
        default boolean verifySize(String subPrefix, int actualSize, int expectSize) {
            return actualSize == expectSize || errorLog("Expecting field `%s` to be size [%d], but was size [%d]",
                    subPrefix, expectSize, actualSize);
        }
    }

    abstract class ContainerSchema extends JavaValueSchema implements ContainerSchemaTrait {
        protected final String fieldFormat;

        public ContainerSchema(String subPrefix, BeanClass<?> type, Object expect, Data actual, String fieldFormat) {
            super(subPrefix, type, expect, actual);
            this.fieldFormat = fieldFormat;
        }

        @Override
        public boolean verify(DALRuntimeContext runtimeContext) {
            return verifySizeAndElements(runtimeContext, getActualProperties(actual), getExpectValues(expect));
        }

        private boolean verifySizeAndElements(DALRuntimeContext runtimeContext, List<?> actualProperties,
                                              Map<?, Object> expectValues) {
            return verifySize(subPrefix, actualProperties.size(), expectValues.size())
                    && actualProperties.stream().allMatch(property -> JavaValueSchema.createFieldSchema(
                    format(fieldFormat, subPrefix, property), getElementType(type), expectValues.get(property),
                    runtimeContext, actual.getValue(property)).verify(runtimeContext));
        }
    }
}
