package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static java.lang.String.format;

public class PropertyNode extends DALNode {
    private final DALNode instanceNode;
    private final Object name;
    private final Type type;

    public PropertyNode(DALNode instanceNode, Object name, Type type) {
        this.instanceNode = instanceNode;
        this.name = name;
        this.type = type;
    }

    @Override
    public Data evaluateDataObject(RuntimeContextBuilder.DALRuntimeContext context) {
        Data data = instanceNode.evaluateDataObject(context);
        if (data.isNull())
            throw new RuntimeException("Instance is null", getPositionBegin());
        try {
            return data.getValue(name);
        } catch (IndexOutOfBoundsException ex) {
            throw new RuntimeException("Index out of bounds (" + ex.getMessage() + ")", getPositionBegin());
        } catch (Exception e) {
            throw new RuntimeException(format("Get property `%s` failed, property can be:\n" +
                    "  1. public field\n" +
                    "  2. public getter\n" +
                    "  3. public no args method\n" +
                    "  4. Map key value\n" +
                    "  5. customized type getter\n" +
                    "  6. static method extension\n" +
                    e.getMessage(), inspect()), getPositionBegin());
        }
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof PropertyNode
                && Objects.equals(instanceNode, ((PropertyNode) obj).instanceNode)
                && Objects.equals(name, ((PropertyNode) obj).name);
    }

    public List<Object> getChain() {
        return new ArrayList<Object>() {{
            if (instanceNode instanceof PropertyNode)
                addAll(((PropertyNode) instanceNode).getChain());
            add(name);
        }};
    }

    @Override
    public String inspect() {
        return type.format(instanceNode.inspect(), name);
    }

    @Override
    public Object getRootName() {
        Object parentRoot = instanceNode.getRootName();
        return parentRoot == null ? name : parentRoot;
    }

    @Override
    public DALNode avoidListMapping() {
        if ("@".equals(name))
            throw new SyntaxException("element property needed", getPositionBegin());
        return this;
    }

    public enum Type {
        DOT("%s.%s"),
        IDENTIFIER("%s%s"),
        BRACKET("%s[%s]") {
            @Override
            protected String valueString(Object value) {
                return value instanceof String ? String.format("'%s'", value) : String.valueOf(value);
            }
        };
        private final String format;

        Type(String format) {
            this.format = format;
        }

        protected String valueString(Object value) {
            return String.valueOf(value);
        }

        public String format(String input, Object value) {
            return String.format(format, input, valueString(value));
        }
    }
}
