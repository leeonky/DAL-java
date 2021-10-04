package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.util.DataObject;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static java.lang.String.format;

public class PropertyNode extends Node {
    private final Node instanceNode;
    private final Object name;
    private final Type type;

    public PropertyNode(Node instanceNode, Object name, Type type) {
        this.instanceNode = instanceNode;
        this.name = name;
        this.type = type;
    }

    public Object getName() {
        return name;
    }

    @Override
    public DataObject evaluateDataObject(RuntimeContext context) {
        try {
            return instanceNode.evaluateDataObject(context).getValue(name);
        } catch (IndexOutOfBoundsException ex) {
            throw new RuntimeException("Index out of bounds (" + ex.getMessage() + ")", getPositionBegin());
        } catch (Exception e) {
            throw new RuntimeException(format(
                    "Get property via `%s` failed, property can be public field, getter or customer type getter:\n\t"
                            + e.getMessage(), inspect()), getPositionBegin());
        }
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        return evaluateDataObject(context).getInstance();
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

    public Object getRootName() {
        if (instanceNode instanceof PropertyNode)
            return ((PropertyNode) instanceNode).getRootName();
        return name;
    }

    @Override
    public Node avoidListMapping() {
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
