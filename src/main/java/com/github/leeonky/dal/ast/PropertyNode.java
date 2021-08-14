package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.RuntimeException;

import java.util.Objects;

import static java.lang.String.format;

public class PropertyNode extends Node {
    private final Node instanceNode;
    private final Object name;
    private final boolean isBracket;

    public PropertyNode(Node instanceNode, Object property) {
        this(instanceNode, property, false);
    }

    public PropertyNode(Node instanceNode, Object property, boolean isBracket) {
        this.instanceNode = instanceNode;
        name = property;
        this.isBracket = isBracket;
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        try {
            return context.getAliasValue(() -> instanceNode.evaluate(context), name);
        } catch (IndexOutOfBoundsException ex) {
            throw new RuntimeException(ex.getMessage(), getPositionBegin());
        } catch (Exception e) {
            throw new RuntimeException(format(
                    "Get property via `%s` failed, property can be public field, getter or customer type getter",
                    inspect()), getPositionBegin());
        }
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof PropertyNode
                && Objects.equals(instanceNode, ((PropertyNode) obj).instanceNode)
                && Objects.equals(name, ((PropertyNode) obj).name);
    }

    @Override
    public String inspect() {
        return format(isBracket ? (name instanceof String ? "%s['%s']" : "%s[%s]") : "%s.%s",
                instanceNode.inspect(), name);
    }
}
