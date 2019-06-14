package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.util.ListAccessor;
import com.github.leeonky.dal.util.PropertyAccessor;
import com.github.leeonky.dal.util.TypeData;

import java.util.LinkedList;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

import static com.github.leeonky.dal.CompilingContextBuilder.requiredType;

public class CompilingContext {
    private final TypeData<PropertyAccessor> propertyAccessors;
    private final TypeData<ListAccessor> listAccessors;
    private final LinkedList<Object> wrappedValueStack = new LinkedList<>();
    private final Map<String, Function<Object, Object>> typeDefinitions;

    public CompilingContext(Object inputValue, TypeData<PropertyAccessor> propertyAccessors,
                            Map<String, Function<Object, Object>> typeDefinitions, TypeData<ListAccessor> listAccessors) {
        this.propertyAccessors = propertyAccessors;
        this.listAccessors = listAccessors;
        wrappedValueStack.add(inputValue);
        this.typeDefinitions = typeDefinitions;

        typeDefinitions.put("List", o -> requiredType(isList(o), () -> o));
    }

    public boolean isList(Object o) {
        return o != null && listAccessors.containsType(o) || o instanceof Iterable || o.getClass().isArray();
    }

    public Object getInputValue() {
        return wrappedValueStack.getLast();
    }

    public Object wrapInputValueAndEvaluate(Object value, Node node) {
        try {
            wrappedValueStack.add(value);
            return node.evaluate(this);
        } finally {
            wrappedValueStack.removeLast();
        }
    }

    public Optional<Function<Object, Object>> searchTypeDefinition(String type) {
        return Optional.ofNullable(typeDefinitions.get(type));
    }

    public Optional<PropertyAccessor> searchPropertyAccessor(Object object) {
        return propertyAccessors.getData(object);
    }

    public Optional<ListAccessor> searchListAccessor(Object object) {
        return listAccessors.getData(object);
    }
}
