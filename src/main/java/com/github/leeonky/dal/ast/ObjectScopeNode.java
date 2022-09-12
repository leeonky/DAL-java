package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Matcher;
import com.github.leeonky.dal.runtime.CurryingMethod;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.AssertionFailure.assertUnexpectedFields;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;

public class ObjectScopeNode extends DALNode {
    private final List<DALNode> verificationExpressions = new ArrayList<>();
    private final boolean isObjectWildcard;

    public ObjectScopeNode(List<DALNode> expressions) {
        verificationExpressions.addAll(expressions);
        isObjectWildcard = false;
    }

    public ObjectScopeNode(DALNode ignore) {
        isObjectWildcard = true;
    }

    @Override
    public String inspect() {
        return format("{%s}", isObjectWildcard ? "..." : verificationExpressions.stream().map(DALNode::inspect)
                .collect(joining(", ")));
    }

    @Override
    protected boolean verify(Data data, Equal operator, DALRuntimeContext context, DALNode actualNode) {
        checkNull(data);
        return data.newBlockScope(() -> {
            verificationExpressions.forEach(expression -> expression.evaluate(context));
            assertUnexpectedFields(collectUnexpectedFields(data, context), actualNode.inspect(), operator.getPosition());
            return true;
        });
    }

    private Set<Object> collectUnexpectedFields(Data data, DALRuntimeContext context) {
        return new LinkedHashSet<Object>(data.getFieldNames()) {{
            Stream.concat(collectFields(data), context.collectPartialProperties(data).stream())
                    .map(obj -> convertFiled(data, obj)).forEach(this::remove);
        }};
    }

    private Object convertFiled(Data data, Object obj) {
        return data.getInstance() instanceof CurryingMethod ?
                ((CurryingMethod) data.getInstance()).convertToArgType(obj) : obj;
    }

    @Override
    protected boolean verify(Data data, Matcher operator, DALRuntimeContext context, DALNode actualNode) {
        if (verificationExpressions.isEmpty() && !isObjectWildcard)
            throw new SyntaxException("Should use `{...}` to verify any non null object", getPositionBegin());
        checkNull(data);
        return data.newBlockScope(() -> {
            verificationExpressions.forEach(expression -> expression.evaluate(context));
            return true;
        });
    }

    private void checkNull(Data data) {
        if (data.isNull())
            throw new AssertionFailure("The input value is null", getPositionBegin());
    }

    @Override
    public Stream<Object> collectFields(Data data) {
        return verificationExpressions.stream().flatMap(expression -> expression.collectFields(data));
    }
}
