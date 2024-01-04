package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.runtime.*;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.List;
import java.util.stream.Collector;

import static com.github.leeonky.dal.runtime.ExpressionException.opt1;
import static java.lang.String.format;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class SchemaComposeNode extends DALNode {
    private final List<SchemaNode> schemas;
    private final boolean isList;

    public SchemaComposeNode(List<SchemaNode> schemas, boolean isList) {
        this.schemas = schemas;
        this.isList = isList;
    }

    @Override
    public String inspect() {
        Collector<CharSequence, ?, String> joining = isList ? joining(" / ", "[", "]") : joining(" / ");
        return schemas.stream().map(SchemaNode::inspect).collect(joining);
    }

    public Data verify(DALNode input, DALRuntimeContext context) {
        try {
            List<Object> instanceBySchema = schemas.stream().map(schemaNode ->
                    verifyAndConvertAsSchemaType(context, schemaNode, input)).collect(toList());
            return context.wrap(instanceBySchema.get(instanceBySchema.size() - 1), schemas.get(0).inspect(), isList);
        } catch (IllegalStateException e) {
            throw new RuntimeException(e.getMessage(), getPositionBegin());
        }
    }

    private Object verifyAndConvertAsSchemaType(DALRuntimeContext context, SchemaNode schemaNode, DALNode input) {
        Data inputData = input.evaluateData(context);
        if (isList)
            try {
                DALCollection<Object> collection = opt1(inputData::list).wraps().map((index, data) ->
                        convertViaSchema(context, schemaNode, data, format("%s[%d]", input.inspect(), index)));
                //get size to avoid lazy mode, should verify element with schema
                collection.collect();
                return collection;
            } catch (InfiniteCollectionException e) {
                throw new RuntimeException("Not supported for infinite collection", getPositionBegin());
            }
        else
            return convertViaSchema(context, schemaNode, inputData, input.inspect());
    }

    private Object convertViaSchema(DALRuntimeContext context, SchemaNode schemaNode, Data element, String input) {
        try {
            return schemaNode.getValueConstructorViaSchema(context).apply(element, context);
        } catch (IllegalTypeException exception) {
            throw new AssertionFailure(exception.assertionFailureMessage(input.isEmpty() ? input : input + " ",
                    schemaNode), schemaNode.getPositionBegin());
        }
    }
}
