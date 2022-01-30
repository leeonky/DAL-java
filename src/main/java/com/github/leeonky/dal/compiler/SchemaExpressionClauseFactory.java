package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.ExpressionClause;
import com.github.leeonky.interpreter.ExpressionClauseFactory;
import com.github.leeonky.interpreter.NodeFactory;

import java.util.List;
import java.util.stream.Collectors;

public class SchemaExpressionClauseFactory implements ExpressionClauseFactory<DALRuntimeContext, DALNode, DALExpression,
        DALOperator, DALScanner> {
    public static final NodeFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALScanner> SCHEMA;

    static {
        SCHEMA = Tokens.SCHEMA.map(token -> new SchemaNode(token.getContent()));
    }

    @Override
    public ExpressionClause<DALRuntimeContext, DALNode> fetch(DALScanner scanner) {
        return fetchSchemaClause(scanner, 0);
    }

    private ExpressionClause<DALRuntimeContext, DALNode> fetchSchemaClause(DALScanner tokenParser, int dimension) {
        if (dimension > 1)
            throw tokenParser.getSourceCode().syntaxError("Not support multidimensional schema", 0);
        return tokenParser.fetchBetween("[", "]", () -> fetchSchemaClause(tokenParser, dimension + 1)).orElseGet(() -> {
            List<SchemaNode> schemaNodes = tokenParser.fetchNodesSplitBy("/", SCHEMA).stream()
                    .map(SchemaNode.class::cast).collect(Collectors.toList());
            return input -> new SchemaExpression(input, schemaNodes, dimension);
        });
    }
}
