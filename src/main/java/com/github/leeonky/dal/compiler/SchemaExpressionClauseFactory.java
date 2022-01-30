package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.ExpressionClause;
import com.github.leeonky.interpreter.ExpressionClauseParser;
import com.github.leeonky.interpreter.NodeParser;

import java.util.List;
import java.util.stream.Collectors;

public class SchemaExpressionClauseFactory implements ExpressionClauseParser.ExpressionClauseFactory<DALRuntimeContext, DALNode, DALExpression,
        DALOperator, DALParser> {
    public static final NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALParser> SCHEMA;

    static {
        SCHEMA = Tokens.SCHEMA.map(token -> new SchemaNode(token.getContent()));
    }

    @Override
    public ExpressionClause<DALRuntimeContext, DALNode> parse(DALParser parser) {
        return fetchSchemaClause(parser, 0);
    }

    private ExpressionClause<DALRuntimeContext, DALNode> fetchSchemaClause(DALParser dalParser, int dimension) {
        if (dimension > 1)
            throw dalParser.getSourceCode().syntaxError("Not support multidimensional schema", 0);
        return dalParser.fetchBetween("[", "]", () -> fetchSchemaClause(dalParser, dimension + 1)).orElseGet(() -> {
            List<SchemaNode> schemaNodes = dalParser.fetchNodesSplitBy("/", SCHEMA).stream()
                    .map(SchemaNode.class::cast).collect(Collectors.toList());
            return input -> new SchemaExpression(input, schemaNodes, dimension);
        });
    }
}
