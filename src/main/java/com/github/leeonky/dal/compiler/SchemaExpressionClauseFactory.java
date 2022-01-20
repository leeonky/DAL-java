package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.SchemaExpression;
import com.github.leeonky.dal.ast.SchemaNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.ExpressionClause;
import com.github.leeonky.interpreter.ExpressionClauseFactory;
import com.github.leeonky.interpreter.NodeFactory;
import com.github.leeonky.interpreter.TokenParser;

import java.util.List;

public class SchemaExpressionClauseFactory implements ExpressionClauseFactory<DALNode, DALRuntimeContext> {
    public static final NodeFactory<DALNode, DALRuntimeContext> SCHEMA;

    static {
        SCHEMA = TokenParser.SCHEMA.map(token -> new SchemaNode(token.getContent()));
    }

    @Override
    public ExpressionClause<DALNode, DALRuntimeContext> fetch(TokenParser<DALNode, DALRuntimeContext> tokenParser) {
        return fetchSchemaClause(tokenParser, 0);
    }

    private ExpressionClause<DALNode, DALRuntimeContext> fetchSchemaClause(TokenParser<DALNode, DALRuntimeContext> tokenParser, int dimension) {
        if (dimension > 1)
            throw tokenParser.getSourceCode().syntaxError("Not support multidimensional schema", 0);
        return tokenParser.fetchBetween("[", "]", () -> fetchSchemaClause(tokenParser, dimension + 1)).orElseGet(() -> {
            List<SchemaNode> schemaNodes = tokenParser.fetchNodes("/", SCHEMA);
            return input -> new SchemaExpression(input, schemaNodes, dimension);
        });
    }
}
