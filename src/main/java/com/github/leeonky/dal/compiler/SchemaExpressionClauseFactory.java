package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.SchemaExpression;
import com.github.leeonky.dal.ast.SchemaNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.ExpressionClause;
import com.github.leeonky.interpreter.ExpressionClauseFactory;
import com.github.leeonky.interpreter.NodeFactory;
import com.github.leeonky.interpreter.TokenParser;

import java.util.List;

public class SchemaExpressionClauseFactory implements ExpressionClauseFactory<DALRuntimeContext, DALNode, DALExpression> {
    public static final NodeFactory<DALRuntimeContext, DALNode, DALExpression> SCHEMA;

    static {
        SCHEMA = TokenParser.SCHEMA.map(token -> new SchemaNode(token.getContent()));
    }

    @Override
    public ExpressionClause<DALRuntimeContext, DALNode> fetch(TokenParser<DALExpression, DALNode, DALRuntimeContext> tokenParser) {
        return fetchSchemaClause(tokenParser, 0);
    }

    private ExpressionClause<DALRuntimeContext, DALNode> fetchSchemaClause(TokenParser<DALExpression, DALNode, DALRuntimeContext> tokenParser, int dimension) {
        if (dimension > 1)
            throw tokenParser.getSourceCode().syntaxError("Not support multidimensional schema", 0);
        return tokenParser.fetchBetween("[", "]", () -> fetchSchemaClause(tokenParser, dimension + 1)).orElseGet(() -> {
            List<SchemaNode> schemaNodes = tokenParser.fetchNodes("/", SCHEMA);
            return input -> new SchemaExpression(input, schemaNodes, dimension);
        });
    }
}
