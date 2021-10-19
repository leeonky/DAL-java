package com.github.leeonky.dal.compiler;

import java.util.Optional;

public interface ExpressionClauseMatcher {
    Optional<ExpressionClause> fetch(TokenParser tokenParser);
}
