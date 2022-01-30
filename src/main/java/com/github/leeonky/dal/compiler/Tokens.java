package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.OperatorFactory;
import com.github.leeonky.interpreter.Token;
import com.github.leeonky.interpreter.TokenFactory;
import com.github.leeonky.interpreter.TokenMatcher;

import java.util.HashSet;

import static com.github.leeonky.dal.compiler.Constants.*;
import static com.github.leeonky.interpreter.FunctionUtil.not;
import static com.github.leeonky.interpreter.SourceCode.tokenMatcher;
import static java.util.Arrays.asList;
import static java.util.Collections.emptySet;

public class Tokens {
    public static final TokenMatcher<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator,
            DALScanner>
            NUMBER = tokenMatcher(DIGITAL::contains, emptySet(), false, (lastChar, nextChar) ->
            ((lastChar != 'e' && lastChar != 'E') || (nextChar != '-' && nextChar != '+')) && DELIMITER.contains(nextChar), Token::isNumber),
            INTEGER = tokenMatcher(DIGITAL_OR_MINUS::contains, emptySet(), false, DELIMITER, Token::isNumber),
            IDENTITY_PROPERTY = tokenMatcher(not(DELIMITER::contains), ALL_KEY_WORDS, false, DELIMITER_OR_DOT, not(Token::isNumber)),
            DOT_PROPERTY = tokenMatcher(DOT::equals, new HashSet<>(asList(ELEMENT_ELLIPSIS, DOT.toString())), true, DELIMITER_OR_DOT);
    public static final TokenFactory SCHEMA = tokenMatcher(not(DELIMITER::contains), ALL_KEY_WORDS,
            false, DELIMITER, not(Token::isNumber)).or("expect a schema");
    public static final OperatorFactory<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator,
            DALScanner>
            DEFAULT_JUDGEMENT_OPERATOR = tokenParser -> tokenParser.currentOperator().orElseGet(DALOperator.Matcher::new);
}
