Feature: const remark

  Rule: parentheses ()

    Scenario: N/A for parentheses
      When evaluate by:
      """
      (1+1)(2)
      """
      Then failed with the message:
      """
      more than one expression
      """
      And got the following notation:
      """
      (1+1)(2)
           ^
      """

  Rule: property . []

    Scenario: N/A after property
      Given the following json:
      """
      [2]
      """
      When evaluate by:
      """
      .0(2)
      """
      Then failed with the message:
      """
      Expect a symbol
      """
      And got the following notation:
      """
      .0(2)
       ^
      """
      When evaluate by:
      """
      [0](2)
      """
      Then failed with the message:
      """
      Not implement operator () of java.lang.Integer
      """
      And got the following notation:
      """
      [0](2)
         ^
      """
      When evaluate by:
      """
      : {
        0(2): 2
      }
      """
      Then failed with the message:
      """
      Not implement operator () of java.lang.Integer
      """
      And got the following notation:
      """
      : {
        0(2): 2
         ^
      }
      """
# TODO missing test
#    Scenario: should keep expression before property
#      Then the following verification should pass:
#      """
#      2(1+1).toString= '2'
#      """

  Rule: unary ! -

    Scenario: lower precedence than minus
      Then the following verification should pass:
      """
      (-2(-2))= -2
      """

    Scenario: N/A for negative
      When evaluate by:
      """
      !2(-2)
      """
      Then failed with the message:
      """
      Operand should be boolean but 'java.lang.Integer'
      """
      And got the following notation:
      """
      !2(-2)
      ^
      """

  Rule: arithmetic * /

    Scenario: has higher precedence
      Then the following verification should pass:
      """
      10 * 2(1+1)= 20,
      10 / 2(1+1)= 5
      """
      Then the following verification should pass:
      """
      2(1+1) * 10= 20,
      10(5+5) / 2= 5
      """

  Rule: arithmetic + -

    Scenario: has higher precedence
      Then the following verification should pass:
      """
      10 + 2(1+1)= 12,
      10 - 2(1+1)= 8
      """
      Then the following verification should pass:
      """
      2(1+1) + 10= 12,
      10(5+5) - 2= 8
      """

  Rule: comparison > >= < <= !=

    Scenario: has higher precedence
      Then the following verification should pass:
      """
      10 > 2(1+1)= true,
      10 >= 2(1+1)= true,
      1 < 2(1+1)= true,
      1 <= 2(1+1)= true,
      1 != 2(1+1)= true
      """
      Then the following verification should pass:
      """
      2(1+1) < 10= true,
      2(1+1) <= 10= true,
      2(1+1) > 1= true,
      2(1+1) >= 1= true,
      2(1+1) != 1= true
      """

  Rule: logical && || and or

    Scenario: has higher precedence
      Then the following verification should pass:
      """
      1 && 2(1+1)= 2,
      false || 2(1+1)= 2,
      1 and 2(1+1)= 2,
      false or 2(1+1)= 2
      """
      Then the following verification should pass:
      """
      2(1+1) && 1= 1,
      2(1+1) || false= 2,
      2(1+1) and 1= 1,
      2(1+1) or false= 2
      """

  Rule: verification

    Scenario: has higher precedence
      Then the following verification should pass:
      """
      2(1+1)= 2,
      2(1+1): 2
      """
      Then the following verification should pass:
      """
      2= 2(1+1),
      2: 2(1+1)
      """

  Rule: legacy

    Scenario: const remark has lower precedence and change expression
      Then the following verification should pass:
      """
      (-10(-5-5))= -10
      """
