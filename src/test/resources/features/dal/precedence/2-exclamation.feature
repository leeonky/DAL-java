Feature: exclamation

  Background:
    Given the following java class:
    """
    public class Bean {
      public Bean(String s) {
        this.value = s;
      }
      public String value;
    }
    """
    Given the following java class:
    """
    public class BeanRef {
      public Bean bean;
      public BeanRef(){
        this.bean = new Bean("bean");
      }
    }
    """

  Rule: N/A for parentheses ()

    Scenario: N/A for parentheses
      When use a instance of java class "BeanRef" to evaluate:
      """
      (bean)!
      """
      Then failed with the message:
      """
      Expect a value or expression
      """
      And got the following notation:
      """
      (bean)!
             ^
      """

  Rule: same precedence with property . []

    Scenario Outline: <position> property <opt>
      When register DAL:
      """
      dal.getRuntimeContextBuilder().registerExclamation(Bean.class, rd-> rd.data().map(i->((Bean)i).value.toUpperCase()));
      """
      Then the following verification for the instance of java class "BeanRef" should pass:
      """
      <code>
      """
      Then the result should:
      """
      = <value>
      """
      Examples:
        | code                 | value | position | opt |
        | bean!                | BEAN  | after    |     |
        | .bean!               | BEAN  | after    | .   |
        | ['bean']!            | BEAN  | after    | []  |
        | bean!.toLowerCase    | bean  | before   | .   |
        | bean!['toLowerCase'] | bean  | before   | []  |

  Rule: higher precedence than unary ! -

    Scenario: with not !
      When register DAL:
      """
      dal.getRuntimeContextBuilder().registerExclamation(Bean.class, rd-> rd.data().map(i->true));
      """
      When use a instance of java class "BeanRef" to evaluate:
      """
      !bean!
      """
      Then the result should:
      """
      = false
      """

    Scenario: with negative -
      When register DAL:
      """
      dal.getRuntimeContextBuilder().registerExclamation(Bean.class, rd-> rd.data().map(i->100));
      """
      When use a instance of java class "BeanRef" to evaluate:
      """
      (-bean!)
      """
      Then the result should:
      """
      = -100
      """

  Rule: higher precedence than arithmetic + - * /

    Scenario Outline: <position> <opt>
      When register DAL:
      """
      dal.getRuntimeContextBuilder().registerExclamation(Bean.class, rd-> rd.data().map(i->100));
      """
      When use a instance of java class "BeanRef" to evaluate:
      """
      <operand1> <opt> <operand2>
      """
      Then the result should:
      """
      = <value>
      """
      Examples:
        | operand1 | opt | operand2 | value | position |
        | 10       | +   | bean!    | 110   | after    |
        | 10       | -   | bean!    | -90   | after    |
        | 10       | *   | bean!    | 1000  | after    |
        | 2400     | /   | bean!    | 24    | after    |
        | bean!    | +   | 2        | 102   | before   |
        | bean!    | -   | 2        | 98    | before   |
        | bean!    | *   | 2        | 200   | before   |
        | bean!    | /   | 5        | 20    | before   |

  Rule: higher precedence than comparison > >= < <= !=

    Scenario Outline: <position> <opt>
      When register DAL:
      """
      dal.getRuntimeContextBuilder().registerExclamation(Bean.class, rd-> rd.data().map(i->100));
      """
      When use a instance of java class "BeanRef" to evaluate:
      """
      <operand1> <opt> <operand2>
      """
      Then the result should:
      """
      = <value>
      """
      Examples:
        | operand1 | opt | operand2 | value | position |
        | 100      | >   | bean!    | false | after    |
        | 101      | >   | bean!    | true  | after    |
        | 99       | >=  | bean!    | false | after    |
        | 100      | >=  | bean!    | true  | after    |
        | 101      | >=  | bean!    | true  | after    |
        | 99       | <   | bean!    | true  | after    |
        | 100      | <   | bean!    | false | after    |
        | 99       | <=  | bean!    | true  | after    |
        | 100      | <=  | bean!    | true  | after    |
        | 101      | <=  | bean!    | false | after    |
        | 100      | !=  | bean!    | false | after    |
        | 101      | !=  | bean!    | true  | after    |
        | bean!    | >   | 99       | true  | before   |
        | bean!    | >   | 100      | false | before   |
        | bean!    | >=  | 100      | true  | before   |
        | bean!    | >=  | 99       | true  | before   |
        | bean!    | >=  | 101      | false | before   |
        | bean!    | <   | 100      | false | before   |
        | bean!    | <   | 101      | true  | before   |
        | bean!    | <=  | 99       | false | before   |
        | bean!    | <=  | 100      | true  | before   |
        | bean!    | <=  | 101      | true  | before   |
#        | bean!    | !=  | 100      | false | before   | considered as (bean! !)=
#        | bean!    | !=  | 101      | true  | before   | considered as (bean! !)=

  Rule: higher precedence than comparison logical && || and or

    Scenario Outline: <position> <opt>
      When register DAL:
      """
      dal.getRuntimeContextBuilder().registerExclamation(Bean.class, rd-> rd.data().map(i->Boolean.parseBoolean("<exclamationValue>")));
      """
      When use a instance of java class "BeanRef" to evaluate:
      """
      <operand1> <opt> <operand2>
      """
      Then the result should:
      """
      = <value>
      """
      Examples:
        | operand1 | opt  | operand2 | value | exclamationValue | position |
        | true     | &&   | bean!    | true  | true             | after    |
        | true     | &&   | bean!    | false | false            | after    |
        | true     | and  | bean!    | true  | true             | after    |
        | true     | and  | bean!    | false | false            | after    |
        | false    | and  | bean!    | false | not boolean      | after    |
        | false    | \|\| | bean!    | true  | true             | after    |
        | false    | \|\| | bean!    | false | false            | after    |
        | false    | or   | bean!    | true  | true             | after    |
        | false    | or   | bean!    | false | false            | after    |
        | true     | or   | bean!    | true  | not boolean      | after    |
        | bean!    | &&   | true     | true  | true             | before   |
        | bean!    | &&   | true     | false | false            | before   |
        | bean!    | and  | true     | true  | true             | before   |
        | bean!    | and  | true     | false | false            | before   |
        | bean!    | \|\| | true     | true  | true             | before   |
        | bean!    | \|\| | false    | false | false            | before   |
        | bean!    | or   | true     | true  | true             | before   |
        | bean!    | or   | false    | false | false            | before   |

  Rule: higher precedence than verification = :

    Scenario: before verification = :
      When register DAL:
      """
      dal.getRuntimeContextBuilder().registerExclamation(Bean.class, rd-> rd.data().map(i->99));
      """
      When use a instance of java class "BeanRef" to evaluate:
      """
      bean! = 100
      """
      Then failed with the message:
      """
      Expected to be equal to: java.lang.Integer
      <100>
       ^
      Actual: java.lang.Integer
      <99>
       ^
      """
      When use a instance of java class "BeanRef" to evaluate:
      """
      bean!: 100
      """
      Then failed with the message:
      """
      Expected to match: java.lang.Integer
      <100>
       ^
      Actual: java.lang.Integer
      <99>
       ^
      """

    Scenario: N/A after verification = :
      When use a instance of java class "BeanRef" to evaluate:
      """
      100= bean!
      """
      Then failed with the message:
      """
      Expected to be equal to: java.lang.String
                                         ^
      <bean!>
      Actual: java.lang.Integer
                        ^
      <100>
      """
      When use a instance of java class "BeanRef" to evaluate:
      """
      100: bean!
      """
      Then failed with the message:
      """
      Cannot compare between java.lang.Integer
      <100>
      and java.lang.String
      <bean!>
      """
