Feature: bug

  Rule: starts with 'is' in object verification key

    Scenario: start with notation error when start withs key words 1
      Given the following json:
      """
      {
      "is": "str_is",
      "order": "str_order",
      "iso": "str_iso",
      "whichA": "which_str"
      }
      """
      Then the following verification should pass:
      """
      = {
      iso= str_iso
      'is'= str_is
      order= str_order
      whichA= which_str
      }
      """

    Scenario: start with notation error when start withs key words 2
      Given the following json:
      """
      {
      "a": true,
      "andB": false,
      "B": false
      }
      """
      Then the following verification should pass:
      """
      : {
      a= true andB= false
      }
      """

  Rule: cannot invoke method in expectation in object verification

    Scenario: in the middle of object
      Given the following json:
      """
      {
        "name": "tom",
        "id": "001"
      }
      """
      Then the following verification should pass:
      """
      : {
        name: 'tom'.toString.toString
        id: '001'
      }
      """

    Scenario: in table
      Given the following json:
      """
      [{
        "name": "tom",
        "id": "001"
      }]
      """
      Then the following verification should pass:
      """
      : | name                   | id    |
        |'tom'.toString.toString | '001' |
      """

  Rule: call instance method of anonymous class

    Scenario: call method of lambda
      Given the following java class:
      """
      public interface Acc {
        int inc(int v);
      }
      """
      Given the following java class:
      """
      public class BeanObject {
        public Acc acc = i-> i+1;
      }
      """
      Then the following verification for the instance of java class "BeanObject" should pass:
      """
      acc.inc[100]= 101
      """
