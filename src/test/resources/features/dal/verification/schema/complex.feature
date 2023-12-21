Feature: multiple schema and list schema

  Background:
    Given the following schema class:
    """
    public class IdZero implements Schema {
        public int id = 0;
    }
    """
    And the following schema class:
    """
    public class IdOne implements Schema {
        public int id = 1;
    }
    """

  Scenario: verify data matches multiple schema
    And the following json:
    """
      {
        "id": 0
      }
    """
    Then the following verification should pass:
    """
      is IdZero / IdZero/IdZero
    """
    And the inspect should:
    """
    is IdZero / IdZero / IdZero
    """

  Scenario: raise error when any schema failed
    When the following json:
    """
      {
        "id": 0
      }
    """
    When evaluate by:
    """
      is IdZero / IdOne
    """
    Then failed with the message:
    """
    Expected to match schema `IdOne` but was not
        Expected field `.id` to be java.lang.Integer
        <1>
        Actual: java.lang.Integer
        <0>
    """
    And got the following notation:
    """
      is IdZero / IdOne
                  ^
    """
    When evaluate by:
    """
      is IdOne / IdZero
    """
    Then failed with the message:
    """
    Expected to match schema `IdOne` but was not
        Expected field `.id` to be java.lang.Integer
        <1>
        Actual: java.lang.Integer
        <0>
    """
    And got the following notation:
    """
      is IdOne / IdZero
         ^
    """

  Scenario: verify list data matches list schema
    Given the following json:
    """
      [{
        "id": 0
      }, {
        "id": 0
      }]
    """
    Then the following verification should pass:
    """
      is [IdZero]
    """
    And the following verification should pass:
    """
      is [IdZero/ IdZero]
    """
    And the inspect should:
    """
    is [IdZero / IdZero]
    """

  Scenario: verify list data matches with not exist schema
    Given the following json:
    """
      [1]
    """
    When evaluate by:
    """
      is [NotExist]
    """
    Then failed with the message:
    """
    Schema 'NotExist' not registered
    """

  Scenario: raise error when one element is not matched schema
    Given the following json:
    """
      [{
        "id": 0
      }, {
        "id": 1
      }]
    """
    When evaluate by:
    """
    is [IdZero]
    """
    Then failed with the message:
    """
    Expected [1] to match schema `IdZero` but was not
        Expected field `.id` to be java.lang.Integer
        <0>
        Actual: java.lang.Integer
        <1>
    """
    And got the following notation:
    """
    is [IdZero]
        ^
    """

  Scenario: should raise error when input is not list
    When evaluate by:
    """
      1 is [IdZero]
    """
    Then failed with the message:
    """
    Invalid input value, expect a List but: java.lang.Integer
    <1>
    """
    And got the following notation:
    """
      1 is [IdZero]
      ^
    """

  Scenario: should inherit first index of list
    Given the following java class:
    """
    public class Bean {
      public final String id;
      public Bean(String v) {
        this.id=v;
      }
    }
    """
    Given the following java class:
    """
    public class BeanList extends ArrayList<Bean> {
      public BeanList() {
        add(new Bean("a"));
        add(new Bean("b"));
        add(new Bean("c"));
      }
    }
    """
    And register the following BeanDALCollectionFactory for java class "BeanList":
    """
    public class BeanDALCollectionFactory implements DALCollectionFactory<BeanList, Bean> {
      public DALCollection<Bean> create(BeanList list) {
        return new CollectionDALCollection<Bean>(list) {
          public int firstIndex() {
            return 1;
          }
        };
      }
    }
    """
    And the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id")
    })
    public class BeanSchema implements Schema{
    }
    """
    Then the following verification for the instance of java class "BeanList" should pass:
    """
    ({} is [BeanSchema])[1].id= a
    """
