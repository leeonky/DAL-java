Feature: define field alias in schema

  Scenario: define field alias in schema
    Given the following schema:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id")
    })
    public class Order {
    }
    """
    When the following input data:
    """
      {
        "id": 0
      }
    """
    Then the following assertion should pass:
    """
      is Order which .aliasOfId = 0
    """

  Scenario: recursive alias
    Given the following schema:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id"),
            @FieldAlias(alias = "aliasOfAliasId", field = "aliasOfId")
    })
    public class Order {
    }
    """
    When the following input data:
    """
      {
        "id": 0
      }
    """
    Then the following assertion should pass:
    """
      is Order which .aliasOfAliasId = 0
    """

  Scenario: alias of list element accessing
    Given the following schema:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "firstLine", field = "lines[0]"),
    })
    public class Order {
    }
    """
    When the following input data:
    """
      {
        "lines": [{
          "amount": 100
        }]
      }
    """
    Then the following assertion should pass:
    """
      is Order which .firstLine.amount = 100
    """

  Scenario: alias of field chain
    Given the following schema:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "userName", field = "user.name")
    })
    public class Order {
    }
    """
    When the following input data:
    """
      {
        "user": {
          "name": "Tom"
        }
      }
    """
    Then the following assertion should pass:
    """
      is Order which .userName = 'Tom'
    """

  Scenario: alias in sub schema
    Given the following schema:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfAge", field = "age"),
    })
    public class User {
    }
    """
    And the following schema:
    """
    @Partial
    public class Order {
        public User user;
    }
    """
    When the following input data:
    """
      {
        "user": {
          "age": 18
        }
      }
    """
    Then the following assertion should pass:
    """
      is Order which .user.aliasOfAge = 18
    """

  Scenario: alias in object judgement
    Given the following schema:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfId", field = "id")
    })
    public class Order {
    }
    """
    When the following input data:
    """
      {
        "id": 1
      }
    """
    Then the following assertion should pass:
    """
      is Order which :{
        aliasOfId: 1
      }
    """
    And the following assertion should pass:
    """
      is Order which ={
        aliasOfId: 1
      }
    """

  Scenario: alias in nested object judgement
    Given the following schema:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfAge", field = "age"),
    })
    public class User {
    }
    """
    And the following schema:
    """
    public class Order {
        public User user;
    }
    """
    When the following input data:
    """
      {
        "user": {
          "age": 10
        }
      }
    """
    Then the following assertion should pass:
    """
      is Order which :{
        user: {
          aliasOfAge: 10
        }
      }
    """
    And the following assertion should pass:
    """
      is Order which :{
        user= {
          aliasOfAge: 10
        }
      }
    """

  Scenario: alias in list judgement
    Given the following schema:
    """
    public class Order {
      public List<Product> products;
    }
    """
    And the following schema:
    """
    @FieldAliases({
            @FieldAlias(alias = "aliasOfName", field = "name"),
    })
    public class Product {
        public String name;
    }
    """
    When the following input data:
    """
      {
        "products": [{
          "name": "ipad"
        }, {
          "name": "iphone"
        }]
      }
    """
    Then the following assertion should pass:
    """
      is Order which :{
        products: [
          {aliasOfName: 'ipad'}
          {aliasOfName: 'iphone'}
        ]
      }
    """

  Scenario: alias in nested list judgement
    Given the following schema:
    """
    public class Image {
      public List<List<Pixel>> pixels;
    }
    """
    And the following schema:
    """
    @FieldAliases({
            @FieldAlias(alias = "aliasOfColor", field = "color"),
    })
    public class Pixel {
      public String color;
    }
    """
    When the following input data:
    """
      {
        "pixels": [
          [{"color": "red"}],
          [{"color": "black"}]
        ]
      }
    """
    Then the following assertion should pass:
    """
      is Image which :{
        pixels: [
          [{aliasOfColor: 'red'}],
          [{aliasOfColor: 'black'}]
        ]
      }
    """

  Scenario: alias in list mapping
    Given the following schema:
    """
    public class Order {
      public List<Product> products;
    }
    """
    And the following schema:
    """
    public class Product {
        public Catalog catalog;
    }
    """
    And the following schema:
    """
    @FieldAliases({
            @FieldAlias(alias = "aliasOfName", field = "name"),
            @FieldAlias(alias = "aliasOfDescription", field = "description"),
    })
    @Partial
    public class Catalog {
    }
    """
    When the following input data:
    """
      {
        "products": [{
          "catalog": {
            "name": "c1",
            "description": "catalog c1"
          }
        }, {
          "catalog": {
            "name": "c2",
            "description": "catalog c1"
          }
        }]
      }
    """
    Then the following assertion should pass:
    """
      is Order which :{
        products.catalog: [
          {aliasOfName: 'c1'}
          {aliasOfName: 'c2'}
        ]
        products.@.catalog: [
          {aliasOfDescription: 'catalog c1'}
          {aliasOfDescription: 'catalog c1'}
        ]
      }
    """
