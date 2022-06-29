Feature: flatten-data

  Scenario: flatten-data property should map back to parent data
    Given the following java class:
    """
    public class User implements com.github.leeonky.dal.runtime.PartialObject {
      public String name;
      public int age;
    }
    """
    And the following java class:
    """
    public class Order {
      public String userName = "Tom";
      public int userAge = 18;
      public String id = "001";

      public static User user(Order order) {
        User user = new User();
        user.name = order.userName;
        user.age = order.userAge;
        return user;
      }
    }
    """
    Then the following verification for the instance of java class "Order" should pass:
    """
    = {
      id= '001'
      user= {
        name= Tom
        age= 18
      }
    }
    """
    And the following verification for the instance of java class "Order" should pass:
    """
    = {
      id= '001'
      user.name= Tom
      user.age= 18
    }
    """
    And the following verification for the instance of java class "Order" should pass:
    """
      id= '001',
      user.name= Tom,
      user.age= 18
    """

  Scenario: nested flatten-data property
    Given the following java class:
    """
    public class User implements com.github.leeonky.dal.runtime.PartialObject {
      public String name;
      public int age;
    }
    """
    And the following java class:
    """
    public class Order implements com.github.leeonky.dal.runtime.PartialObject {
      public String userName;
      public int userAge;
      public String id;

      public static User user(Order order) {
        User user = new User();
        user.name = order.userName;
        user.age = order.userAge;
        return user;
      }
    }
    """
    And the following java class:
    """
    public class Store implements com.github.leeonky.dal.runtime.PartialObject {
      public String orderUserName;
      public int orderUserAge;
      public String orderId;
      public String id;

      public static Order order(Store store) {
        Order order = new Order();
        order.id = store.orderId;
        order.userName = store.orderUserName;
        order.userAge = store.orderUserAge;
        return order;
      }
    }
    """
    And the following java class:
    """
    public class Site {
      public String storeOrderUserName = "Tom";
      public int storeOrderUserAge = 18;
      public String storeOrderId = "001";
      public String storeId = "s001";
      public String id = "site1";

      public static Store store(Site site) {
        Store store = new Store();
        store.id = site.storeId;
        store.orderId = site.storeOrderId;
        store.orderUserName = site.storeOrderUserName;
        store.orderUserAge = site.storeOrderUserAge;
        return store;
      }
    }
    """
    Then the following verification for the instance of java class "Site" should pass:
    """
    = {
      id= site1
      store= {
        id= s001
        order= {
          id= '001'
          user= {
            name= Tom
            age= 18
          }
        }
      }
    }
    """
    And the following verification for the instance of java class "Site" should pass:
    """
    = {
      id= site1,
      store.id= s001,
      store.order.id= '001',
      store.order.user.name= Tom,
      store.order.user.age= 18
    }
    """

  Scenario: root object is flatten-data
    Given the following java class:
    """
    public class User implements com.github.leeonky.dal.runtime.PartialObject {
      public String name;
      public int age;
    }
    """
    And the following java class:
    """
    public class Order implements com.github.leeonky.dal.runtime.PartialObject {
      public String userName = "Tom";
      public int userAge = 18;
      public String id = "order1";

      public static User user(Order order) {
        User user = new User();
        user.name = order.userName;
        user.age = order.userAge;
        return user;
      }
    }
    """
    Then the following verification for the instance of java class "Order" should pass:
    """
    id= order1,
    user.name= Tom,
    user.age= 18
    """
    Then the following verification for the instance of java class "Order" should pass:
    """
    id= order1,
    user= {
      name= Tom
      age= 18
    }
    """
