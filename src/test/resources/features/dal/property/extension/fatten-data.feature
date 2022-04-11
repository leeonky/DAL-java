Feature: flatten-data

  Scenario: flatten-data property should map back to parent data
    Given the following java class:
    """
    public class User implements com.github.leeonky.dal.runtime.Flatten {
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
#    Then the following verification for the instance of java class "Order" should pass:
#    """
#    = {
#      id= '001'
#      user= {
#        name= Tom
#        age= 18
#      }
#    }
#    """
    And the following verification for the instance of java class "Order" should pass:
    """
    = {
      id= '001'
      user.name= Tom
      user.age= 18
    }
    """

  Scenario: nested flatten-data property
    Given the following java class:
    """
    public class User implements com.github.leeonky.dal.runtime.Flatten {
      public String name;
      public int age;
    }
    """
    And the following java class:
    """
    public class Order implements com.github.leeonky.dal.runtime.Flatten {
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
    public class Store {
      public String orderUserName = "Tom";
      public int orderUserAge = 18;
      public String orderId = "001";
      public String id = "s001";

      public static Order order(Store store) {
        Order order = new Order();
        order.id = store.orderId;
        order.userName = store.orderUserName;
        order.userAge = store.orderUserAge;
        return order;
      }
    }
    """
#    Then the following verification for the instance of java class "Store" should pass:
#    """
#    = {
#      id= s001
#      order= {
#        id= '001'
#        user= {
#          name= Tom
#          age= 18
#        }
#      }
#    }
#    """
    And the following verification for the instance of java class "Store" should pass:
    """
    = {
      id= s001,
      order.id= '001',
      order.user.name= Tom,
      order.user.age= 18
    }
    """

#
##  a.b.c
##  a.d.c
##  {
##    a: {
##      b: {
##        c:  1
##      }
##      d: {
##        c:  1
##      }
##    }
##  }

#  TODO root object in flatten data