Feature: row header

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
      public BeanRef(Bean b){
        this.bean = b;
      }
    }
    """
    Given the following java class:
    """
    public class Beans {
      public Bean hello = new Bean("hello");
      public List<Bean> beans = Arrays.asList(new Bean("b1"), new Bean("b2"));
    }
    """

  Scenario: support data remark and exclamation and schema in row header

    When register DAL:
    """
    dal.getRuntimeContextBuilder().registerDataRemark(Bean.class, rd-> rd.data().map(i->((Bean)i).value+rd.remark()));
    dal.getRuntimeContextBuilder().registerExclamation(String.class, rd-> rd.data().map(s->((String)s).toUpperCase()));
    """
    Then the following verification for the instance of java class "Beans" should pass:
    """
    : {
      beans:                | trim      |
        (remark1) is String | b1remark1 |
        (remark2) is String | b2remark2 |

      beans:       | trim      |
        (remark1)! | B1REMARK1 |
        (remark2)! | B2REMARK2 |

      beans:                 | trim      |
        (remark1)! is String | B1REMARK1 |
        (remark2)! is String | B2REMARK2 |
    }
    """

  Scenario: support data remark and exclamation and schema in row header 2
    When register DAL:
    """
    dal.getRuntimeContextBuilder().registerExclamation(String.class, rd-> rd.data().map(s->((String)s).toUpperCase()));
    dal.getRuntimeContextBuilder().registerDataRemark(String.class, rd-> rd.data().map(i->i+rd.remark()));
    """
    Then the following verification for the instance of java class "Beans" should pass:
    """
    : {
      beans.value[]: | trim |
         ! is String | B1   |
         ! is String | B2   |

      beans.value[]: | trim      |
          !(remark1) | B1remark1 |
          !(remark2) | B2remark2 |

      beans.value[]:  | trim      |
          !(remark1)! | B1REMARK1 |
          !(remark2)! | B2REMARK2 |

      beans.value[]:         | trim      |
        !(remark1) is String | B1remark1 |
        !(remark2) is String | B2remark2 |

      beans.value[]:                | trim           |
        (remark1)!(hello) is String | B1REMARK1hello |
        (remark2)!(world) is String | B2REMARK2world |
    }
    """
