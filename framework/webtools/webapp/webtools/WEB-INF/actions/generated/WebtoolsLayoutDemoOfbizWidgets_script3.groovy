import org.ofbiz.base.util.*;
                    productStores = select("productStoreId").from("ProductStore").orderBy("productStoreId").cache(true).queryList().collect { it.productStoreId };
                    Debug.logInfo("Product stores in system (cached): " + productStores, "InlineDemoScript.groovy");