import org.ofbiz.entity.util.*;

catalogData = context.catalogData ?: [:];

prodCatalog = catalogData.prodCatalog;
context.prodCatalog = prodCatalog;

productStoreCatalog = catalogData.productStoreCatalog;
context.productStoreCatalog = productStoreCatalog;

