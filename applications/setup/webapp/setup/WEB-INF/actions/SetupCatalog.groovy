import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import com.ilscipio.scipio.setup.*;

final module = "SetupCatalog.groovy";

catalogData = context.catalogData ?: [:];

prodCatalog = catalogData.prodCatalog;
context.prodCatalog = prodCatalog;

productStoreCatalog = catalogData.productStoreCatalog;
context.productStoreCatalog = productStoreCatalog;

productStoreCatalogList = catalogData.productStoreCatalogList;
context.productStoreCatalogList = productStoreCatalogList;

highestSequenceNum = null;
if (productStoreCatalogList) {
    for(cat in productStoreCatalogList) {
        if (cat.sequenceNum != null) {
            if (highestSequenceNum == null || cat.sequenceNum > highestSequenceNum) {
                highestSequenceNum = cat.sequenceNum;
            }
        }
    }
}
context.highestSequenceNum = highestSequenceNum;

defaultSequenceNum = (highestSequenceNum != null) ? (highestSequenceNum + 1) : 1;
context.defaultSequenceNum = defaultSequenceNum;

prodCatalogAndStoreAssoc = null
if (prodCatalog != null && productStoreCatalog != null) {
    prodCatalogAndStoreAssoc = new HashMap(prodCatalog);
    prodCatalogAndStoreAssoc.putAll(productStoreCatalog);
}
context.prodCatalogAndStoreAssoc = prodCatalogAndStoreAssoc;