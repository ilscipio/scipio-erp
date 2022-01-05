import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilFormatOut
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.cache.UtilCache
import org.ofbiz.common.uom.UomWorker
import org.apache.commons.math3.util.Precision;

context.hasUtilCacheEdit = security.hasEntityPermission("UTIL_CACHE", "_EDIT", request);

final module = "MemoryInfo.groovy"
final displayMbScale = 2; // SCIPIO

Debug.logInfo("Begin fetching memory info", module);

cacheList = [];
totalCacheMemory = 0.0;
names = new TreeSet(UtilCache.getUtilCacheTableKeySet());
names.each { cacheName ->
    utilCache = UtilCache.findCache(cacheName);
    cache = [:];

    cache.cacheName = utilCache.getName();
    cache.cacheSize = UtilFormatOut.formatQuantity(utilCache.size());
    cache.hitCount = UtilFormatOut.formatQuantity(utilCache.getHitCount());
    cache.missCountTot = UtilFormatOut.formatQuantity(utilCache.getMissCountTotal());
    cache.missCountNotFound = UtilFormatOut.formatQuantity(utilCache.getMissCountNotFound());
    cache.missCountExpired = UtilFormatOut.formatQuantity(utilCache.getMissCountExpired());
    cache.missCountSoftRef = UtilFormatOut.formatQuantity(utilCache.getMissCountSoftRef());
    cache.removeHitCount = UtilFormatOut.formatQuantity(utilCache.getRemoveHitCount());
    cache.removeMissCount = UtilFormatOut.formatQuantity(utilCache.getRemoveMissCount());
    cache.maxInMemory = UtilFormatOut.formatQuantity(utilCache.getMaxInMemory());
    cache.expireTime = UtilFormatOut.formatQuantity(utilCache.getExpireTime());
    cache.useSoftReference = utilCache.getUseSoftReference().toString();
    cache.sizeLimit = utilCache.getSizeLimit(); // SCIPIO: 2.1.0: Added
    cache.cacheMemory = utilCache.getSizeInBytes();

    totalCacheMemory += cache.cacheMemory;
    cacheList.add(cache);
}
sortField = parameters.sortField;
if (sortField) {
    context.cacheList = UtilMisc.sortMaps(cacheList, UtilMisc.toList(sortField));
} else {
    context.cacheList = cacheList;
}
context.totalCacheMemory = totalCacheMemory;

rt = Runtime.getRuntime();
memoryInfo = [:];
maxMemoryMB = ((rt.maxMemory() / 1024) / 1024);
context.maxMemoryMB = maxMemoryMB;

totalMemoryMB = ((rt.totalMemory() / 1024) / 1024);
context.totalMemoryMB = totalMemoryMB;

freeMemoryMB = ((rt.freeMemory() / 1024) / 1024);
memoryInfo.put("Free Memory", Precision.round(freeMemoryMB, displayMbScale)); // SCIPIO: invalid for @chartdata, don't pass string: UtilFormatOut.formatQuantity(freeMemoryMB)

cachedMemoryMB = ((totalCacheMemory / 1024) / 1024);
memoryInfo.put("Cache Memory", Precision.round(cachedMemoryMB, displayMbScale)); // SCIPIO: invalid for @chartdata, don't pass string: UtilFormatOut.formatQuantity(cachedMemoryMB)

usedMemoryMB = ((((rt.totalMemory() - rt.freeMemory()) - totalCacheMemory)  / 1024) / 1024);
memoryInfo.put("Used Memory without cache", Precision.round(usedMemoryMB, displayMbScale)); // SCIPIO: invalid for @chartdata, don't pass string: UtilFormatOut.formatQuantity(usedMemoryMB)

context.memoryInfo = memoryInfo;

Debug.logInfo("Finished fetching memory info", module);
