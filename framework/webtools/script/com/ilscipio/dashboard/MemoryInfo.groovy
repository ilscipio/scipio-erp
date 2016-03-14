import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilFormatOut
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.cache.UtilCache
import org.ofbiz.common.uom.UomWorker

context.hasUtilCacheEdit = security.hasEntityPermission("UTIL_CACHE", "_EDIT", session);

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
    cache.useFileSystemStore = utilCache.getUseFileSystemStore().toString();
    cache.useFileSystemStore = utilCache.getUseFileSystemStore().toString();
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
//maxMemoryMB = ((rt.maxMemory() / 1024) / 1024);
//memoryInfo.put("Max Memory", maxMemoryMB);
//Debug.log("Max memory ==========> " + maxMemoryMB + " MB");
//
totalMemoryMB = ((rt.totalMemory() / 1024) / 1024);
//memoryInfo.put("Total Memory", totalMemoryMB);
Debug.log("Total memory ==========> " + totalMemoryMB + " MB");

freeMemoryMB = ((rt.freeMemory() / 1024) / 1024);
memoryInfo.put("Free Memory", UtilFormatOut.formatQuantity(freeMemoryMB));
Debug.log("Free memory ==========> " + freeMemoryMB + " MB");

cachedMemoryMB = ((totalCacheMemory / 1024) / 1024);
memoryInfo.put("Cache Memory", UtilFormatOut.formatQuantity(cachedMemoryMB));
Debug.log("Cached memory ==========> " + cachedMemoryMB);

usedMemoryMB = ((((rt.totalMemory() - rt.freeMemory()) - totalCacheMemory)  / 1024) / 1024);
memoryInfo.put("Used Memory without cache", UtilFormatOut.formatQuantity(usedMemoryMB));
Debug.log("Used memory ==========> " + usedMemoryMB + " MB");




context.memoryInfo = memoryInfo;