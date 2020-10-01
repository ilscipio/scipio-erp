package com.ilscipio.scipio.solr;

import org.ofbiz.base.util.Debug;
import org.ofbiz.service.DispatchContext;

/**
 * Collects status during SolrProductSearch.updateToSolrCoreMultiAdd and rebuildSolrIndex.
 * Available to {@link IndexingHookHandler}.
 */
public interface IndexingStatus {

    boolean isSuccess();
    boolean isAborted();
    DispatchContext getDctx();
    IndexingHookHandler.HookType getHookType();
    SolrDocBuilder getIndexer();
    int getBufSize();
    int getMaxDocs();
    int getNumDocs();
    int getGeneralFailures();
    int getStartIndex();
    int getEndIndex();
    int getHookFailures();
    int getNumFiltered();
    default int getTotalFailures() { return getGeneralFailures() + getHookFailures(); }
    default String getIndexProgressString() { return getStartIndex() + "-" + getEndIndex() + " / " + getMaxDocs(); }

    class Standard implements IndexingStatus {
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

        private final DispatchContext dctx;
        private final IndexingHookHandler.HookType hookType;
        private final SolrDocBuilder indexer;
        private final String logPrefix;
        private int bufSize;
        private int maxDocs;
        private int numDocs;
        private int generalFailures;
        private int startIndex;
        private int endIndex;
        private int hookFailures;
        private int numFiltered;
        private boolean aborted = false;

        public Standard(DispatchContext dctx, IndexingHookHandler.HookType hookType, SolrDocBuilder indexer, int maxDocs, int bufSize, String logPrefix) {
            this.dctx = dctx;
            this.hookType = hookType;
            this.indexer = indexer;
            this.logPrefix = logPrefix != null ? logPrefix : "";
            this.bufSize = bufSize;
            this.maxDocs = maxDocs;
            this.numDocs = 0;
            this.generalFailures = 0;
            this.startIndex = 1;
            this.endIndex = -1; // calculated on first loop
            this.hookFailures = 0;
            this.numFiltered = 0;
        }

        public boolean isSuccess() {
            return getGeneralFailures() == 0 && getHookFailures() == 0 && !isAborted();
        }

        public boolean isAborted() {
            return aborted;
        }
        public void setAborted(boolean aborted) {
            this.aborted = aborted;
        }

        public DispatchContext getDctx() { return dctx; }
        public IndexingHookHandler.HookType getHookType() {
            return hookType;
        }
        public SolrDocBuilder getIndexer() {
            return indexer;
        }
        public String getLogPrefix() { return logPrefix; }
        public int getBufSize() { return bufSize; }
        public int getMaxDocs() {
            return maxDocs;
        }
        public int getNumDocs() {
            return numDocs;
        }
        public int getGeneralFailures() {
            return generalFailures;
        }
        public int getStartIndex() {
            return startIndex;
        }
        public int getEndIndex() {
            return endIndex;
        }
        public int getHookFailures() {
        return hookFailures;
    }

        public void setBufSize(int bufSize) { this.bufSize = bufSize; }
        public void setMaxDocs(int maxDocs) { this.maxDocs = maxDocs; }
        public void setNumDocs(int numDocs) { this.numDocs = numDocs; }
        public void increaseNumDocs(int amount) { this.numDocs += amount; }
        public void setGeneralFailures(int generalFailures) { this.generalFailures = generalFailures; }
        public void registerGeneralFailure(String msg, Throwable t) {
            this.generalFailures++;
            Debug.logError(t, getLogPrefix() + msg + getErrorLogSuffix(), module);
        }
        public void setStartIndex(int startIndex) { this.startIndex = startIndex; }
        public void increaseStartIndex(int amount) { this.startIndex += amount; }
        public void setEndIndex(int endIndex) { this.endIndex = endIndex; }
        public void setHookFailures(int hookFailures) { this.hookFailures = hookFailures; }
        public void registerHookFailure(String msg, Throwable t, IndexingHookHandler hookHandler, String methodName) {
            this.hookFailures++;
            Debug.logError(t, getLogPrefix()+ "Error in hook handler " + hookHandler + " method [" + methodName + "]" +
                    (msg != null ? ": " + msg : "") + getErrorLogSuffix(), module);
        }
        public void updateStartEndIndex(int docsConsumed) {
            increaseStartIndex(docsConsumed);
            // NOTE: the endIndex is actually a prediction, but if it's ever false, there is a serious DB problem
            if (getBufSize() > 0) {
                setEndIndex(getStartIndex() + Math.min(getBufSize(), getMaxDocs() - (getStartIndex() - 1)) - 1);
            } else {
                setEndIndex(getMaxDocs());
            }
        }
        public String getErrorLogSuffix() { return "; total general errors: " + getGeneralFailures() + "; total hook errors: " + getHookFailures(); }

        public int getNumFiltered() {
            return numFiltered;
        }

        public void increaseNumFiltered(int amount) {
            this.numFiltered += amount;
        }
    }
}
