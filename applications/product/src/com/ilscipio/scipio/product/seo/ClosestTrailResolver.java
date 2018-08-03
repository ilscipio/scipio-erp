package com.ilscipio.scipio.product.seo;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

/**
 * Selects a best trail based on a current hint trail (breadcrumbs).
 * MAINLY for new URL generation - currently not used for the filter incoming URL matching
 * (which is stricter - see {@link SeoCatalogUrlWorker#findBestTrailForUrlPathElems}.
 * <p>
 * This is the core algo that makes browsing complex category rollups (with multiple paths to root) 
 * in the frontend possible (e.g. top cat vs best-sell browsing).
 */
@SuppressWarnings("serial")
public abstract class ClosestTrailResolver implements Serializable {
    
    public enum ResolverType {
        NONCONSEC_ENDSWITH("nonconsec-endswith", new NonConsecutiveEndsWithResolver()),
        NONCONSEC_ANYPART("nonconsec-anypart", new NonConsecutiveAnyPartResolver());
        
        private static final Map<String, ResolverType> nameMap;
        static {
            Map<String, ResolverType> map = new HashMap<>();
            for(ResolverType type : ResolverType.values()) {
                map.put(type.getName(), type);
            }
            nameMap = map;
        }
        
        private final String name;
        private final ClosestTrailResolver resolver;

        private ResolverType(String name, ClosestTrailResolver resolver) {
            this.name = name;
            this.resolver = resolver;
        }

        public String getName() {
            return name;
        }

        public ClosestTrailResolver getResolver() {
            return resolver;
        }
        
        public static ResolverType fromName(String name) {
            return nameMap.get(name);
        }
        
        public static ResolverType fromNameOrDefault(String name, ResolverType defaultValue) {
            ResolverType resolver = nameMap.get(name);
            return (resolver != null) ? resolver : defaultValue;
        }
    }
    
    /**
     * Selects the trail that closest-matches the hintTrail (from the session).
     * Returns null if nothing good enough to make a selection.
     * <p>
     * This means: find the smallest trail that contains the most entries from the hint trail.
     * NOTE: this is DIFFERENT from the inbound match algorithm!! Here, for legacy reasons, we do
     * not require that the trail "ends with" the hint trail, just that it contains the hint trail
     * elems in the same order.
     * <p>
     * NOTE: 2017-11-14: THIS RELIES ON THE SCIPIO ERP-BEHAVIOR GIVEN TO THE SESSION BREADCRUMB TRAIL.
     * In otherwords, this will work poorly or not at all if the entries in the breadcrumb trail don't reflect
     * a real category path, though it could partially work anyway.
     * <p>
     * DEV NOTE: I'm leaving two versions here with strictHint param, because it's hard to say
     * which will be better or worse in most applications.
     * 
     * @param topCategoryIds used to break ties
     */
    public List<String> findClosestTrail(List<List<String>> trails, List<String> hintTrail, Collection<String> topCategoryIds) {
        List<String> bestTrail = null;
        
        if (hintTrail != null && !hintTrail.isEmpty()) {
            if ("TOP".equals(hintTrail.get(0))) hintTrail = hintTrail.subList(1, hintTrail.size());
            if (!hintTrail.isEmpty()) {
                int bestScore = -1;
                for(List<String> trail : trails) {            
                    int score = scoreTrail(trail, hintTrail);
                    
                    if (score > bestScore || (score == bestScore && isFirstTrailBetterThanSecondScoreless(trail, bestTrail, topCategoryIds))) {
                        bestScore = score;
                        bestTrail = trail;
                    }
                }
            }
        }
        
        if (bestTrail == null) {
            bestTrail = getFallbackBestTrail(trails, topCategoryIds);
        }
        return bestTrail;
    }
    
    protected abstract int scoreTrail(List<String> trail, List<String> hintTrail);
    
    /**
     * Breaks score ties.
     * Currently this returns the trail which is the first in the top category IDs.
     * If both same top category, returns smallest trail. 
     */
    protected boolean isFirstTrailBetterThanSecondScoreless(List<String> first, List<String> second, Collection<String> topCategoryIds) {
        String firstTopCatId = first.get(0);
        String secondTopCatId = second.get(0);
        if (firstTopCatId.equals(secondTopCatId)) return (first.size() < second.size());
        
        for(String topCatId : topCategoryIds) {
            if (topCatId.equals(firstTopCatId)) return true;
            else if (topCatId.equals(secondTopCatId)) return false; 
        }
        
        return (first.size() < second.size());
    }
    
    protected List<String> getFallbackBestTrail(List<List<String>> trails, Collection<String> topCategoryIds) {
        return SeoCatalogUrlWorker.getFirstTopTrail(trails, topCategoryIds);
    }
    
    /**
     * Stops as soon as a hint trail element (from the end) is not found in the trail.
     * HOWEVER, the matches don't need to be consecutive (so not as strict as the inbound trail match).
     */
    public static class NonConsecutiveEndsWithResolver extends ClosestTrailResolver {
        @Override
        protected int scoreTrail(List<String> trail, List<String> hintTrail) {
            int score = 0;
            ListIterator<String> hintIt = hintTrail.listIterator(hintTrail.size());
            ListIterator<String> trailIt = trail.listIterator(trail.size());
            String hintCatId = hintIt.previous();
            while(trailIt.hasPrevious()) {
                String catId = trailIt.previous();
                if (catId.equals(hintCatId)) {
                    score++;
                    if (hintIt.hasPrevious()) {
                        hintCatId = hintIt.previous();
                    } else {
                        break;
                    }
                }
            }
            return score;
        }
        
    }
    
    /**
     * This allows to "skip" hint trail elements (from the end) if not found in the trail.
     * Always iterates the entire hint trail.
     * FIXME?: this algo doesn't give higher priority to the last entries in hintTrail.
     */
    public static class NonConsecutiveAnyPartResolver extends ClosestTrailResolver {
        @Override
        protected int scoreTrail(List<String> trail, List<String> hintTrail) {
            int score = 0;
            ListIterator<String> hintIt = hintTrail.listIterator(hintTrail.size());
            int lastTrailMatchIndex = trail.size();
            while(hintIt.hasPrevious()) {
                String hintCatId = hintIt.previous();
                int matchIndex = SeoCatalogUrlWorker.lastIndexOf(trail, hintCatId, lastTrailMatchIndex - 1);
                if (matchIndex > 0) {
                    score++;
                    lastTrailMatchIndex = matchIndex;
                } else if (matchIndex == 0) {
                    score++;
                    break;
                }
            }
            return score;
        }
    }
}