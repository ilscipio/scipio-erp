package com.ilscipio.scipio.cms.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelKeyMap;
import org.ofbiz.entity.model.ModelReader;
import org.ofbiz.entity.model.ModelRelation;

import com.ilscipio.scipio.ce.build.util.DependencyGraph;

/**
 * SCIPIO: Entity meta-information helper util.
 * <p>
 * Added 2017-05-31, originally to help with CMS entities (but these are generic
 * and should NOT contain CMS-specific code).
 * <p>
 * TODO: move this class outside CMS
 */
public abstract class EntityInfoUtil {

    public static final String module = EntityInfoUtil.class.getName();
    
    public static final String DEFAULT_DELEGATOR_NAME = "default";
    
    protected EntityInfoUtil() {
    }

    /**
     * Returns the field names of the entity that are foreign keys to the specified relEntityName.
     * Only useful if relEntityName is entity with single-field PK.
     */
    public static Set<String> getRelationFieldNames(ModelEntity modelEntity, String relEntityName, 
            boolean includeOne, boolean includeOneNoFk, boolean includeMany) {
        if (modelEntity == null) return null;
        Set<String> fieldNames = new LinkedHashSet<>(); // NOTE: using LinkedHashSet so order is more predictable...
        for(ModelRelation relation : modelEntity.getRelationsList(includeOne, includeOneNoFk, includeMany)) {
            if (relEntityName.equals(relation.getRelEntityName())) {
                for(ModelKeyMap keyMap : relation.getKeyMaps()) {
                    fieldNames.add(keyMap.getFieldName());
                }
            }
        }
        return fieldNames;
    }
    
    /**
     * Returns the ModelRelations of the entity that are foreign keys to the specified relEntityName.
     */
    public static List<ModelRelation> findModelRelationsByRelEntityName(ModelEntity modelEntity, String relEntityName, 
            boolean includeOne, boolean includeOneNoFk, boolean includeMany) {
        if (modelEntity == null) return null;
        List<ModelRelation> modelRelations = new ArrayList<>(); // NOTE: using LinkedHashSet so order is more predictable...
        for(ModelRelation relation : modelEntity.getRelationsList(includeOne, includeOneNoFk, includeMany)) {
            if (relEntityName.equals(relation.getRelEntityName())) {
                modelRelations.add(relation);
            }
        }
        return modelRelations;
    }

    public static Map<String, List<String>> makeEntityNameDependencyMap(Set<ModelEntity> modelEntities, Set<String> entityNames) {
        Map<String, List<String>> depMap = new LinkedHashMap<>();
        for(ModelEntity modelEntity : modelEntities) {
            String name = modelEntity.getEntityName();
            Set<String> deps = new LinkedHashSet<>(); // eliminate duplicates
            // TODO: REVIEW: WE ARE EXCLUDING one-nofk RELATIONS FOR NOW
            // they are typically used to resolve circular deps, so we risk creating circular deps by including...
            for(ModelRelation relation : modelEntity.getRelationsList(true, false, false)) {
                String relEntityName = relation.getRelEntityName();
                if (entityNames.contains(relEntityName)) {
                    deps.add(relEntityName);
                }
            }
            depMap.put(name, new ArrayList<>(deps));
        }
        return depMap;
    }

    public static StringBuilder printEntityNameDependencyMap(Map<String, List<String>> depMap) {
        StringBuilder sb = new StringBuilder();
        for(Map.Entry<String, List<String>> entry : depMap.entrySet()) {
            sb.append(entry.getKey());
            sb.append(" -> ");
            sb.append(entry.getValue().toString());
            sb.append("\n");
        }
        return sb;
    }

    public static DependencyGraph<String> makeEntityNameDependencyGraph(Set<ModelEntity> modelEntities, Set<String> entityNames) {
        return new DependencyGraph<>(makeEntityNameDependencyMap(modelEntities, entityNames));
    }

    public static List<String> makeEntityNameDependencyOrderFromDepMap(Map<String, List<String>> depMap) throws IllegalArgumentException {
        return new DependencyGraph<>(depMap).getResolvedDependenciesDfs();
    }
    
    /**
     * Returns a list of entity names ordered by resolved dependencies based on relations, with least dependent
     * first and most dependent last. Only considers the entities within the passed set and ignores the relations
     * to entities outside of it.
     * NOTE: entityNames is an optimization, should match modelEntities.
     */
    public static List<String> makeEntityNameDependencyOrder(Set<ModelEntity> modelEntities, Set<String> entityNames, boolean logVerbose,
            String logPrefix, String logErrorSuffix) throws IllegalArgumentException {
        Map<String, List<String>> depMap = makeEntityNameDependencyMap(modelEntities, entityNames);
        if (logVerbose) {
            Debug.logInfo(logPrefix + "EntityInfo: Entity dependency graph, pre-processed:\n" + EntityInfoUtil.printEntityNameDependencyMap(depMap), module);
            List<String> result = new DependencyGraph<>(depMap).getResolvedDependenciesDfs(); // this may throw exception
            Debug.logInfo(logPrefix + "EntityInfo: Entity dependency resolved order:\n" + result.toString(), module);
            return result;
        } else {
            return new DependencyGraph<>(depMap).getResolvedDependenciesDfs();
        }
    }
    
    /**
     * Returns a list of entity names ordered by resolved dependencies based on relations, with least dependent
     * first and most dependent last. Only considers the entities within the passed set and ignores the relations
     * to entities outside of it.
     * NOTE: entityNames is an optimization, should match modelEntities.
     */
    public static List<String> makeEntityNameDependencyOrder(Set<ModelEntity> modelEntities, Set<String> entityNames) throws IllegalArgumentException {
        Map<String, List<String>> depMap = makeEntityNameDependencyMap(modelEntities, entityNames);
        return new DependencyGraph<>(depMap).getResolvedDependenciesDfs();
    }

    public static <T extends Collection<String>> T getEntityNames(Set<ModelEntity> modelEntities, T outEntityNames) {
        for(ModelEntity modelEntity : modelEntities) {
            outEntityNames.add(modelEntity.getEntityName());
        }
        return outEntityNames;
    }

    public static <T extends Collection<ModelEntity>> T getModelEntities(ModelReader reader, Collection<String> entityNames, T outModelEntities) throws GenericEntityException {
        for(String name : entityNames) {
            outModelEntities.add(reader.getModelEntity(name));
        }
        return outModelEntities;
    }
    
    public static <T extends Collection<ModelEntity>> T getModelEntitiesSafe(ModelReader reader, Collection<String> entityNames, T outModelEntities) {
        for(String name : entityNames) {
            try {
                outModelEntities.add(reader.getModelEntity(name));
            } catch(GenericEntityException e) {
                Debug.logError(e, module);
            }
        }
        return outModelEntities;
    }
    

    /**
     * Checks if entity package starts with any one of given prefixes.
     * If no prefixes passed, returns true. If prefixes passed but entity has no package name, returns false.
     */
    public static boolean hasPackagePrefix(ModelEntity modelEntity, Collection<String> allowedPkgPrefixes) {
        if (allowedPkgPrefixes == null || allowedPkgPrefixes.isEmpty()) return true;
        String pkg = modelEntity.getPackageName();
        if (pkg == null || pkg.isEmpty()) return false;
        for(String prefix : allowedPkgPrefixes) {
            if (pkg.startsWith(prefix)) return true;
        }
        return false;
    }
    
    /**
     * For "one.two.three", generates: ["one", "one.two", "one.two.three"].
     */
    public static String[] generateSubPackageNames(String pkgName) {
        String[] parts = StringUtils.split(pkgName, '.');
        if (parts.length >= 2) {
            StringBuilder sb = new StringBuilder(parts[0]);
            for(int i=1; i < parts.length; i++) {
                sb.append('.');
                sb.append(parts[i]);
                parts[i] = sb.toString();
            }
        }
        return parts;
    }
    
    public static String getSinglePkFieldNameStrict(Delegator delegator, String entityName) throws IllegalArgumentException {
        ModelReader reader = delegator.getModelReader();
        ModelEntity modelEntity;
        try {
            modelEntity = reader.getModelEntity(entityName);
        } catch (GenericEntityException e) {
            throw new IllegalArgumentException(e);
        }
        return getSinglePkFieldNameStrict(modelEntity);
    }
    
    public static String getSinglePkFieldNameStrict(ModelEntity modelEntity) throws IllegalArgumentException {
        List<String> pkNames = modelEntity.getPkFieldNames();
        if (pkNames.size() != 1) {
            throw new IllegalArgumentException("Entity " + modelEntity.getEntityName() + " has multiple PK fields; this function only supports single field PK");
        }
        return pkNames.get(0);
    }
    
    public static Delegator ensureDelegator(Delegator delegator) throws IllegalArgumentException {
        return delegator != null ? delegator : getDefaultDelegatorAlways();
    }
    
    /**
     * Returns the default delegator, sometimes needed in static field init (but avoid elsewhere).
     */
    public static Delegator getDefaultDelegatorAlways() throws IllegalArgumentException {
        Delegator delegator = DelegatorFactory.getDelegator(DEFAULT_DELEGATOR_NAME);
        if (delegator == null) throw new IllegalArgumentException("Could not get default delegator (\"" + DEFAULT_DELEGATOR_NAME + "\")");
        return delegator;
    }
    
    /**
     * Returns the default model reader, sometimes needed in static field init (but avoid elsewhere).
     */
    public static ModelReader getDefaultModelReaderAlways() throws IllegalArgumentException {
        try {
            return ModelReader.getModelReader(DEFAULT_DELEGATOR_NAME);
        } catch (GenericEntityException e) {
            throw new IllegalArgumentException(e);
        }
    }
    
    public static ModelEntity getModelEntityAlways(ModelReader reader, String entityName) throws IllegalArgumentException {
        try {
            return reader.getModelEntity(entityName);
        } catch (GenericEntityException e) {
            throw new IllegalArgumentException(e);
        }
    }
    
    @Deprecated
    public static ModelEntity getModelEntityAlways(String entityName) throws IllegalArgumentException {
        return getModelEntityAlways(getDefaultModelReaderAlways(), entityName);
    }
    
}
