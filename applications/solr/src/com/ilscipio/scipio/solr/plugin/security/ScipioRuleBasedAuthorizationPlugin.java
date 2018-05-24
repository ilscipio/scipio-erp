package com.ilscipio.scipio.solr.plugin.security;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.solr.security.RuleBasedAuthorizationPlugin;
import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityExpr;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ScipioRuleBasedAuthorizationPlugin extends RuleBasedAuthorizationPlugin {
    private static final Logger log = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected String entityDelegatorName = "default";

    private boolean multitenant = EntityUtil.isMultiTenantEnabled();

    @SuppressWarnings("unchecked")
    @Override
    public void init(Map<String, Object> initInfo) {
        String entityDelegatorName = (String) initInfo.get("entityDelegatorName");
        if (entityDelegatorName != null && !entityDelegatorName.isEmpty()) {
            this.entityDelegatorName = entityDelegatorName;
        }
        Object multitenant = initInfo.get("multitenant");
        if (multitenant instanceof Boolean) {
            this.multitenant = (Boolean) multitenant;
        }

        Map<String, Object> scipioPermSolrRoles = (Map<String, Object>) initInfo.get("scipioPermSolrRoles");
        if (scipioPermSolrRoles != null && scipioPermSolrRoles.size() > 0) {
            Map<String, Set<String>> allUserIdPerms = getPermsForAllUserLoginIds(scipioPermSolrRoles.keySet());
            if (!allUserIdPerms.isEmpty()) {
                log.debug("Solr: authorization: security.json: auto-generating new solr user-roles"
                        + " from UserLogin SecurityPermissions:\n{}\nand scipioPermSolrRoles mappings:\n{}", allUserIdPerms, scipioPermSolrRoles);

                Map<String, Object> newInitInfo = new LinkedHashMap<>(initInfo);
                Map<String, Object> userRoles = (Map<String, Object>) newInitInfo.get("user-role");
                if (userRoles == null) {
                    userRoles = new LinkedHashMap<>();
                } else {
                    userRoles = new LinkedHashMap<>(userRoles);
                }
                newInitInfo.put("user-role", userRoles);

                for(Map.Entry<String, Set<String>> entry : allUserIdPerms.entrySet()) {
                    String user = entry.getKey();
                    Set<String> newRoles = expandRoles(entry.getValue(), scipioPermSolrRoles);
                    if (!newRoles.isEmpty()) {
                        Set<String> combinedRoles = new LinkedHashSet<>();

                        Object oldRoles = userRoles.get(user);
                        if (oldRoles instanceof String) {
                            combinedRoles.add((String) oldRoles);
                        } else if (oldRoles instanceof Collection) {
                            combinedRoles.addAll((Collection<String>) oldRoles);
                        }

                        combinedRoles.addAll(newRoles);
                        userRoles.put(user, new ArrayList<>(combinedRoles));
                    }
                }

                initInfo = newInitInfo;
            }
        }

        log.info("Solr: authorization: security.json: initializing with solr user-roles: {}", initInfo.get("user-role"));

        super.init(initInfo);
    }

    protected static Set<String> expandRoles(Collection<String> perms, Map<String, Object> scipioPermSolrRoles) {
        if (perms == null) return null;
        Set<String> set = new LinkedHashSet<>();
        for(String perm : perms) {
            Object rolesObj = scipioPermSolrRoles.get(perm);
            if (rolesObj instanceof String) {
                set.add((String) rolesObj);
            } else if (rolesObj instanceof Collection) {
                for(Object obj : (Collection<?>) rolesObj) {
                    if (obj instanceof String) {
                        set.add((String) obj);
                    }
                }
            }
        }
        return set;
    }

    protected Map<String, Set<String>> getPermsForAllUserLoginIds(Collection<String> perms) {
        ClassLoader solrClassLoader = Thread.currentThread().getContextClassLoader();
        ClassLoader ofbizClassLoader = ScipioUserLoginAuthPlugin.findParentNonWebappClassLoader(solrClassLoader);
        Thread.currentThread().setContextClassLoader(ofbizClassLoader);
        try {
            return getPermsForAllUserLoginIdsCore(perms);
        } finally {
            Thread.currentThread().setContextClassLoader(solrClassLoader);
        }
    }

    protected Map<String, Set<String>> getPermsForAllUserLoginIdsCore(Collection<String> perms) {
        Map<String, Set<String>> allUserIdPerms = new HashMap<>();

        Delegator delegator = DelegatorFactory.getDelegator(entityDelegatorName);
        if (delegator == null) {
            Debug.logError("Solr: authorization: Could not get delegator '" + entityDelegatorName
                    + "'; no scipio perms can be mapped to solr roles", module);
            return allUserIdPerms;
        }

        getPermsForAllUserLoginIdsCore(delegator, perms, allUserIdPerms);

        if (multitenant) {
            List<GenericValue> activeTenants;
            try {
                activeTenants = getActiveTenants(delegator);
            } catch(Exception e) {
                Debug.logError(e, "Solr: authorization: Could not read active tenants: " + e.getMessage(), module);
                return allUserIdPerms;
            }
            for(GenericValue tenant : activeTenants) {
                String tenantDelegName = delegator.getDelegatorBaseName() + "#" + tenant.getString("tenantId");
                Delegator tenantDelegator = DelegatorFactory.getDelegator(tenantDelegName);
                if (tenantDelegator == null) {
                    Debug.logError("Solr: authorization: Could not get tenant delegator '" + tenantDelegName + "'", module);
                    continue;
                }
                getPermsForAllUserLoginIdsCore(tenantDelegator, perms, allUserIdPerms);
            }
        }

        if (Debug.verboseOn()) {
            Debug.logVerbose("Solr: security.json: userLoginId->permissionId map: " + allUserIdPerms, module);
        }
        return allUserIdPerms;
    }

    protected void getPermsForAllUserLoginIdsCore(Delegator delegator, Collection<String> perms, Map<String, Set<String>> allUserIdPerms) {
        for(String perm : perms) {
            try {
                List<GenericValue> securityGroupPermissions = EntityQuery.use(delegator).from("SecurityGroupPermission")
                        .where("permissionId", perm).queryList();
                for(GenericValue securityGroupPermission : securityGroupPermissions) {
                    List<GenericValue> userLoginSecurityGroups = EntityUtil.filterByDate(EntityQuery.use(delegator)
                            .from("UserLoginSecurityGroup").where("groupId", securityGroupPermission.getString("groupId")).queryList());
                    for(GenericValue userLoginSecurityGroup : userLoginSecurityGroups) {
                        String userLoginId = userLoginSecurityGroup.getString("userLoginId");
                        if (delegator.getDelegatorTenantId() != null) {
                            userLoginId += "#" + delegator.getDelegatorTenantId();
                        }
                        Set<String> userIdPerms = allUserIdPerms.get(userLoginId);
                        if (userIdPerms == null) {
                            userIdPerms = new LinkedHashSet<>();
                            allUserIdPerms.put(userLoginId, userIdPerms);
                        }
                        userIdPerms.add(perm);
                    }
                }
            } catch (Exception e) {
                Debug.logError(e, module);
            }
        }
    }

    protected static List<GenericValue> getActiveTenants(Delegator delegator) throws GenericEntityException {
        List<EntityExpr> expr = new ArrayList<EntityExpr>();
        expr.add(EntityCondition.makeCondition("disabled", EntityOperator.EQUALS, "N"));
        expr.add(EntityCondition.makeCondition("disabled", EntityOperator.EQUALS, null));
        return EntityQuery.use(delegator).from("Tenant")
                .where(EntityCondition.makeCondition(expr, EntityOperator.OR)).queryList();
    }
}
