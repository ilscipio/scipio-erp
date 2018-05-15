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
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ScipioRuleBasedAuthorizationPlugin extends RuleBasedAuthorizationPlugin {
    private static final Logger log = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

    private static final String module = ScipioRuleBasedAuthorizationPlugin.class.getName();
    
    public ScipioRuleBasedAuthorizationPlugin() {
    }

    @Override
    public void init(Map<String, Object> initInfo) {
        Map<String, Object> scipioPermSolrRoles = (Map<String, Object>) initInfo.get("scipioPermSolrRoles");
        if (scipioPermSolrRoles != null && scipioPermSolrRoles.size() > 0) {
            Map<String, Set<String>> allUserIdPerms = getPermsForUserLoginIds(scipioPermSolrRoles.keySet());
            if (!allUserIdPerms.isEmpty()) {
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
                
                log.info("Solr: security.json: Generated new user-roles in security.json from SecurityPermissions: {}", userRoles);
                initInfo = newInitInfo;
            }
        }

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

    protected Map<String, Set<String>> getPermsForUserLoginIds(Collection<String> perms) {
        ClassLoader solrClassLoader = Thread.currentThread().getContextClassLoader();
        ClassLoader ofbizClassLoader = ScipioUserLoginAuthPlugin.findParentNonWebappClassLoader(solrClassLoader);
        Thread.currentThread().setContextClassLoader(ofbizClassLoader);
        try {
            return getPermsForUserLoginIdsCore(perms);
        } finally {
            Thread.currentThread().setContextClassLoader(solrClassLoader);
        }
    }
    
    protected Map<String, Set<String>> getPermsForUserLoginIdsCore(Collection<String> perms) {
        Delegator delegator = DelegatorFactory.getDelegator("default");
        Map<String, Set<String>> allUserIdPerms = new HashMap<>();
       
        for(String perm : perms) {
            try {
                List<GenericValue> securityGroupPermissions = EntityQuery.use(delegator).from("SecurityGroupPermission")
                        .where("permissionId", perm).queryList();
                for(GenericValue securityGroupPermission : securityGroupPermissions) {
                    List<GenericValue> userLoginSecurityGroups = EntityUtil.filterByDate(EntityQuery.use(delegator)
                            .from("UserLoginSecurityGroup").where("groupId", securityGroupPermission.getString("groupId")).queryList());
                    for(GenericValue userLoginSecurityGroup : userLoginSecurityGroups) {
                        String userLoginId = userLoginSecurityGroup.getString("userLoginId");
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
        
        if (Debug.verboseOn()) {
            Debug.logVerbose("Solr: security.json: userLoginId->permissionId map: " + allUserIdPerms, module);
        }
        return allUserIdPerms;
    }
    
}
