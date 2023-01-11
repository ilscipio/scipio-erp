package com.ilscipio.scipio.web;

import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;

import javax.servlet.http.HttpSession;
import javax.websocket.Session;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Simple permission security checker interface, which implicitly contains the permissions to check.
 * <p>SCIPIO: 3.0.0: Added for annotations support (generic interface).</p>
 */
public interface SocketPermissionVerifier {

    boolean hasPermission(Security security, GenericValue userLogin, HttpSession httpSession, Session socketSession);

    class EntityViewOr implements SocketPermissionVerifier {
        private final List<String> entities;

        public EntityViewOr(String... entities) {
            this.entities = new ArrayList<>(Arrays.asList(entities));
        }

        public List<String> getEntities() {
            return entities;
        }

        @Override
        public boolean hasPermission(Security security, GenericValue userLogin, HttpSession httpSession, Session socketSession) {
            for (String entity : getEntities()) {
                if (security.hasEntityPermission(entity, "_VIEW", userLogin)) {
                    return true;
                }
            }
            return false;
        }
    }

}
