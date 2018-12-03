package org.ofbiz.webapp.control;

import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.UtilMisc;

/**
 * SCIPIO: Used to communicate to methods if an attribute should go into request or session attributes,
 * and also reads from request parameters. Also provides some very common attribute/parameter reading operations.
 * <p>
 * Added 2018-12-03.
 */
public interface RequestVarScopes {

    public static final RequestVarScope REQUEST = RequestVarScope.REQUEST;
    public static final RequestVarScope PARAMETER = RequestVarScope.PARAMETER;
    public static final RequestVarScope SESSION = RequestVarScope.SESSION;
    public static final RequestVarScope APPLICATION = RequestVarScope.APPLICATION;

    public static final RequestAndSessionRequestVarScopes REQUEST_AND_SESSION = new RequestAndSessionRequestVarScopes();
    public static final RequestAndParameterRequestVarScopes REQUEST_AND_PARAMETER = new RequestAndParameterRequestVarScopes();

    public static final AllRequestVarScopes ALL = new AllRequestVarScopes();
    public static final NoneRequestVarScopes NONE = new NoneRequestVarScopes();

    public Set<RequestVarScope> scopes();
    public Collection<RequestVarScope> reversedScopes();

    public default boolean request() { return scopes().contains(REQUEST); }
    public default boolean parameter() { return scopes().contains(PARAMETER); }
    public default boolean session() { return scopes().contains(SESSION); }
    public default boolean application() { return scopes().contains(APPLICATION); }

    public default boolean requestOrSession() { return request() || session(); }
    public default boolean requestOrParam() { return request() || parameter(); }

    public default boolean all() { return (scopes().size() == RequestVarScope.ALL.size()); }
    public default boolean any() { return !scopes().isEmpty(); }
    public default boolean none() { return scopes().isEmpty(); }
    public default boolean single() { return (scopes().size() == 1); }
    public default boolean intersects(RequestVarScopes other) { return scopes().retainAll(other.scopes()); }

    public default void setValue(HttpServletRequest request, String name, Object value) {
        for(RequestVarScope scope : scopes()) { scope.setValue(request, name, value); }
    }
    public default void setValue(HttpServletRequest request, RequestVarScopes limitScopes, String name, Object value) {
        for(RequestVarScope scope : scopes()) { scope.setValue(request, limitScopes, name, value); }
    }

    public default void removeValue(HttpServletRequest request, String name) {
        for(RequestVarScope scope : scopes()) { scope.removeValue(request, name); }
    }
    public default void removeValue(HttpServletRequest request, RequestVarScopes limitScopes, String name) {
        for(RequestVarScope scope : scopes()) { scope.removeValue(request, limitScopes, name); }
    }

    /** If value is non-null, sets corresponding attributes; if value is null, removes them. */
    public default void setOrRemoveValue(HttpServletRequest request, String name, Object value) {
        if (value != null) { setValue(request, name, value); }
        removeValue(request, name);
    }
    /** If value is non-null, sets corresponding attributes; if value is null, removes them. */
    public default void setOrRemoveValue(HttpServletRequest request, RequestVarScopes limitScopes, String name, Object value) {
        if (value != null) { setValue(request, limitScopes, name, value); }
        removeValue(request, limitScopes, name);
    }

    /**  Returns the first non-null value (attribute or parameter) in the given scopes, from most specific to least. */
    public default Object getValue(HttpServletRequest request, String name) {
        for(RequestVarScope scope : scopes()) {
            Object value = scope.getValue(request, name);
            if (value != null) { return value; }
        }
        return null;
    }
    /** Returns the first non-null value (attribute or parameter) in the given scopes. */
    public default Object getValue(HttpServletRequest request, String name, boolean mostSpecificFirst) {
        if (mostSpecificFirst) { return getValue(request, name); }
        return getValueLeastSpecific(request, name);
    }

    /** Returns the first non-null value (attribute or parameter) in the given scopes, from least specific to most. */
    public default Object getValueLeastSpecific(HttpServletRequest request, String name) {
        for(RequestVarScope scope : reversedScopes()) {
            Object value = scope.getValueLeastSpecific(request, name);
            if (value != null) { return value; }
        }
        return null;
    }

    public enum RequestVarScope implements RequestVarScopes {
        REQUEST() {
            @Override public boolean request() { return true; }
            @Override public boolean intersects(RequestVarScopes other) { return other.request(); }
            @Override
            public void setValue(HttpServletRequest request, String name, Object value) {
                request.setAttribute(name, value);
            }
            @Override
            public void setValue(HttpServletRequest request, RequestVarScopes limitScopes, String name, Object value) {
                if (limitScopes.request()) { setValue(request, name, value); }
            }
            @Override
            public void removeValue(HttpServletRequest request, String name) {
                request.removeAttribute(name);
            }
            @Override
            public void removeValue(HttpServletRequest request, RequestVarScopes limitScopes, String name) {
                if (limitScopes.request()) {
                    removeValue(request, name);
                }
            }
            @Override
            public Object getValue(HttpServletRequest request, String name) {
                return request.getAttribute(name);
            }
            @Override
            public Object getValueLeastSpecific(HttpServletRequest request, String name) {
                return getValue(request, name);
            }
        },
        PARAMETER() {
            @Override public boolean parameter() { return true; }
            @Override public boolean intersects(RequestVarScopes other) { return other.parameter(); }
            @Override
            public void setValue(HttpServletRequest request, String name, Object value) {
                request.setAttribute(name, value);
            }
            @Override
            public void setValue(HttpServletRequest request, RequestVarScopes limitScopes, String name, Object value) {
                if (limitScopes.parameter()) {
                    setValue(request, name, value);
                }
            }
            @Override
            public void removeValue(HttpServletRequest request, String name) {
                request.removeAttribute(name);
            }
            @Override
            public void removeValue(HttpServletRequest request, RequestVarScopes limitScopes, String name) {
                if (limitScopes.parameter()) {
                    removeValue(request, name);
                }
            }
            @Override
            public Object getValue(HttpServletRequest request, String name) {
                return request.getAttribute(name);
            }
            @Override
            public Object getValueLeastSpecific(HttpServletRequest request, String name) {
                return getValue(request, name);
            }
        },
        SESSION() {
            @Override public boolean session() { return true; }
            @Override public boolean intersects(RequestVarScopes other) { return other.session(); }
            @Override
            public void setValue(HttpServletRequest request, String name, Object value) {
                request.getSession().setAttribute(name, value);
            }
            @Override
            public void setValue(HttpServletRequest request, RequestVarScopes limitScopes, String name, Object value) {
                if (limitScopes.session()) {
                    setValue(request, name, value);
                }
            }
            @Override
            public void removeValue(HttpServletRequest request, String name) {
                HttpSession session = request.getSession(false);
                if (session != null) {
                    session.removeAttribute(name);
                }
            }
            @Override
            public void removeValue(HttpServletRequest request, RequestVarScopes limitScopes, String name) {
                if (limitScopes.session()) {
                    removeValue(request, name);
                }
            }
            @Override
            public Object getValue(HttpServletRequest request, String name) {
                HttpSession session = request.getSession(false);
                return (session != null) ? session.getAttribute(name) : null;
            }
            @Override
            public Object getValueLeastSpecific(HttpServletRequest request, String name) {
                return getValue(request, name);
            }

        },
        APPLICATION() {
            @Override public boolean application() { return true; }
            @Override public boolean intersects(RequestVarScopes other) { return other.application(); }
            @Override
            public void setValue(HttpServletRequest request, String name, Object value) {
                request.getServletContext().setAttribute(name, value);
            }
            @Override
            public void setValue(HttpServletRequest request, RequestVarScopes limitScopes, String name, Object value) {
                if (limitScopes.application()) {
                    setValue(request, name, value);
                }
            }
            @Override
            public void removeValue(HttpServletRequest request, String name) {
                request.getServletContext().removeAttribute(name);
            }
            @Override
            public void removeValue(HttpServletRequest request, RequestVarScopes limitScopes, String name) {
                if (limitScopes.application()) {
                    removeValue(request, name);
                }
            }
            @Override
            public Object getValue(HttpServletRequest request, String name) {
                return request.getServletContext().getAttribute(name);
            }
            @Override
            public Object getValueLeastSpecific(HttpServletRequest request, String name) {
                return getValue(request, name);
            }
        };

        public static final Set<RequestVarScope> ALL = Collections.unmodifiableSet(EnumSet.allOf(RequestVarScope.class));
        public static final Set<RequestVarScope> NONE = Collections.unmodifiableSet(EnumSet.noneOf(RequestVarScope.class));

        private final Set<RequestVarScope> scopes;

        private RequestVarScope() {
            this.scopes = Collections.unmodifiableSet(EnumSet.of(this));
        }

        @Override
        public Set<RequestVarScope> scopes() { return scopes; }
        @Override public Collection<RequestVarScope> reversedScopes() { return scopes; }
        @Override public boolean request() { return false; }
        @Override public boolean parameter() { return false; }
        @Override public boolean session() { return false; }
        @Override public boolean application() { return false; }
    }

    public static class GenericReqVarScopes implements RequestVarScopes {
        protected final Set<RequestVarScope> scopes;
        protected final List<RequestVarScope> reversedScopes;

        public GenericReqVarScopes(Set<RequestVarScope> scopes) {
            this.scopes = (scopes instanceof EnumSet) ? Collections.unmodifiableSet(scopes) : scopes;
            this.reversedScopes = UtilMisc.unmodifiableReversedList(scopes);
        }

        @Override public Set<RequestVarScope> scopes() { return scopes; }
        @Override public Collection<RequestVarScope> reversedScopes() { return reversedScopes; }
    }

    public static class NoneRequestVarScopes extends GenericReqVarScopes {
        protected NoneRequestVarScopes() { super(RequestVarScope.NONE);}

        @Override public boolean request() { return false; }
        @Override public boolean parameter() { return false; }
        @Override public boolean session() { return false; }
        @Override public boolean application() { return false; }
        @Override public boolean intersects(RequestVarScopes other) { return false; }
        @Override public void setValue(HttpServletRequest request, String name, Object value) {}
        @Override public void setValue(HttpServletRequest request, RequestVarScopes limitScopes, String name, Object value) {}
        @Override public void removeValue(HttpServletRequest request, String name) {}
        @Override public void removeValue(HttpServletRequest request, RequestVarScopes limitScopes, String name) {}
        @Override public Object getValue(HttpServletRequest request, String name) { return null; }
        @Override public Object getValueLeastSpecific(HttpServletRequest request, String name) { return null; }
    }

    public static class RequestAndSessionRequestVarScopes extends GenericReqVarScopes {
        protected RequestAndSessionRequestVarScopes() { super(EnumSet.of(REQUEST, SESSION));}

        @Override public boolean request() { return true; }
        @Override public boolean session() { return true; }
        @Override public boolean intersects(RequestVarScopes other) { return other.request() || other.session();  }
        @Override
        public void setValue(HttpServletRequest request, String name, Object value) {
            REQUEST.setValue(request, name, value);
            SESSION.setValue(request, name, value);
        }
        @Override
        public void setValue(HttpServletRequest request, RequestVarScopes limitScopes, String name, Object value) {
            REQUEST.setValue(request, limitScopes, name, value);
            SESSION.setValue(request, limitScopes, name, value);
        }
        @Override
        public void removeValue(HttpServletRequest request, String name) {
            REQUEST.removeValue(request, name);
            SESSION.removeValue(request, name);
        }
        @Override
        public void removeValue(HttpServletRequest request, RequestVarScopes limitScopes, String name) {
            REQUEST.removeValue(request, limitScopes, name);
            SESSION.removeValue(request, limitScopes, name);
        }
        @Override
        public Object getValue(HttpServletRequest request, String name) {
            Object value = REQUEST.getValue(request, name);
            if (value != null) { return value; }
            return SESSION.getValue(request, name);
        }
        @Override
        public Object getValueLeastSpecific(HttpServletRequest request, String name) {
            Object value = SESSION.getValue(request, name);
            if (value != null) { return value; }
            return REQUEST.getValue(request, name);
        }
    }

    public static class RequestAndParameterRequestVarScopes extends GenericReqVarScopes {
        protected RequestAndParameterRequestVarScopes() {super(EnumSet.of(REQUEST, PARAMETER));}

        @Override public boolean request() { return true; }
        @Override public boolean parameter() { return true; }
        @Override public boolean intersects(RequestVarScopes other) { return other.request() || other.parameter(); }
        @Override public void setValue(HttpServletRequest request, String name, Object value) {
            REQUEST.setValue(request, name, value);
        }
        @Override
        public void setValue(HttpServletRequest request, RequestVarScopes limitScopes, String name, Object value) {
            REQUEST.setValue(request, limitScopes, name, value);
        }
        @Override
        public void removeValue(HttpServletRequest request, String name) {
            REQUEST.removeValue(request, name);
        }
        @Override
        public void removeValue(HttpServletRequest request, RequestVarScopes limitScopes, String name) {
            REQUEST.removeValue(request, limitScopes, name);
        }
        @Override
        public Object getValue(HttpServletRequest request, String name) {
            Object value = REQUEST.getValue(request, name);
            if (value != null) { return value; }
            return PARAMETER.getValue(request, name);
        }
        @Override
        public Object getValueLeastSpecific(HttpServletRequest request, String name) {
            Object value = PARAMETER.getValue(request, name);
            if (value != null) { return value; }
            return REQUEST.getValue(request, name);
        }
    }

    public static class AllRequestVarScopes extends GenericReqVarScopes {
        protected AllRequestVarScopes() {super(RequestVarScope.ALL);}

        @Override public boolean request() { return true; }
        @Override public boolean parameter() { return true; }
        @Override public boolean session() { return true; }
        @Override public boolean application() { return true; }
        @Override public boolean all() { return true; }
        @Override public boolean any() { return true; }
        @Override public boolean none() { return false; }
    }
}