package org.ofbiz.base.component;

import java.net.MalformedURLException;

/**
 * SCIPIO: Component exception that extends MalformedURLException,
 * that can cross legacy interfaces that support only MalformedURLException.
 * NOTE: Does NOT extend ComponentException!
 * Added 2017-08-03.
 */
@SuppressWarnings("serial")
public class ComponentURLException extends MalformedURLException {
    
    private final Throwable cause;
   
    /**
     * Constructor with extra component error cause.
     * DEV NOTE: this does does not initialize Throwable's cause member; it returns through {@link #getCause()} only;
     * this is due to MalformedURLException limitation.
     */
    public ComponentURLException(String msg, Throwable cause) {
        super(msg);
        this.cause = cause;
    }
    
    public ComponentURLException(String msg) {
        super(msg);
        this.cause = null;
    }

    /**
     * Constructor with extra component error cause.
     * DEV NOTE: this does does not initialize Throwable's cause member; it returns through {@link #getCause()} only;
     * this is due to MalformedURLException limitation.
     */
    public ComponentURLException(Throwable cause) {
        super(cause.getMessage());
        this.cause = cause;
    }

    /**
     * Factory method that wraps a ComponentException with optional message.
     * @param msg optional message or null
     * @param e component exception (required)
     */
    public static ComponentURLException fromComponentException(String msg, ComponentException e) {
        if (e instanceof ComponentException.ComponentNotFoundException) {
            return msg != null ? new ComponentNotFoundURLException(msg, e) : new ComponentNotFoundURLException(e);
        } else {
            return msg != null ? new ComponentURLException(msg, e) : new ComponentURLException(e);
        }
    }
    
    /**
     * Returns a cause of this ComponentURLException
     * DEV NOTE: this returns the local cause member, NOT the Throwable one.
     */
    public Throwable getCause() {
        return cause;
    }

    /**
     * Helper exception that used to wrap a ComponentNotFoundException, for caller convenience.
     * NOTE: not all ComponentException subclasses need or will have a corresponding ComponentURLException
     * sub-class.
     */
    public static class ComponentNotFoundURLException extends ComponentURLException {
        public ComponentNotFoundURLException(String msg, Throwable cause) {
            super(msg, cause);
        }
        public ComponentNotFoundURLException(String msg) {
            super(msg);
        }
        protected ComponentNotFoundURLException(Throwable cause) {
            super(cause);
        }
    }
    
}
