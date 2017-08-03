package org.ofbiz.base.component;

import java.net.MalformedURLException;

/**
 * SCIPIO: Component exception that extends MalformedURLException,
 * that can cross legacy interfaces that support only MalformedURLException.
 * NOTE: Does NOT extend ComponentException!
 * Added 2017-08-03.
 * FIXME: componentCause is not currently part of the stack trace.
 */
@SuppressWarnings("serial")
public class ComponentURLException extends MalformedURLException {
    
    private final Throwable componentErrorCause;
    
    public ComponentURLException(String msg) {
        super(msg);
        this.componentErrorCause = null;
    }

    /**
     * Constructor with extra component error cause.
     * WARN: componentErrorCause does NOT initialize Throwable's cause; it is separate,
     * due to MalformedURLException limitations.
     */
    public ComponentURLException(String msg, Throwable componentErrorCause) {
        super(msg);
        this.componentErrorCause = componentErrorCause;
    }
    
    /**
     * Constructor with extra component error cause.
     * WARN: componentErrorCause does NOT initialize Throwable's cause; it is separate,
     * due to MalformedURLException limitations.
     */
    public ComponentURLException(Throwable componentErrorCause) {
        super(componentErrorCause.getMessage());
        this.componentErrorCause = componentErrorCause;
    }

    /**
     * Returns a cause of this ComponentURLException
     * WARN: this is not the same as {@link #getCause()}, but can be used
     * for checking cause exception type roughly the same way, when
     * the ComponentURLException subclasses do not suffice.
     */
    public Throwable getComponentErrorCause() {
        return componentErrorCause;
    }

    public static class ComponentNotFoundURLException extends ComponentURLException {
        public ComponentNotFoundURLException(String msg, Throwable componentErrorCause) {
            super(msg, componentErrorCause);
        }
        public ComponentNotFoundURLException(String msg) {
            super(msg);
        }
        protected ComponentNotFoundURLException(Throwable componentErrorCause) {
            super(componentErrorCause);
        }
    }
    
}
