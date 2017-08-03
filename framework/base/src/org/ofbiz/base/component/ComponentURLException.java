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
    
    public ComponentURLException(String msg) {
        super(msg);
    }

    public static class ComponentNotFoundURLException extends ComponentURLException {
        public ComponentNotFoundURLException(String msg) {
            super(msg);
        }
    }
    
}
