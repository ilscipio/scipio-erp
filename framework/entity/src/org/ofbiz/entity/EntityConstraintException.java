package org.ofbiz.entity;

/**
 * Thrown upon a database entity constraint violation.
 * <p>SCIPIO: 2.1.0: Added toward specific exception support.</p>
 */
@SuppressWarnings("serial")
public class EntityConstraintException extends GenericDataSourceException {
    public EntityConstraintException(String message, Throwable cause) {
        super(message, cause);
    }
}
