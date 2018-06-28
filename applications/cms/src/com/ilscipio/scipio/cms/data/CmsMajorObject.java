package com.ilscipio.scipio.cms.data;

/**
 * Implemented by the "major" entities, or in other words those that can
 * correspond to high-level objects or abstractions, like Pages or Templates.
 * <p>
 * NOTE: For technical reasons this also enforces the notion the major entities
 * must have a single simple physical representing entity.
 * They are assumed to extend CmsDataObject.
 * <p>
 * DEV NOTE: All data object classes implementing this should have their entity name
 * added to {@link CmsEntityInfo#majorCmsEntityNames}.
 */
public interface CmsMajorObject extends CmsEntityReadable, CmsEntityVisit.CmsEntityVisitee {
}
