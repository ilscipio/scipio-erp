/**
 * Scipio CMS Content processor placeholder - can be used to put Variable content into
 * the context of the CMS Editor.
 * <p>
 * NOTE: 2017: the CMS-specific processor script is no longer of major importance in Scipio,
 * because Scipio supports custom system-wide and webapp-specific global scripts.
 * It is preferable to reuse those mechanisms, unless the script is truly CMS-specific.
 * <p>
 * WARNING: This script may not be running under any database transaction.
 * If you use this script and must query the entity engine, it is recommended
 * to query with the entity cache enabled only - any non-cached entity lookups
 * may impose an extra performance cost.
 */

