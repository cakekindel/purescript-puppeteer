import { Page } from 'puppeteer'

/**
 * `foreign import _bypassCsp :: Page -> Promise Unit`
 * @type {(_p: Page) => () => Promise<void>}
 */
export const _bypassCsp = p => () => p.setBypassCSP(true)

/**
 * `foreign import _unbypassCsp :: Page -> Promise Unit`
 * @type {(_p: Page) => () => Promise<void>}
 */
export const _unbypassCsp = p => () => p.setBypassCSP(false)

/**
 * `foreign import _enableCache :: Page -> Promise Unit`
 * @type {(_p: Page) => () => Promise<void>}
 */
export const _enableCache = p => () => p.setCacheEnabled(true)

/**
 * `foreign import _disableCache :: Page -> Promise Unit`
 * @type {(_p: Page) => () => Promise<void>}
 */
export const _disableCache = p => () => p.setCacheEnabled(false)

/**
 * `foreign import _interceptRequests :: Page -> Promise Unit`
 * @type {(_p: Page) => () => Promise<void>}
 */
export const _interceptRequests = p => () => p.setRequestInterception(true)

/**
 * `foreign import _uninterceptRequests :: Page -> Promise Unit`
 * @type {(_p: Page) => () => Promise<void>}
 */
export const _uninterceptRequests = p => () => p.setRequestInterception(false)

/**
 * `foreign import _sendExtraHeaders :: Map String String -> Page -> Promise Unit`
 * @type {(_0: Record<string, string>) => (_1: Page) => () => Promise<void>}
 */
export const _sendExtraHeaders = h => p => () => p.setExtraHTTPHeaders(h)
