import { Page } from 'puppeteer'

/**
 * `foreign import _list :: Array String -> Page -> Promise (Array CookieRaw)`
 * @type {(_0: Array<string>) => (_1: Page) => () => Promise<Array<import('devtools-protocol').Protocol.Network.Cookie>>}
 */
export const _list = cs => page => () => page.cookies(...cs)

/**
 * `foreign import _delete :: Foreign -> Page -> Promise Unit`
 * @type {(_0: import('devtools-protocol').Protocol.Network.DeleteCookiesRequest) => (_1: Page) => () => Promise<void>}
 */
export const _delete = dc => p => () => p.deleteCookie(dc)

/**
 * `foreign import _set :: Foreign -> Page -> Promise Unit`
 * @type {(_0: import('devtools-protocol').Protocol.Network.CookieParam) => (_1: Page) => () => Promise<void>}
 */
export const _set = c => p => () => p.setCookie(c)
