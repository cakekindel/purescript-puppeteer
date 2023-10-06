import { HTTPResponse } from 'puppeteer'
import { HTTPRequest } from 'puppeteer'

/**
 * `foreign import _abort :: String -> Request -> Promise Unit`
 * @type {(_0: import("puppeteer").ErrorCode) => (_1: HTTPRequest) => () => Promise<void>}
 */
export const _abort = e => r => () => r.abort(e)

/**
 * `foreign import _continue :: Foreign -> Request -> Promise Unit`
 * @type {(_0: import("puppeteer").ContinueRequestOverrides) => (_1: HTTPRequest) => () => Promise<void>}
 */
export const _continue = o => r => () => r.continue(o)

/**
 * `foreign import _respond :: RespondToRequest -> Request -> Promise Unit`
 * @type {(_0: import("puppeteer").ResponseForRequest) => (_1: HTTPRequest) => () => Promise<void>}
 */
export const _respond = rep => req => () => req.respond(rep)

/**
 * `foreign import _failure :: Request -> Effect (Nullable {errorText :: String})`
 * @type {(_1: HTTPRequest) => () => {errorText: string} | null}
 */
export const _failure = r => () => r.failure()

/**
 * `foreign import headers :: Request -> Effect (Map String String)`
 * @type {(_1: HTTPRequest) => () => Map<string, string>}
 */
export const headers = r => () =>
  Object.entries(r.headers()).reduce((map, [k, v]) => {
    map.set(k, v)
    return map
  }, new Map())

/**
 * `foreign import _isNavigation :: Request -> Effect Boolean`
 * @type {(_1: HTTPRequest) => () => boolean}
 */
export const isNavigation = r => () => r.isNavigationRequest()

/**
 * `foreign import _method :: Request -> Effect String`
 * @type {(_1: HTTPRequest) => () => string}
 */
export const method = r => () => r.method()

/**
 * `foreign import _postData :: Request -> Effect Foreign`
 * @type {(_1: HTTPRequest) => () => string | undefined}
 */
export const _postData = r => () => r.postData()

/**
 * `foreign import _resourceType :: Request -> Effect String`
 * @type {(_1: HTTPRequest) => () => string}
 */
export const resourceType = r => () => r.resourceType()

/**
 * `foreign import _url :: Request -> Effect String`
 * @type {(_1: HTTPRequest) => () => string}
 */
export const url = r => () => r.url()

/**
 * `foreign import _response :: Request -> Effect (Nullable Response)`
 * @type {(_1: HTTPRequest) => () => HTTPResponse | null}
 */
export const _response = r => () => r.response()
