import { HTTPResponse } from 'puppeteer'
import { HTTPRequest } from 'puppeteer'

/**
 * @type {(_0: import("puppeteer").ErrorCode) => (_1: HTTPRequest) => () => Promise<void>}
 */
export const _abort = e => r => () => r.abort(e)

/**
 * @type {(_0: import("puppeteer").ContinueRequestOverrides) => (_1: HTTPRequest) => () => Promise<void>}
 */
export const _continue = o => r => () => r.continue(o)

/**
 * @type {(_0: import("puppeteer").ResponseForRequest) => (_1: HTTPRequest) => () => Promise<void>}
 */
export const _respond = rep => req => () => req.respond(rep)

/**
 * @type {(_1: HTTPRequest) => () => {errorText: string} | null}
 */
export const _failure = r => () => r.failure()

/**
 * @type {(_1: HTTPRequest) => () => Array<{k: string, v: string}>}
 */
export const headers = r => () => {
  /** @type {Array<{k: string, v: string}>} */
  const init = []
  return Object.entries(r.headers()).reduce(
    (map, [k, v]) => [...map, { k, v }],
    init,
  )
}

/**
 * @type {(_1: HTTPRequest) => () => boolean}
 */
export const isNavigation = r => () => r.isNavigationRequest()

/**
 * @type {(_1: HTTPRequest) => () => string}
 */
export const method = r => () => r.method()

/**
 * @type {(_1: HTTPRequest) => () => string | undefined}
 */
export const _postData = r => () => r.postData()

/**
 * @type {(_1: HTTPRequest) => () => string}
 */
export const resourceType = r => () => r.resourceType()

/**
 * @type {(_1: HTTPRequest) => () => string}
 */
export const url = r => () => r.url()

/**
 * @type {(_1: HTTPRequest) => () => HTTPResponse | null}
 */
export const _response = r => () => r.response()
