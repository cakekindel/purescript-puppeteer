import { HTTPRequest } from 'puppeteer'
import { HTTPResponse } from 'puppeteer'

/**
 * `foreign import _request :: Response -> Effect Request`
 * @type {(_0: HTTPResponse) => () => HTTPRequest}
 */
export const request = r => () => r.request()

/**
 * `foreign import _bodyBuffer :: Response -> Promise Buffer`
 * @type {(_0: HTTPResponse) => Promise<Buffer>}
 */
export const _bodyBuffer = r => r.buffer()

/**
 * `foreign import _bodyJson :: Response -> Promise Foreign`
 * @type {(_0: HTTPResponse) => Promise<unknown>}
 */
export const _bodyJson = r => r.json()

/**
 * `foreign import _bodyText :: Response -> Promise String`
 * @type {(_0: HTTPResponse) => Promise<string>}
 */
export const _bodyText = r => r.text()

/**
 * `foreign import _url :: Response -> Effect String`
 * @type {(_0: HTTPResponse) => () => string}
 */
export const url = r => () => r.url()

/**
 * `foreign import _remoteAddressIp :: Response -> Effect Foreign`
 * @type {(_0: HTTPResponse) => () => string | undefined}
 */
export const _remoteAddressIp = r => () => r.remoteAddress().ip

/**
 * `foreign import _remoteAddressPort :: Response -> Effect Foreign`
 * @type {(_0: HTTPResponse) => () => number | undefined}
 */
export const _remoteAddressPort = r => () => r.remoteAddress().port

/**
 * `foreign import _status :: Response -> Effect Int`
 * @type {(_0: HTTPResponse) => () => number}
 */
export const status = r => () => r.status()

/**
 * `foreign import _statusText :: Response -> Effect String`
 * @type {(_0: HTTPResponse) => () => string}
 */
export const statusText = r => () => r.statusText()
