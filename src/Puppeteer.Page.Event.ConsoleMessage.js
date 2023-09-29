import { ConsoleMessage } from 'puppeteer'
import { JSHandle } from 'puppeteer'

/** @type {(m: ConsoleMessage) => Array<JSHandle<unknown>>} */
export const args = m => m.args()

/** @type {(m: ConsoleMessage) => string} */
export const text = m => m.text()

/** @type {(m: ConsoleMessage) => string} */
export const _messageType = m => m.type()

/** @type {(m: ConsoleMessage) => import("puppeteer").ConsoleMessageLocation} */
export const _location = m => m.location()

/** @type {(m: ConsoleMessage) => Array<import("puppeteer").ConsoleMessageLocation>} */
export const _stackTrace = m => m.stackTrace()
