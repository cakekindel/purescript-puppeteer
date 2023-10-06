import { BrowserContext } from 'puppeteer'
import { Browser } from 'puppeteer'

/** @type {(b: Browser) => () => Promise<void>} */
export const _close = b => () => b.close()

/** @type {(b: Browser) => () => void} */
export const disconnect = b => () => b.disconnect()

/** @type {(b: {browser: () => Browser}) => () => Browser} */
export const _get = b => () => b.browser()

/** @type {(_: Browser) => () => string} */
export const websocketEndpoint = b => () => b.wsEndpoint()

/** @type {(_: Browser) => () => boolean} */
export const connected = b => () => b.connected

/** @type {(_: BrowserContext) => () => Browser} */
export const ofContext = c => () => c.browser()
