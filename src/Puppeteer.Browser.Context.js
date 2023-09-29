import { Page } from 'puppeteer'
import { Browser, BrowserContext } from 'puppeteer'

/** @type {(b: Browser) => Array<BrowserContext>} */
export const all = b => b.browserContexts()

/** @type {(b: BrowserContext) => boolean} */
export const isIncognito = c => c.isIncognito()

/** @type {(p: Page) => BrowserContext} */
export const forPage = p => p.browserContext()

/** @type {(b: Browser) => BrowserContext} */
export const _default = b => b.defaultBrowserContext()

/** @type {(o: object) => (b: Browser) => Promise<BrowserContext>} */
export const _incognito = o => b => b.createIncognitoBrowserContext(o)

/** @type {(origin: string) => (p: Array<import('puppeteer').Permission>) => (b: BrowserContext) => Promise<void>} */
export const _overridePermissions = o => p => c => c.overridePermissions(o, p)

/** @type {(b: BrowserContext) => Promise<void>} */
export const _clearPermissionOverrides = c => c.clearPermissionOverrides()

/** @type {(b: BrowserContext) => Promise<void>} */
export const _close = c => c.close()
