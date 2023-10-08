import { BrowserContext, CDPSession } from 'puppeteer'
import { Keyboard } from 'puppeteer'
import { Mouse } from 'puppeteer'
import { Touchscreen } from 'puppeteer'
import { ElementHandle } from 'puppeteer'
import { Browser } from 'puppeteer'
import { Page } from 'puppeteer'

/** @type {(_: Page) => () => Keyboard} */
export const keyboard = p => () => p.keyboard

/** @type {(_: Page) => () => Mouse} */
export const mouse = p => () => p.mouse

/** @type {(_: Page) => () => Touchscreen} */
export const touchscreen = p => () => p.touchscreen

/** @type {(_: Browser | BrowserContext) => () => Promise<Page>} */
export const _newPage = b => () => b.newPage()

/** @type {(_: Page) => () => Promise<CDPSession>} */
export const _createCDPSession = p => () => p.createCDPSession()

/** @type {(_: Browser | BrowserContext) => () => Promise<Array<Page>>} */
export const _all = b => () => b.pages()

/** @type {(_: {username: string, password: string}) => (_: Page) => () => Promise<void>} */
export const _authenticate = creds => p => () => p.authenticate(creds)

/**
 * @type {(_1: string) => (_2: Page) => () => Promise<Array<ElementHandle<any>>>}
 */
export const _findAll = s => p => () => p.$$(s)

/**
 * @type {(_1: import("puppeteer").FrameAddScriptTagOptions) => (_2: Page) => () => Promise<ElementHandle<any>>}
 */
export const _addScriptTag = o => p => () => p.addScriptTag(o)

/**
 * @type {(_1: import("puppeteer").FrameAddStyleTagOptions) => (_2: Page) => () => Promise<ElementHandle<any>>}
 */
export const _addStyleTag = o => p => () => p.addStyleTag(o)

/**
 * @type {(_1: Page) => () => Promise<void>}
 */
export const _bringToFront = p => () => p.bringToFront()

/**
 * @type {(_0: Page) => () => Promise<void>}
 */
export const _close = p => () => p.close()

/**
 * @type {(_0: Page) => () => boolean}
 */
export const isClosed = p => () => p.isClosed()

/**
 * @type {(_0: Page) => () => Promise<string>}
 */
export const _content = p => () => p.content()

/**
 * @type {(_0: string) => (_2: import("puppeteer").PuppeteerLifeCycleEvent) => (_4: Page) => () => Promise<void>}
 */
export const _setContent = html => ev => p => () =>
  p.setContent(html, { timeout: 0, waitUntil: ev })

/**
 * @type {(_0: import("puppeteer").Viewport) => (_4: Page) => () => Promise<void>}
 */
export const _setViewport = v => p => () => p.setViewport(v)

/**
 * @type {(_4: Page) => () => Promise<string>}
 */
export const _title = p => () => p.title()

/**
 * @type {(_4: Page) => () => string}
 */
export const url = p => () => p.url()

/**
 * @type {(_0: Page) => import("puppeteer").Viewport | null}
 */
export const _viewport = p => p.viewport()
