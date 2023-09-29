import { BrowserContext } from 'puppeteer'
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

/** @type {(_: Browser | BrowserContext) => Promise<Page>} */
export const _newPage = b => b.newPage()

/** @type {(_: Browser | BrowserContext) => Promise<Array<Page>>} */
export const _all = b => b.pages()

/**
 * `foreign import _findAll :: String -> Page -> Promise (Array (Handle HTMLElement))`
 * @type {(_1: string) => (_2: Page) => Promise<Array<ElementHandle<any>>>}
 */
export const _findAll = s => p => p.$$(s)

/**
 * `foreign import _addScriptTag :: Foreign -> Page -> Promise (Handle HTMLScriptElement)`
 * @type {(_1: import("puppeteer").FrameAddScriptTagOptions) => (_2: Page) => Promise<ElementHandle<any>>}
 */
export const _addScriptTag = o => p => p.addScriptTag(o)

/**
 * `foreign import _addStyleTag :: Foreign -> Page -> Promise (Handle Foreign)`
 * @type {(_1: import("puppeteer").FrameAddStyleTagOptions) => (_2: Page) => Promise<ElementHandle<any>>}
 */
export const _addStyleTag = o => p => p.addStyleTag(o)

/**
 * `foreign import _bringToFront :: Page -> Promise Unit`
 * @type {(_1: Page) => Promise<void>}
 */
export const _bringToFront = p => p.bringToFront()

/**
 * `foreign import _close :: forall r. Page -> Promise Unit`
 * @type {(_0: Page) => Promise<void>}
 */
export const _close = p => p.close()

/**
 * `foreign import _isClosed :: forall r. Page -> Effect Boolean`
 * @type {(_0: Page) => () => boolean}
 */
export const isClosed = p => () => p.isClosed()

/**
 * `foreign import _content :: Page -> Promise String`
 * @type {(_0: Page) => Promise<string>}
 */
export const _content = p => p.content()

/**
 * `foreign import _setContent :: String -> Foreign -> Page -> Promise Unit`
 * @type {(_0: string) => (_2: import("puppeteer").PuppeteerLifeCycleEvent) => (_4: Page) => Promise<void>}
 */
export const _setContent = html => ev => p =>
  p.setContent(html, { timeout: 0, waitUntil: ev })

/**
 * `foreign import _setViewport :: Viewport -> Page -> Promise Unit`
 * @type {(_0: import("puppeteer").Viewport) => (_4: Page) => Promise<void>}
 */
export const _setViewport = v => p => p.setViewport(v)

/**
 * `foreign import _title :: Page -> Promise String`
 * @type {(_4: Page) => Promise<string>}
 */
export const _title = p => p.title()

/**
 * `foreign import _url :: Page -> Effect URL`
 * @type {(_4: Page) => () => string}
 */
export const url = p => () => p.url()

/**
 * `foreign import _viewport :: Page -> Nullable Viewport`
 * @type {(_0: Page) => import("puppeteer").Viewport | null}
 */
export const _viewport = p => p.viewport()
