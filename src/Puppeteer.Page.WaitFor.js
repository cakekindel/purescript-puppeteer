import { ElementHandle } from 'puppeteer'
import { Page } from 'puppeteer'

/**
 * `foreign import _navigation :: Number -> Foreign -> Page -> Effect (Promise Unit)`
 * @type {(_1: import("puppeteer").PuppeteerLifeCycleEvent) => (_2: Page) => () => Promise<void>}
 */
export const _navigation = ev => p => () =>
  p.waitForNavigation({ timeout: 0, waitUntil: ev }).then(() => {})

/**
 * `foreign import _networkIdle :: Number -> Foreign -> Page -> Promise Unit`
 * @type {(_1: number) => (_2: Page) => () => Promise<void>}
 */
export const _networkIdle = idleTime => p => () =>
  p.waitForNetworkIdle({ idleTime, timeout: 0 })

/**
 * `foreign import _selector :: forall a. String -> Number -> Page -> Promise (Handle a)`
 * @type {(_0: string) => (_2: Page) => () => Promise<ElementHandle<Element> | null>}
 */
export const _selectorToExist = sel => p => () =>
  p.waitForSelector(sel, { timeout: 0 })

/**
 * `foreign import _selector :: forall a. String -> Number -> Page -> Promise (Handle a)`
 * @type {(_0: string) => (_2: Page) => () => Promise<ElementHandle<Element> | null>}
 */
export const _selector = sel => p => () =>
  p.waitForSelector(sel, { visible: true, timeout: 0 })

/**
 * `foreign import _selector :: forall a. String -> Number -> Page -> Promise (Handle a)`
 * @type {(_0: string) => (_2: Page) => () => Promise<void>}
 */
export const _selectorToBeHidden = sel => p => () =>
  p.waitForSelector(sel, { hidden: true, timeout: 0 }).then(() => {})
